#####################
## Seminar 2       ##
## Michal Kubista  ##
## 17 January 2022 ##
#####################

install_and_load = function(name, char = T){
    if (!require(name, character.only = char)) {
        install.packages(name)
    }
    require(name, character.only = char)
}

sapply(
    c("data.table","tidyverse","magrittr",
      "arules","arulesViz","readxl"),
    install_and_load
)

rm(install_and_load)

if (!dir.exists("w2/data")) {
    dir.create("w2/data", recursive = T)
}

#-- PART 1 - NAIVE BAYES #######################################################

#--- 1.1 ETL -------------------------------------------------------------------
# download data from 
# https://drive.google.com/drive/folders/1ynL4HLsulRwqbv_L5djmmsRvuuNJKUbn?usp=sharing
# into w2/data

prodTab = read_csv2("w2/data/prod_structure.csv")

## The first data overview
str(prodTab)
# everything looks fine, 2 character columns as expected.
# since the size is manageable, we can view the whole table.
View(prodTab)
# no surprises

## Check the proportion of the categories and products
table(prodTab$category_name)
map(prodTab, ~length(unique(.)))
# there is a strong class inbalance, especially with
# Mineral waters = 315 products AND
# Juice =          446 products VERSUS
# Tonic =            9 products.
# overall, we have 8 categories and 843 products.
# since there are 1039 rows, there will be some duplicites
# since this is only a slice of the original data
# we will not manage the duplicities to keep the the original
# proportions
prodTab %>% 
    pull(product_name) %>% 
    duplicated() %>% 
    prodTab[.,] %>% 
    unique()

# Split the product names
# to increase the number of variables to better feed our Bayes
# classifier, we will split the product_names into three different
# description columns
prodTab =
    prodTab %>% 
    group_by(product_name, category_name) %>% 
    summarise(prodlist = strsplit(product_name, " ")) %>% 
    unnest_wider(prodlist) %>% 
    rename("desc1" = "...1", "desc2" = "...2", "desc3" = "...3")


## all factors!
# change the column clases into factors
# as Bayes is not able to work with text data
prodTab = map_df(prodTab, as.factor)

# let's check the unique values in all columns
# we have most of the unique descriptions in the second and third column
map(prodTab, ~length(unique(.)))

## train & test division
# because of the random splitting, we are setting the seed to ensure
# the reproducibilty, we split the data in half into the train and
# test data
set.seed(123)
nrow(prodTab) %>% {sample(.,. * 0.50)} -> index
train = prodTab[index,]
test = prodTab[-index,]
rm(index)

#--- 1.2 LABELLING -------------------------------------------------------------
if (!require("e1071")) {
    install.packages("e1071")
    require(e1071)
}

## training
bayes = naiveBayes(category_name ~ ., train, laplace = 1)

# in-sample accuracy?
train %>% 
    mutate(lab = predict(bayes, train)) %>% 
    summarise(acc = sum(lab == category_name)/n())

## prediction
test$lab = predict(bayes, test)

## changin the columns
# changing the column order for the purpose of interpretation and
# model performance assessment, putting the original and predicted
# labels next to each other
# the accuracy looks very well on the first peak
test = test[,c(1,6,2:5)]
View(test)
test$ok = test$category_name == test$lab

## accuracy statistics
# defining the accuracy statistics:
# overall accuracy = the overall accuracy is very good, 93,1 %
# confusion table = we see no large problems in the model
#                   there are just two labels that are systematically
#                   mispredicted (a very small ones); the categories with
#                   higher amount of observations have higher number of bad predictions,
#                   but there is no surprise in that either 
sum(test$ok)/nrow(test)

table(test$category_name, test$lab) %>% 
    print() %>% 
    as.data.frame() %>% 
    group_by(Var1) %>% 
    summarise(ok = sum((Var1 == Var2)*Freq),
              count = sum(Freq)) %>% 
    mutate(acc = ok/count * 100)

rm(bayes, prodTab, test, train)

#-- PART 2 - APRIORI ###########################################################

#--- 2.1 STRING INPUT ----------------------------------------------------------
##--- 2.1.1 ETL ----------------------------------------------------------------
download.file("http://fimi.ua.ac.be/data/retail.dat.gz", "w2/data/retail.dat.gz")

transRaw = read_csv("w2/data/retail.dat.gz")
colnames(transRaw) = "items"
transRaw %>% head(500) %>% View()

## find unique items
items = 
    strsplit(transRaw$items, split = " ") %>% 
    unlist()
itemsUn = unique(items)

## this will not work :/
# mat = matrix(0, nrow(transRaw), length(itemsUn))

## item frequencies
itemsFreq = 
    table(items) %>% 
    as_tibble() %>% 
    arrange(desc(n))

summary(itemsFreq$n)
sum(itemsFreq$n > 100)

itemsCh = 
    itemsFreq %>% 
    filter(n > 100) %>%
    pull(items)

##____Since we will limit the support of the rules later in the training phase,
##____we can already omit some items. By omitting I mean excluding them
##____as variables, not removing them from transactions (or even removing the
##____transactions). 
rm(items, itemsUn, itemsFreq)

inject = function(raw){
    raw %>%
        strsplit(split = " ") %>%
        unlist() -> nonList
    
    index = itemsCh %in% nonList %>% which()
    out = rep(0, length(itemsCh))
    out[index] = 1
    return(out)
}
#

# WHAT NOT TO DO! ----
# transMat2 = matrix(0,nrow(transRaw), length(itemsCh))
# colnames(transMat2) = itemsCh
# timeFor = system.time({
#       for(i in seq_along(transRaw$items)){
#             transRaw$items[1] %>%
#                   strsplit(split = " ") %>%
#                   unlist() -> non_list
#             index = itemsCh %in% non_list %>% which()
#             transMat2[i,index] = 1
#
#             print(paste(round(i/nrow(transRaw)*100,3),"%"))
#             flush.console()
#       }
# })
# rm(transMat2, i , index, non_list, timeFor, timeApply)
# ----
system.time({
    transMat = t(sapply(transRaw$items, inject))
})

colnames(transMat) = itemsCh
rownames(transMat) = 1:nrow(transMat)

## are the dimensions ok?
dim(transMat) == c(nrow(transRaw), length(itemsCh))

rm(transRaw, itemsCh, inject)

##--- 2.1.2 ASSOCIATIONS -------------------------------------------------------
model = apriori(transMat, parameter = list(support = 0.01, confidence = 0.5))

ruleTab = 
    inspect(model) %>%
    as.data.frame()

plot(model)
plot(model, engine = 'interactive')

rm(model, ruleTab, transMat)

#--- 2.2 DATAFRAME INPUT -------------------------------------------------------
##--- 2.2.1 ETL ----------------------------------------------------------------
url = "https://archive.ics.uci.edu/ml/machine-learning-databases/00352/Online%20Retail.xlsx"

download.file(url, "w2/data/online_retail.xlsx", mode = 'wb' )

# because of the problems with excel reading, after downloading the file
# from Gdrive, run following
# !!!!! if you read the csv start from line 262  !!!!
# transRaw = fread("w2/data/online.csv")

transRaw = read_xlsx("w2/data/online_retail.xlsx")
str(transRaw)

transRaw =
    transRaw %>% 
    select(InvoiceNo, Description)

## create a product table
prodTable = 
    transRaw %>% 
    summarise(Description = unique(Description)) %>% 
    mutate(prodID = seq_len(n()))

## create a transaction table
transTable = 
    transRaw %>% 
    summarise(InvoiceNo = unique(InvoiceNo)) %>% 
    mutate(transID = seq_len(n()))

## bind to the original table
transRaw = 
    transRaw %>% 
    right_join(prodTable, by = "Description") %>% 
    right_join(transTable, by = "InvoiceNo")

## IDs as numeric
transRaw[,3:4] = apply(transRaw[,3:4], 2, as.numeric)

## create sparse matrix based on IDs
transMat = sparseMatrix(j = transRaw$transID,
                        i = transRaw$prodID)


rownames(transMat) = prodTable$Description
colnames(transMat) = transTable$InvoiceNo

##--- 2.2.2 ASSOCIATIONS -------------------------------------------------------
model = apriori(transMat, parameter = list(support = 0.02, confidence = 0.25))

inspect(model)

## extract data manually
rules = cbind(labels = labels(model), model@quality)
# "@" because S4 happened

rules = 
  rules %>% 
  mutate(
    lhs = gsub("=>.*","", labels),
    rhs = gsub(".*=>","", labels)
    ) %>% 
  select(lhs, rhs, support, confidence, lift, count)

View(rules)
