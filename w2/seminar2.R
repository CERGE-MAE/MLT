#####################
## Seminar 2       ##
## Michal Kubista  ##
## 13 January 2021 ##
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

prodTab = fread("w2/data/prod_structure.csv")

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

prodTab$product_name %>% 
  duplicated() %>% 
  prodTab[.,] %>% 
  unique()

# Define the injection function
    # to increase the number of variables to better feed our Bayes
    # classifier, we will split the product_names into three different
    # description columns

inject = function(x) {
      x %>%
        strsplit(split = " ") %>% 
        unlist() %>% 
        as.list()
}

## splitting the product names
    # now let's apply the user-defined function over the rows of the table,
    # creating a new table and add the column names

prodTab[,
        c("desc1", "desc2", "desc3") := inject(product_name),
        by = product_name
        ]
prodTab

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
rm(index, inject)

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
    summarise(acc = sum(lab == category_name))

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
sum(test$ok)/nrow(test) * 100

table(test$category_name, test$lab) %>% 
    # print() %>% 
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

transRaw = read.delim("w2/data/retail.dat.gz",
                stringsAsFactors = FALSE)

colnames(transRaw) = "items"

transRaw %>% head(500) %>% View()

## find unique items
strsplit(transRaw$items, split = " ") %>% 
      unlist() -> items
itemsUn = unique(items)

## this will not work :/
mat = matrix(0, nrow(transRaw), length(itemsUn))

## item frequencies
table(items) %>% 
      as.data.frame() %>% 
      arrange(desc(Freq)) -> itemsFreq

summary(itemsFreq$Freq)

sum(itemsFreq$Freq > 100)

itemsFreq %>% 
      filter(Freq > 100) %>%
      .$items -> itemsCh

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

inspect(model) %>%
      as.data.frame() -> ruleTab

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

transRaw %<>% 
      select(InvoiceNo, Description)

## create a product table
prodUn = transRaw$Description %>% unique()

prodTable = cbind(Description = prodUn,
                   prodID = seq_along(prodUn)) %>%
      as.data.frame(stringsAsFactors = F)
rm(prodUn)

## create a transaction table 
transUn = transRaw$InvoiceNo %>% unique()

transTable =  cbind(InvoiceNo = transUn,
                     transID = seq_along(transUn)) %>%
      as.data.frame(stringsAsFactors = F)
rm(transUn)

## bind to the original table
transRaw %<>% 
      right_join(prodTable, by = "Description") %>% 
      right_join(transTable, by = "InvoiceNo")

## IDs as numeric
transRaw[,3:4] = apply(transRaw[,3:4],2,as.numeric)

## create sparse matrix based on IDs

transMat = sparseMatrix(j = transRaw$transID,
                         i = transRaw$prodID)


rownames(transMat) = prodTable$Description
colnames(transMat) = transTable$InvoiceNo

##--- 2.2.2 ASSOCIATIONS -------------------------------------------------------
model = apriori(transMat, parameter = list(support = 0.02, confidence = 0.25))

model %>% inspect() %>% as.data.frame()

## extract data manually
rules = cbind(labels = labels(model), model@quality)
    # "@" because S4 happened

rules$lhs = gsub("=>.*","", rules$labels)
rules$rhs = gsub(".*=>","", rules$labels)

rules = rules[,c("lhs","rhs","support","confidence","lift", "count")]
View(rules)
