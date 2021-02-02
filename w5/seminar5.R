#####################
## Seminar 5       ##
## Michal Kubista  ##
## 5 February 2021 ##
#####################

# install.packages("rpart")
# install.packages("rattle")

sapply(
    c("data.table","dplyr","magrittr","ggplot2",
      "purrr", "GGally", "readxl", "rpart", "rattle"),
    require, character.only = T
)

#-- PART 1 - BASKET ANALYSIS BASICS ############################################

#--- 1.1 ETL -------------------------------------------------------------------
if (!dir.exists("w5/data")) {
dir.create("w5/data")
}
# data from https://community.tableau.com/docs/DOC-1236
# doesn't work anymore :(
# url = "https://community.tableau.com/servlet/JiveServlet/downloadBody/1236-102-2-15278/Sample%20-%20Superstore.xls"
# download.file(url, "w5/data/transac.xls", mode = "wb")
# rm(url)
# download from https://drive.google.com/drive/folders/1M1nsc282Eoi_ax_O6piuDrWjYcxwAZWl?usp=sharing

raw = read_excel("w5/data/transac.xls") %>% as.data.table()
## ID coercing

str(raw)
summary(raw)

map_dbl(raw, ~length(unique(.)))

#---- 1.1.1 TABLES SPLITTING ---------------------------------------------------
## customer table
cust = raw[,.(cust_name = unique(`Customer Name`),
               segment = unique(Segment)
               ),
            by = "Customer ID"]
setnames(cust, "Customer ID", "cust_ID")

## products table
prod = raw[,.(cat = unique(Category),
               subcat = unique(`Sub-Category`),
               desc = unique(`Product Name`)),
            by = "Product ID"]
setnames(prod, "Product ID", "prod_ID")

## locations table
loc = raw[,.(city = unique(City),
              state = unique(State),
              region = unique(Region)),
           by = "Postal Code"]

setnames(loc, "Postal Code", "zip")

## ZIP duplicates handling
loc$zip %>% unique() %>% length()
nrow(loc)

loc[duplicated(loc$zip),zip] %>% 
    {filter(.data = loc, zip == .)}

loc[zip == "92024" & city == "Encinitas", zip := 92028]

## transactions table
pur = raw[,.(pur_ID = unique(`Order ID`),
              date = unique(`Order Date`),
              trans_mode = unique(`Ship Mode`),
              cust_ID = unique(`Customer ID`),
              prod_ID = unique(`Product ID`),
              sales = Sales,
              items = Quantity,
              price = Sales/Quantity,
              promo = Discount),
           by = "Row ID"]

## WHAT IS THAT SPLITTING GOOD FOR?
### relational databases logic...
object.size(raw) %>% format(units = "Mb")

(object.size(cust) + object.size(loc) + object.size(prod) +
    object.size(pur)) %>% format(units = "Mb")

### more realistic proportions
(10 * object.size(raw)) %>% format(units = "Mb")

(object.size(cust) + object.size(loc) + object.size(prod) +
        10 * object.size(pur)) %>% format(units = "Mb")

rm(raw)

#--- 1.2 EDA -------------------------------------------------------------------
pur[,
    promoC := ifelse(promo == 0, 0, 1)
    ][,
      .(cust_ID = unique(cust_ID),
       date = unique(date),
       sales = sum(sales),
       items = sum(items),
       price = mean(sales/items),
       promoC = sum(promoC),
       noSKU = .N),
    by = "pur_ID"
    ][,
      promoC := promoC/noSKU
      ] -> bas

## sales histogram
ggplot(bas, aes(x = sales)) +
    geom_histogram(binwidth = 1000)

ggplot(bas[sales < 1000], aes(x = sales)) +
    geom_histogram(binwidth = 100)

ggplot(bas[sales < 1000], aes(x = 1, y = sales)) +
    geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))

## the lower the prices, the more the items?
ggplot(bas, aes(x = items, y = price)) +
    geom_point() +
    geom_smooth(method = "loess")

## basket promo share
ggplot(bas[order(bas$promoC),], aes(x = 1:nrow(bas), y = promoC)) +
    geom_line()

### analyse the periodicity on nopromo baskets ####
np_bas =  bas[promoC == 0]                        #
np_bas$cust_ID %>% unique %>% length              #
                                                  #
np_bas$cust_ID %>%                                #
    table %>%                                     #
    as.data.frame() %>%                           #
    {.[order(.$Freq, decreasing = T),]} %>%       #
    View()                                        #
rm(np_bas)                                        #
### nothing to analyse :D #########################

## basket unique SKU
ggplot(bas[order(bas$noSKU),], aes(x = 1:nrow(bas), y = noSKU)) +
    geom_line()

## relation between unique SKU and number of items bought
ggplot(bas, aes(x = noSKU, y = items)) +
    geom_point() +
    geom_smooth(method = "lm")
    ## BAD IDEA!

ggplot(bas, aes(x = as.factor(noSKU), y = items)) +
    geom_boxplot()

ggplot(bas, aes(x = noSKU, y = items)) +
    geom_jitter(width = 0.5, height = 0.6) +
    geom_smooth(method = "lm")


## daily visits
bas[,
    day := format(date,"%u")
    ][,
      promoG := ifelse(promoC > 0.5, 1, 0)
    ][,
      .(visits = .N),
      by = .(day, promoG)
    ] -> daily

ggplot(daily, aes(x = day, y = visits, col = as.factor(promoG),
                  group = as.factor(promoG))) +
    geom_line() +
    geom_point()

#-- PART 2 - SEGMENTATIONS #####################################################

#--- 2.1 CUSTOMERS -------------------------------------------------------------
## feature engineering
pur[,
    .(unSKU = length(unique(prod_ID)),
      itemsSKU = mean(items),
      priceSKU = mean(price),
      promoSKU = mean(promo)),
    by = "cust_ID"] -> custStats

bas[,
    .(basSKU = mean(noSKU),
      valueBas = mean(sales),
      itemsBas = mean(items),
      priceBas = mean(price)),
    by = "cust_ID"] -> basStats

custStats[basStats, on = "cust_ID"
          ][cust[,.(cust_ID, segment)],, on = "cust_ID"] -> custStats
rm(basStats)

## dummies
custStats$segment %>% unique

custStats %<>% 
    mutate(Corp = ifelse(segment == "Corporate", 1, 0)) %>% 
    mutate(HO = ifelse(segment == "Home Office", 1, 0)) %>% 
    select(-segment)

## into matrix and scale
custStats %>% summary()
    
custStatsS = custStats[,-1]
custStatsS = apply(custStatsS, 2, scale)
rownames(custStatsS) = custStats$cust_ID

## kmeans
set.seed(123)

ss = rep(NA, 20)
for (i in 1:20) {
    ss[i] = kmeans(custStatsS, i)$tot.withinss
}
plot(1:20, ss, type = "b")
k = 3

custStats$group = kmeans(custStatsS, k)$cluster
cluster::clusplot(custStatsS, custStats$group, color = TRUE, shade = T, labels = 1)
table(custStats$group) 

tree = rpart(as.factor(group)~., custStats[,-1])
fancyRpartPlot(tree)

#--- 2.1 PRODUCTS --------------------------------------------------------------
## feature engineering
pur[,
    .(custPen = length(unique(cust_ID))/nrow(cust),
      basPen = length(unique(pur_ID))/nrow(bas),
      items = mean(items),
      price = mean(price),
      promo = mean(promo)),
    by = "prod_ID"] -> prodStats

## dummies
prod$cat %>% table
prodStats[prod[,.(prod_ID, cat)], on = "prod_ID"
          ][,furn := ifelse(cat == "Furniture", 1, 0)
            ][,tech := ifelse(cat == "Technology", 1, 0)
              ][!duplicated(prod_ID), !"cat"] -> prodStats

## matrix and scale
rownames(prodStats) = prodStats$prod_ID
prodStatsS = apply(prodStats[,-1], 2, scale)
rownames(prodStatsS) = prodStats$prod_ID

cluster = hclust(dist(prodStatsS), method = "average")
# plot(cluster)

set.seed(123)
res = rep(NA, 20)
for (i in 1:20) {
    res[i] = kmeans(prodStatsS, i)$tot.withinss
}
plot(1:20, res, type = "b")
k = 4

prodStats$group = kmeans(prodStatsS, k)$cluster

tree = rpart(as.factor(group) ~ ., prodStats[,-1])
fancyRpartPlot(tree)

## pca
pca = prcomp(prodStatsS)
biplot(pca)

summary(pca)

ss = rep(NA, 20)
for (i in 1:20) {
    ss[i] = kmeans(pca$x[,1:3], i)$tot.withinss
}
plot(1:20, ss, type = "b")
k = 3

prodStats$group = kmeans(pca$x[,1:3], k)$cluster
cluster::clusplot(prodStatsS, prodStats$group, color = TRUE, shade = T, labels = 1)

table(prodStats$group)

tree = rpart(as.factor(group)~., prodStats[,-1])
fancyRpartPlot(tree)
tree$variable.importance / sum(tree$variable.importance)

## which customers which items?
custStats %<>% as.data.table() 
pur[custStats[,.(cust_ID, custG = group)], on = "cust_ID"
    ][prodStats[,.(prod_ID, prodG = group)], on = "prod_ID"] -> purGrp

purGrp[,.(custG, prodG)
       ][,.(freq = .N),
         by = .(custG, prodG)] -> purGrp
purGrp[, sum := sum(freq), by = "custG"
       ][,share := freq/sum
         ][,.(custG, prodG, share)] -> purGrp

ggplot(purGrp, aes(x = custG, y = share , fill = as.factor(prodG))) +
    geom_col()
