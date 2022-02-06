#####################
## Seminar 5       ##
## Michal Kubista  ##
## 7 February 2022 ##
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

# download from https://drive.google.com/drive/folders/1M1nsc282Eoi_ax_O6piuDrWjYcxwAZWl?usp=sharing
## ID coercing
raw = read_excel("w5/data/transac.xls") %>% as_tibble()

str(raw)
summary(raw)

map_dbl(raw, ~length(unique(.)))

colnm =
    raw %>%
    colnames() %>% 
    gsub(" |-","_",.) %>% 
    tolower()

length(unique(colnm)) == ncol(raw)
colnames(raw) = colnm

#---- 1.1.1 TABLES SPLITTING ---------------------------------------------------
## customer table
cust =
    raw %>% 
    group_by(customer_id) %>% 
    summarise(
        cust_name = unique(customer_name),
        segment = unique(segment)
        ) %>% 
    rename(cust_id = customer_id)


## products table
prod = 
    raw %>% 
    group_by(product_id) %>% 
    summarise_at(
        vars(category, sub_category, product_name),
        unique
        ) %>% 
    rename(prod_id = product_id)

## locations table
loc = 
    raw %>% 
    group_by(postal_code) %>% 
    summarise_at(
        vars(city, state, region),
        unique
    ) %>% 
    rename(zip = postal_code)

## ZIP duplicates handling
loc %>% pull(zip) %>% unique() %>% length()
nrow(loc)

loc %>% 
    select(zip) %>% 
    filter(duplicated(zip)) %>% 
    {filter(.data = loc, zip == .)}

loc =
    loc %>% 
    mutate(zip = 
               ifelse(
                   zip == "92024" & city == "Encinitas",
                   92028,
                   zip)
           )

## transactions table
pur = 
    raw %>% 
    group_by(row_id) %>% 
    summarise(
        pur_id = unique(order_id),
        date = unique(order_date),
        trans_mode = unique(ship_mode),
        cust_id = unique(customer_id),
        prod_id = unique(product_id),
        sales = sales,
        items = quantity,
        price = sales / quantity,
        promo = discount
    )

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
bas = 
    pur %>% 
    mutate(promoC = ifelse(promo == 0, 0, 1)) %>% 
    group_by(pur_id) %>% 
    summarise(
        cust_id = unique(cust_id),
        date = unique(date),
        sales = sum(sales),
        items = sum(items),
        price = mean(sales/items),
        promoC = sum(promoC),
        noSKU = n()
        ) %>% 
    mutate(promoC := promoC/noSKU)

## sales histogram
ggplot(bas, aes(x = sales)) +
    geom_histogram(binwidth = 1000)

ggplot(bas %>% filter(sales < 1000), aes(x = sales)) +
    geom_histogram(binwidth = 100)

ggplot(bas %>% filter(sales < 1000), aes(x = 1, y = sales)) +
    geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))

## the lower the prices, the more the items?
ggplot(bas, aes(x = items, y = price)) +
    geom_point() +
    geom_smooth(method = "loess")

## basket promo share
ggplot(bas %>% arrange(bas$promoC), aes(x = 1:nrow(bas), y = promoC)) +
    geom_line()

### analyse the periodicity on nopromo baskets ####
np_bas =  bas %>% filter(promoC == 0)
np_bas$cust_id %>% unique %>% length

np_bas %>%
    pull(cust_id) %>%
    table %>%
    as_tibble() %>%
    arrange(desc(n)) %>%
    View()
rm(np_bas)                                        
### nothing to analyse :( #########################

## basket unique SKU
ggplot(bas %>% arrange(noSKU), aes(x = 1:nrow(bas), y = noSKU)) +
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
daily = 
    bas %>% 
    mutate(
        day = format(date, "%u"),
        promoG = ifelse(promoC > 0.5, 1, 0)
        ) %>% 
    group_by(day, promoG) %>% 
    summarise(visits = n())

ggplot(daily, aes(x = day, y = visits, col = as.factor(promoG),
                  group = as.factor(promoG))) +
    geom_line() +
    geom_point()

#-- PART 2 - SEGMENTATIONS #####################################################

#--- 2.1 CUSTOMERS -------------------------------------------------------------
## feature engineering
custStats = 
    pur %>% 
    group_by(cust_id) %>% 
    summarise(
        unSKU = length(unique(prod_id)),
        itemsSKU = mean(items),
        priceSKU = mean(price),
        promoSKU = mean(promo)
    )

basStats = 
    bas %>% 
    group_by(cust_id) %>% 
    summarise(
        basSKU = mean(noSKU),
        valueBas = mean(sales),
        itemBas = mean(items),
        priceBas = mean(price)
    )

custStats = 
    cust %>% 
    select(cust_id, segment) %>% 
    inner_join(custStats, by = "cust_id") %>% 
    inner_join(basStats, by = "cust_id")

rm(basStats)

## dummies
custStats$segment %>% unique

custStats = 
    custStats %>% 
    mutate(corp = ifelse(segment == "Corporate", 1, 0)) %>% 
    mutate(ho = ifelse(segment == "Home Office", 1, 0)) %>% 
    select(-segment)

## into matrix and scale
custStats %>% summary()

custStatsS = 
    custStats %>% 
    select(-cust_id) %>% 
    scale()

rownames(custStatsS) = custStats$cust_id

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
prodStats = 
    pur %>% 
    group_by(prod_id) %>% 
    summarise(
        custPen = length(unique(cust_id))/nrow(cust),
        basPen = length(unique(pur_id))/nrow(bas),
        items = mean(items),
        price = mean(price),
        promo = mean(promo)
    )

## dummies
prodStats =
    prod %>%
    select(prod_id, category) %>% 
    inner_join(prodStats, by = "prod_id")

prod$category %>% table

prodStats = 
    prodStats %>% 
    mutate(
        furn = ifelse(category== "Furniture", 1, 0),
        tech = ifelse(category == "Technology", 1, 0)
    ) %>% 
    select(-category) %>% 
    filter(!duplicated(prod_id))
    
## matrix and scale
prodStatsS = 
    prodStats %>% 
    ungroup() %>% 
    select(-prod_id) %>% 
    scale()

rownames(prodStatsS) = prodStats$prod_id

cluster = hclust(dist(prodStatsS), method = "complete")
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

## which customers which items?
purGrp = 
    custStats %>% 
    rename(custG = group) %>% 
    select(cust_id, custG) %>% 
    inner_join(pur, on = custid)

purGrp = 
    prodStats %>% 
    rename(prodG = group) %>% 
    select(prod_id, prodG) %>% 
    inner_join(purGrp, on = custid)

purGrp = 
    purGrp %>% 
    group_by(custG, prodG) %>% 
    summarise(freq = n()) %>% 
    ungroup() %>% 
    group_by(custG) %>% 
    mutate(sum = sum(freq)) %>% 
    mutate(share = freq / sum) %>% 
    select(custG, prodG, share)

ggplot(purGrp, aes(x = custG, y = share , fill = as.factor(prodG))) +
    geom_col()
