#####################
## Seminar 6       ##
## Michal Kubista  ##
## 14 February 2022##
#####################

install_and_load = function(name, char = T) {
  if (!require(name, character.only = char)) {
    install.packages(name)
  }
  require(name, character.only = char)
}

sapply(
  c("tidyverse","readxl"),
  install_and_load
)

#-- PART 1 - SIMPLE PRICINGS ###################################################
#--- 1.1 ETL -------------------------------------------------------------------
# use data from w2 
transRaw = read_xlsx("w2/data/online_retail.xlsx")

summary(transRaw)

## log transformation!
transRaw = 
  transRaw %>% 
  filter(Quantity > 0 & UnitPrice > 0)

## items with more (or equal to) 6 different prices
prodID = 
  transRaw %>% 
  group_by(StockCode) %>% 
  summarise(n = length(unique(UnitPrice))) %>% 
  arrange(desc(n)) %>% 
  filter(n > 6) %>% 
  pull(StockCode)

trans = transRaw %>% filter(StockCode %in% prodID)
trans =
  trans %>% 
  mutate(InvoiceDate = as.Date(InvoiceDate))

#--- 1.2 PIE TABLE -------------------------------------------------------------
## create elasticity table
## this runs ~ 320 linear models btw :D
elas_table = 
  trans %>% 
  group_by(StockCode) %>% 
  summarise(elas = lm(log(Quantity)~log(UnitPrice))$coefficients[2]) %>% 
  arrange(desc(elas))

summary(elas_table$elas)

## remove outliers and group elasticities
elas_table = elas_table %>% filter(elas < 0)

elas_table = 
  elas_table %>%
  mutate(
    elasG = cut(
      elas,
      breaks = c(-14, -2, -1, 0),
      labels = c("H", "M", "L")
      )
  )

summary(elas_table$elasG)

## function to find price index of the product relative to usual price
find_price_index = function(code, day) {
  
  price = 
    trans %>% 
    filter(StockCode == code) %>% 
    select(StockCode, InvoiceDate, UnitPrice)
  
  target = 
    price %>% 
    filter(InvoiceDate == day) %>% 
    pull(UnitPrice) %>% 
    mean(na.rm = T)
  
  mean_price = median(price$UnitPrice, na.rm = TRUE)
  return(target / mean_price)
}

## find price indexes for given day
## create table of price indexes
pi_table = 
  map_dbl(prodID, find_price_index, day = "2011-12-05")

## transform to data.table and group indexes
pi_table =
  tibble(StockCode = prodID, pi = pi_table) %>% 
  filter(!is.na(pi)) %>% 
  mutate(piG = ifelse(pi > 0.9, ifelse(pi > 1.1, "H", "M"),"L"))
summary(as.factor(pi_table$piG))

## join together and create chart input 
pie_table = 
  elas_table %>% 
  inner_join(pi_table, by = "StockCode") %>% 
  filter(!is.na(elas))

## ordered factors for visualisation
pie_table = 
  pie_table %>% 
  mutate(group = paste0(piG, elasG)) %>% 
  mutate(piG = ordered(piG, c("L","M","H"))) %>% 
  mutate(elasG = ordered(elasG, c("L","M","H")))

ggplot(pie_table, aes(x = piG, y = elasG, color = group)) +
  geom_jitter() +
  geom_hline(yintercept = 1.5) +
  geom_hline(yintercept = 2.5) +
  geom_vline(xintercept = 1.5) +
  geom_vline(xintercept = 2.5)

#--- 1.2 PENE TABLE -------------------------------------------------------------
## filter SKUs with high elasticity and high price index
hh_items = 
  pie_table %>%
  filter(pie_table$group == "HH") %>% 
  pull(StockCode)

## set base price (base for promo calculation)
base = 
  trans %>% 
  group_by(StockCode) %>% 
  summarise(baseprice = quantile(UnitPrice, 0.75))

## if actual price goes lower than 95% of base price, mark as promo
trans = 
  trans %>% 
  inner_join(base, by = "StockCode") %>% 
  mutate(diff = UnitPrice / baseprice) %>% 
  mutate(promo = ifelse(diff < 0.95, 1, 0))

## create pene table
## runs ~ 40 linear models
pene_table = 
  trans %>% 
  filter(StockCode %in% hh_items) %>% 
  group_by(StockCode, promo) %>% 
  summarise(elas = lm(log(Quantity)~log(UnitPrice))$coefficients[2]) %>% 
  arrange(desc(elas))

## long to wide
pene_table =
  pene_table %>% 
  mutate(promo = ifelse(promo == 0, "N", "P")) %>% 
  spread(promo, elas) %>% 
  drop_na()

## ordered factors
pene_table$PG = ordered(ifelse(pene_table$P < -1.7, "H", "L"), c("L", "H"))
pene_table$NG = ordered(ifelse(pene_table$N < -1.2, "H", "L"), c("L", "H"))
pene_table = 
  pene_table %>% 
  mutate(group = paste0(PG, NG))

ggplot(pene_table, aes(x = PG, y = NG, color = group)) +
  geom_jitter() +
  geom_hline(yintercept = 1.5) +
  geom_vline(xintercept = 1.5)