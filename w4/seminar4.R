#####################
## Seminar 4       ##
## Michal Kubista  ##
## 31 January 2022 ##
#####################

install_and_load = function(name, char = T) {
  if (!require(name, character.only = char)) {
    install.packages(name)
  }
  require(name, character.only = char)
}

sapply(
    c("tidyverse", "magrittr", "GGally", "cluster", "dbscan"),
    install_and_load
)

rm(install_and_load)

#-- PART 1 - kMeans vs hclust #################################################

#--- 1.1 ETL ------------------------------------------------------------------
##  Data: annual spending in monetary units on diverse product categories

url = "https://archive.ics.uci.edu/ml/machine-learning-databases/00292/Wholesale%20customers%20data.csv"
wholesale = read_csv(url)
rm(url)

## data overview
str(wholesale)
summary(wholesale)

## unique region and channel values
wholesale %>%
    select(Channel, Region) %>% 
    map(unique)
## no need to recode

table(wholesale[,1:2])
ggpairs(wholesale[,-1:-2])


## log scalling
wholeLg = 
    wholesale %>% 
    select(Fresh : Delicassen) %>% 
    map_df(log) %>% 
    cbind(wholesale[,1:2],.)

ggpairs(wholeLg[,-1:-2])

## box-plots of original and scaled data
par(mfrow = 2:3)
iwalk(wholesale[,-1:-2], ~boxplot(.x, main = .y))
iwalk(wholeLg[,-1:-2], ~boxplot(.x, main = .y))
par(mfrow = c(1,1))

## aggregation
wholeLg %>% 
    group_by(Region) %>% 
    summarise_at(vars(-group_cols(), -Channel), mean)

wholeLg %>% 
    group_by(Channel) %>% 
    summarise_at(vars(-group_cols(), -Region), mean)

## outlier stalker
detect_outliers = function(x){
    upperBound = median(x) + 1.5 * IQR(x)
    lowerBound = median(x) - 1.5 * IQR(x)
    which(!between(x, lowerBound, upperBound))
}

## stalking outliers
outIndex = 
    wholeLg %>% 
    select(Fresh : Delicassen) %>% 
    map(detect_outliers) %>% 
    unlist %>%
    table() %>%
    as_tibble() %>% 
    arrange(desc(n)) %T>% 
    View() %>%
    filter(n > 2) %>%
    # parameter!
    rename("obs" = ".") %>% 
    pull(obs) %>% 
    as.numeric()

wholeC = wholeLg[-outIndex,]
rm(outIndex, detect_outliers)

#--- 1.2 kMeans ----------------------------------------------------------------
set.seed(12345)

## elbow estimation
### Number of clusters?
### Idea: 
### - run kmeans over several values of k;
### - compute total within-cluster sum of squared errors (SSE)
### - choose the small k-value with small SSE

inter = rep(0, 10)
for (i in 1:10) {
    inter[i] = kmeans(wholeC[,-1:-2],i)$tot.withinss
}

plot(1:i, inter, type = "b")
lines(c(2,10), inter[c(2,10)], col = "green")
lines(c(3,10), inter[c(3,10)], col = "red")
lines(c(4,10), inter[c(4,10)], col = "blue")

(inter / inter[1]) %>% diff(1) * 100
rm(i,inter)

## The choice of k:
k = 2

## visualisation: 
## - Two dimensions (Region and Channel)
kModel = kmeans(wholeC[,-1:-2], k)
wholeC$clust = kModel$cluster

## Use ggplot() with geom_jitter() to add some "noise"
ggplot(wholeC, aes(x = Channel, y = Region, color = as.factor(clust))) +
    geom_jitter()

## cluster explanations?
wholeC %>% 
    split(.$clust) %>% 
    map(~table(
        select(.x,Channel, Region)
        ))

clusplot(wholeC, kModel$cluster, color = TRUE, shade = T, labels = 1)

#--- 1.3 hClust ----------------------------------------------------------------

## Find distances between all pairs of rows (metrics is important!)
wholeMat = dist(wholeC[,-1:-2])

# complete with squared distances!
hModel = hclust(wholeMat, method = "complete")
plot(hModel)

## find the longest line (height!)
heights = hModel$height

h =
    heights %>% 
    diff(lag = 1) %>%
    which.max() %>%
    add(1) %>%
    heights[.]


## label groups
rect.hclust(hModel, h = h)

groups = cutree(hModel, h = h - 0.1)

groups %>% table

## visualisation 
ggplot(wholeC, aes(x = Channel, y = Region, color = as.factor(groups))) +
    geom_jitter()

wholeC$clust = groups

wholeC %>% 
  split(.$clust) %>% 
  map(~table(
      select(.x,Channel, Region)
      ))

clusplot(wholeC[,-1:-2], groups , color = TRUE, 
         shade = T, labels = 1, lines = 0)


#--- 1.2 DBSCAN ----------------------------------------------------------------
wholeC$clust = NULL

dbscan::kNNdistplot(wholeC, 4)
abline(h = 2.65, col = "orange")
dModel = dbscan(wholeC, 2.65, 5)

# all in one cluster
dModel$cluster %>% table()

clusplot(wholeC[,-1:-2], dModel$cluster, color = TRUE, 
         shade = T, labels = 1, lines = 0)
