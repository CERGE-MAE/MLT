#####################
## Seminar 3       ##
## Michal Kubista  ##
## 20 January 2021 ##
#####################

path2data = "w3/data"

if (!dir.exists(path2data)) {
    dir.create(path2data, recursive = T)
}

setwd("w3/data")
## very, very bad practise!

sapply(
    c("data.table","dplyr","magrittr","ggplot2", "purrr"),
    require, character.only = T)

#-- PART 1 - ETL ##############################################################

#--- 1.1 DATA DOWNLOAD --------------------------------------------------------
# url = "http://files.grouplens.org/datasets/movielens/ml-latest-small.zip"
# download.file(url, "data.zip")
# unzip("data.zip")
# rm(url)
# 
# list.files()
# newFiles = list.files("ml-latest-small", full.names = TRUE)
# 
# ## move the files
# for (i in newFiles) {
#     new = gsub("ml-latest-small/","",i)
#     file.rename(i, new)
# }
# 
# readLines("README.txt") %>% 
#     gsub("-*","",.) %>% 
#     gsub("=*","",.) %>% 
#     .[!. == ""] 
# 
# ## clean the directory
# rm(i, newFiles, new)
# file.remove(c("data.zip","links.csv", "README.txt"))
# unlink("ml-latest-small", recursive = TRUE)

#########################################################################
# DOWNLOAD DATA AT
# https://drive.google.com/drive/folders/1pSfHwDtXdSj2CL_oC83My7f7b6ujDGFv?usp=sharing
#########################################################################

## read input
input = map(list.files(), fread, data.table = F)
names(input) = list.files() %>% gsub(".csv","",.)

#--- 1.2 TAGS EXPLORATION -----------------------------------------------------
tags = input$tags

## basic overview
dim(tags)
tags %>% str
tags %>% summary

# Convert time-date to useful format
#
# Reminder: Using notation %<>% we UPDATE LHS using a function on RHS
#
tags$timestamp %<>% as.POSIXct(origin = "1970-01-01")
# same as previous line!
# tags$timestamp = tags$timestamp %>% as.POSIXct(origin = "1970-01-01") 

# Next line does the same withou pipes
# tags$timestamp = as.POSIXct(tags$timestamp,origin = "1970-01-01")


# Count unique elements in the data
map(tags, ~length(unique(.)))

## table & frequency
# Functions in the following pipe:
# - "table" counts frequencies
# - Operator %T>% calls subsequent command and returns the original data on LHS
# - "hist()" makes histogram
# - "select()" chooses only variables which are mentioned (Freq)

tags$userId %>% 
    table() %T>%
    hist() %>%
    as.data.frame() %>% 
    select(Freq) %>% 
    summary()

# Next line does the same:
# summary(select(as.data.frame(table(tags$userId)),Freq))

# Show first element
tags$tag %>% head()

# View frequencies of tags
tags$tag %>% 
    table() %>% 
    as.data.frame() %>% View()

# Remove movie tags and switch to data on movies.
input$tags = NULL
rm(tags)

#--- 1.3 MOVIES EXPLORATION ---------------------------------------------------

movies = input$movies

## basic overview: data size, description and summary
dim(movies)
movies %>% str
movies %>% summary

# convert movieId to strings
movies$movieId %<>% as.character()

# count unique elements in each column: more Ids than movies: some (two!!!) titles are duplicated
map(movies, ~length(unique(.)))

## find non-unique titles (using pipes):
#  count frequencies for each title -> sort in descending order -> slice first two elements

movies$title %>% 
    table %>% 
    as.data.frame() %>%
    arrange(desc(Freq)) %>% 
    head(2)

# Repeated movies are Hamlet (2000) and War of the Worlds (2005)
# Now, we want to exclude them from the data. For that reason, we look for Ids of these movies and reassign
movies %>% 
    filter(title == "Hamlet (2000)")

movies %>% 
    filter(title == "War of the Worlds (2005)")

## set for later
reset_ID = function(ids) {
    ids[ids == "65665"] = "3598"
    ids[ids == "64997"] = "34048"
    return(ids)
}

# We can remove these repeated records just in one line
movies %<>%
    filter(movieId != "65665" & movieId != "64997")

## We're going to look for k-nearest movie using gendres.
## Now, we make a list of genres by splitting the column "genres"
movies$genres %>% 
    strsplit("\\|") %>%
    unlist() %>% 
    unique() -> genres

# Overall, 20 different genres

## helper function :
#  take the string with genres -> split genres by symbol "|" -> indicate genres
set_genre = function(x) {
    x %>% 
        strsplit("\\|") %>% 
        unlist -> 
        genreCache
    
    genres %in% genreCache %>% 
        as.numeric()
}

## genres incidence matrix
map(movies$genres, set_genre) %>% 
    unlist() %>% 
    matrix(nrow(movies),length(genres), byrow = T) -> genreTable

colnames(genreTable) = genres

## add genres to movies data
movies = cbind(movies, genreTable)

rm(genres, genreTable, set_genre)

#--- 1.4 RATING EXPLORATION ---------------------------------------------------
ratings = input$ratings

## basic overview
dim(ratings)
ratings %>% str
ratings %>% summary

## column format
ratings[,1:2] =  apply(ratings[,1:2],2, as.character)

# again, convert time-date to useful format
ratings$timestamp %<>% as.POSIXct(origin = "1970-01-01")
summary(ratings$timestamp)

## count unique elements per column
map(ratings, ~length(unique(.)))

## reset ID of duplicated movies. Here, we're using the function that we have defined before
ratings$movieId %<>% 
    reset_ID

## check integrity: 
# such a line counts a number of rows in "ratings" which do not appear in "movies".
anti_join(ratings, movies, by = "movieId")

# We don't need this function anymore
rm(reset_ID)

#-- PART 2 - kNN: CONTENT BASED RECOMMENDER ###################################

# Purpose: make a recommendation of movie for a given user based on what they were watching before.

#--- 2.1 PREPARE DATA ---------------------------------------------------------

## rating frequency: how many times each user rate each movie
ratings$userId %>% 
    table() %>% 
    as.data.frame() %>% View()

## choose user (arbitrary)
ratings %>% 
    filter(userId == "547") -> rat547

## user's rating overview
rat547 %>% summary()

#seq(0.5,5,by = 0.5) %>% summary()

# Next graph shows that ratings is not "uniform"
qqplot(seq(0.5,5,by = 0.5), rat547$rating)
lines(0:5,0:5)

## rating over time
ggplot(rat547, aes(x = timestamp, y = rating)) +
    geom_line() +
    geom_smooth(method = "loess")

## choose threshold
## this is a parametr!!!
    ## it's choice needs discussion
rat547$class = ifelse(rat547$rating > 4, 1, 0)

# just a fraction of rates higher than 4
mean(rat547$class)

# Use the library with the knn-classifier
if (!require(class)) {
    install.packages("class")
    library(class)
}

# Join movies and ratings from the user
movies547 = left_join(movies, rat547[,c("movieId","class")])
colnames(movies547)

# Define train and test data.
# Train data: those with some rating
moviesTrain = movies547 %>% filter(!is.na(class))
moviesTest = movies547 %>% filter(is.na(class))

#--- 2.2 CROSS-VALIDATION: FIND THE RIGHT "k" ---------------------------------
set.seed(1234)
indexTrain1 = sample(1:nrow(moviesTrain), 1500)

moviesTrain1 = moviesTrain[indexTrain1,]
moviesTrain2 = moviesTrain[-indexTrain1,]

result = c()
for (i in 1:10) {
    moviesTrain2$new =
        knn(moviesTrain1[,4:22],
            moviesTrain2[,4:22],
            moviesTrain1[,"class"],
            i)
    # Fraction of truely-classified movies
    result[i] = mean(moviesTrain2$new == moviesTrain2$class)
    print(result[i])
}

plot(1:i, result, type = "l")

# Choose "k" to maximize prediction quality
k = which.max(result)
# k = 2

# Remove unnecessary variables
rm(result, moviesTrain1, moviesTrain2, indexTrain1, i)

##--- 2.3 RECOMMEND -----------------------------------------------------------

# Last step: apply the classificator to test data
moviesTest$new =
    knn(moviesTrain[,4:22], moviesTest[,4:22], moviesTrain[,"class"], k)

movies547[,4:22] %>% map_dbl(mean) %>% .[order(., decreasing = T)]

moviesTest[moviesTest$new == 1,4:22] %>%
    map_dbl(mean) %>%
    .[order(., decreasing = T)]

moviesTest[moviesTest$new == 1,] %>% View()

rm(moviesTest, moviesTrain, movies547, rat547, k)

##-- PART 3 - COLLABORATIVE FILTERING #########################################

##--- 3.1 - PREPARATIONS ------------------------------------------------------
if (!require(tidyr)) {
    install.packages("tidyr")
    library(tidyr)
}

ratings[c(99041,99132),]
ratings = ratings[-99132,]

# Pipe-scheme:
# use ratings sample -> identify ratings>4 -> select necessary columns -> reshape data (columns: users; rows: movies; cells: incidence)
ratings %>% 
    mutate(class = ifelse(rating > 4, 1, 0)) %>% 
    select(userId, movieId, class) %>%  
    spread(key = userId, value = class, fill = 0) -> ratM 

rownames(ratM) = ratM$movieId
ratM$movieId = NULL

# set it as matrix for later use
ratM %<>% as.matrix()

##--- 3.2 - ITEM-ITEM ---------------------------------------------------------
## way too much for my PC, need to filter
ratMR = ratM[1:1000,]

## factorisation: just multiplying the matrix on its transpose
item = ratMR %*% t(ratMR) 

## erase self-incidence
diag(item) = 0

## choose an user (287 works well)
#user = ratings[sample(1:100000,1), "userId"]

user = 287

## Select just user movies
ratings %>% 
    filter(userId == user, rating > 4) %>% 
    pull(movieId) -> userMov

## find movies in matrix and filter
rownames(item) %in% userMov %>% which() -> indexCR
itemChoice = item[indexCR,]

## for each movie, find the most similar one
apply(itemChoice, 1, which.max) %>% table -> bestPicks
bestPicks

## find the best recommendation
bestPicks %>% which.max() %>% names(bestPicks)[.] -> no1
movies %>% 
    filter(movieId == no1)

movies %>%
    filter(movieId %in% userMov) %>% 
    .[,4:22] %>% 
    map_dbl(mean) %>% 
    {.[order(., decreasing = T)]}

## what has the user already seen?
movies %>%
    filter(movieId %in% userMov) %>% View()

ratings[ratings$userId == user & ratings$movieId == no1,]

rm(ratMR, item, userMov, indexCR, itemChoice, bestPicks, no1, user)

##--- 3.3 - USER - USER -------------------------------------------------------
user = t(ratM) %*% ratM
diag(user) = 0

## choose one user (323)
userID = rownames(user)[sample(1:600,1)]

## most similar users
user[userID,] %>% table() %>% rev() -> bestPicks
bestPicks

## at least 10 users
    ## parameter again!
index = which.max(cumsum(bestPicks) >= 10)
names(bestPicks) %>% as.numeric() %>% .[seq_len(index)] -> index

user[userID,][user[userID,] >= names(index)] %>% names() -> userSim

## movies watched by similar users 
(ratings %>% 
    filter(rating > 4) %>% 
    filter(userId %in% userSim) %>%
    select(movieId) %>%
    table() %>%
    as.data.frame(stringsAsFactors = FALSE) %>% 
    arrange(desc(Freq)) -> recom)

## recommended movies
View(movies %>% 
         filter(movieId %in% recom[1:10,1]))

## what has the user watched
movies[movies$movieId %in% ratings[ratings$userId == userID,"movieId"],] %>%
    View()
