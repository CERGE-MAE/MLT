#####################
## Seminar 3       ##
## Michal Kubista  ##
## 24 January 2022 ##
#####################

path2data = "w3/data"

if (!dir.exists(path2data)) {
    dir.create(path2data, recursive = T)
}

library(tidyverse)

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
input = map(
    list.files(path2data, full.names = T),
    read_csv
    )

names(input) =
    list.files(path2data) %>%
    gsub(".csv","",.)

#--- 1.2 TAGS EXPLORATION -----------------------------------------------------
tags = input$tags

## basic overview
dim(tags)
tags %>% str()
tags %>% summary()

# Convert time-date to useful format
tags %>% 
    mutate(
        timestamp = as.POSIXct(timestamp, origin = "1970-01-01")
    )
# equvivalent solutions
# tags$timestamp = tags$timestamp %>% as.POSIXct(origin = "1970-01-01") 
# tags$timestamp = as.POSIXct(tags$timestamp,origin = "1970-01-01")

# Count unique elements in the data
map(tags, ~length(unique(.)))

## table & frequency
# Functions in the following pipe:
# - "table" counts frequencies
# - Operator %T>% calls subsequent command and returns the original data on LHS
# - "hist()" makes histogram
# - "select()" chooses only variables which are mentioned (Freq)

tags %>% 
    pull(userId) %>% 
    table() %T>%
    hist() %>%
    as.data.frame() %>% 
    select(Freq) %>% 
    summary()

# Show first element
tags$tag %>% head()

# View frequencies of tags
tags$tag %>% 
    table() %>% 
    as.data.frame() %>%
    View()

# Remove movie tags and switch to data on movies.
input$tags = NULL
rm(tags)

#--- 1.3 MOVIES EXPLORATION ---------------------------------------------------

movies = input$movies

## basic overview: data size, description and summary
dim(movies)
movies %>% str()
movies %>% summary()

# convert movieId to strings
movies = 
    movies %>%
    mutate(movieId = as.character(movieId))

# count unique elements in each column: more Ids than movies: 
# some (two!!!) titles are duplicated
map(movies, ~length(unique(.)))

## find non-unique titles (using pipes):
#  count frequencies for each title -> sort in descending order -> 
# slice first two elements
movies$title %>% 
    table() %>% 
    as.data.frame() %>%
    arrange(desc(Freq)) %>% 
    head(2)

# Repeated movies are Hamlet (2000) and War of the Worlds (2005)
# Now, we want to exclude them from the data. For that reason,
# we check the Ids of these movies and reassign
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
movies =
    movies %>% 
    filter(movieId != "65665" & movieId != "64997")

## We're going to look for k-nearest movie using genres.
## Now, we make a list of genres by splitting the column "genres"
movies %>% 
    pull(genres) %>% 
    head()

split_genre = function(x) {
    x %>% 
        strsplit("\\|") %>%
        unlist() %>% 
        unique()
}

genres = 
    movies %>% 
    group_by(movieId) %>% 
    summarise(genre = split_genre(genres))

genres = 
    genres %>% 
    mutate(val = 1) %>% 
    spread(genre, val, fill = 0)

movies = 
    movies %>%
    left_join(genres, by = 'movieId')

View(movies)

rm(genres, split_genre)

#--- 1.4 RATING EXPLORATION ---------------------------------------------------
ratings = input$ratings

## basic overview
dim(ratings)
ratings %>% str()
ratings %>% summary()

## column format
# and again, convert time-date to useful format
ratings = 
    ratings %>% 
    mutate_at(vars(userId, movieId), as.character) %>% 
    mutate(timestamp = as.POSIXct(timestamp, origin = "1970-01-01"))

summary(ratings$timestamp)

## count unique elements per column
map(ratings, ~length(unique(.)))

## reset ID of duplicated movies. Here, we're using the function that we have defined before
ratings = 
    ratings %>%
    mutate(movieId = reset_ID(movieId))

## check integrity: 
# such a line counts a number of rows in "ratings" which do not appear in "movies".
anti_join(ratings, movies, by = "movieId")

# We don't need this function anymore
rm(reset_ID)

#-- PART 2 - kNN: CONTENT BASED RECOMMENDER ###################################

# Purpose: make a recommendation of movie for a given user based on what they
# were watching before.

#--- 2.1 PREPARE DATA ---------------------------------------------------------

## rating frequency: how many times each user rate each movie
ratings %>% 
    pull(userId) %>% 
    table() %>% 
    as.data.frame() %>%
    View()

## choose user (arbitrary)
rat547 = 
    ratings %>% 
    filter(userId == "547")

## user's rating overview
rat547 %>% summary()

# ratings are not "uniform"
rat547 %>% 
    pull(rating) %>% 
    hist()

## rating over time
ggplot(rat547, aes(x = timestamp, y = rating)) +
    geom_line() +
    geom_smooth(method = "loess")

## choose threshold
## this is a parameter!!!
    ## it's choice needs discussion
rat547 = 
    rat547 %>% 
    mutate(class = ifelse(rating > 4, 1, 0))

# just a fraction of rates higher than 4
mean(rat547$class)

# Use the library with the knn-classifier
if (!require(class)) {
    install.packages("class")
    library(class)
}

# Join movies and ratings from the user
movies547 = 
    rat547 %>% 
    select(movieId, class) %>% 
    full_join(movies)
colnames(movies547)

# Define rat and rec data.
# rat - those with some rating
# rec - no rating ~ not seen, recommend some of these!
moviesRat = movies547 %>% filter(!is.na(class))
moviesRec = movies547 %>% filter(is.na(class))

#--- 2.2 CROSS-VALIDATION: FIND THE RIGHT "k" ---------------------------------
set.seed(1234)
indexTrain = sample(seq_len(nrow(moviesRat)), 1500)

moviesTrain = moviesRat[indexTrain,]
moviesTest = moviesRat[-indexTrain,]

result = rep(0,10)
for (i in 1:10) {
    moviesTest$new =
        knn(moviesTrain[,5:24],
            moviesTest[,5:24],
            moviesTrain$class,
            i)
    # Fraction of truely-classified movies
    result[i] = mean(moviesTest$new == moviesTest$class)
    print(result[i])
}

plot(1:i, result, type = "b")

# Choose "k" to maximize prediction quality
k = which.max(result)
# k = 4

# Remove unnecessary variables
rm(result, moviesTrain, moviesTest, indexTrain, i)

##--- 2.3 RECOMMEND -----------------------------------------------------------

# Last step: apply the classifier to test data
moviesRec$new = 
    knn(
        moviesRat[,5:24],
        moviesRec[,5:24],
        moviesRat$class,
        k
        )

movies547[,5:24] %>%
    map_dbl(mean) %>%
    .[order(., decreasing = T)]

moviesRec %>% 
    filter(new == 1) %>% 
    .[5:24] %>%
    map_dbl(mean) %>%
    .[order(., decreasing = T)]

movies547 %>% filter(class == 1) %>% View("well-rated")
moviesRec %>% filter(new == 1) %>% View("recommended")

rm(moviesRat, moviesRec, movies547, rat547, k)

##-- PART 3 - COLLABORATIVE FILTERING #########################################

##--- 3.1 - PREPARATIONS ------------------------------------------------------
ratings[c(99041,99132),]
ratings = ratings[-99132,]

# Pipe-scheme:
# use ratings sample -> identify ratings>4 -> select necessary columns -> reshape data (columns: users; rows: movies; cells: incidence)
ratM = 
    ratings %>% 
    mutate(class = ifelse(rating > 4, 1, 0)) %>% 
    select(userId, movieId, class) %>%  
    spread(key = userId, value = class, fill = 0) 

rnames = ratM$movieId
ratM$movieId = NULL

# set it as matrix for later use
ratM = as.matrix(ratM)
rownames(ratM) = rnames
ratM[1:10, 1:10]

##--- 3.2 - ITEM-ITEM ---------------------------------------------------------
## way too much for my PC, need to filter
ratMR = ratM[1:1000,]

## factorisation: just multiplying the matrix on its transpose
item = ratMR %*% t(ratMR) 
item[1:5,1:5]

## erase self-incidence
diag(item) = 0

## choose an user (287 works well)
#user = ratings[sample(1:100000,1), "userId"]
user = 287

## Select just user movies
userMov = 
    ratings %>% 
    filter(userId == user, rating > 4) %>% 
    pull(movieId)

## find movies in matrix and filter
indexCR = 
    rownames(item) %in% userMov %>%
    which()
itemChoice = item[indexCR,]

## for each movie, find the most similar one
bestPicks =
    apply(itemChoice, 1, which.max) %>%
    table
bestPicks

## find the best recommendation
no1 = 
    bestPicks %>%
    which.max() %>%
    {names(bestPicks)[.]}

movies %>% 
    filter(movieId == no1) %>% View()

movies %>%
    filter(movieId %in% userMov) %>% 
    .[,4:23] %>% 
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
user[1:5,1:5]

## choose one user (323)
userID = rownames(user)[sample(1:600,1)]

## most similar users
bestPicks =
    user[userID,] %>%
    table() %>%
    rev() 
bestPicks

## at least 10 users
    ## parameter again!
index = which.max(cumsum(bestPicks) >= 10)

userSim = 
    user[userID,][user[userID,] >= as.numeric(names(index))] %>%
    names()

## movies watched by similar users 
recom = 
    ratings %>% 
    filter(rating > 4) %>% 
    filter(userId %in% userSim) %>%
    select(movieId) %>%
    table() %>%
    as.data.frame(stringsAsFactors = FALSE) %>% 
    arrange(desc(Freq))
head(recom)

## recommended movies
movies %>% 
    filter(movieId %in% recom[1:10,1]) %>% 
    View()

## what has the user watched
ratings %>% 
    filter(userId == userID) %>% 
    pull(movieId) %>% 
    {filter(movies, movieId %in% .)} %>%
    View()
