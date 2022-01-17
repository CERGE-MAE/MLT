####################
## Seminar 1      ##
## Michal Kubista ##
## 3 January 2022 ##
####################

purrr::map_lgl(
  c("data.table","dplyr","magrittr","ggplot2"), 
  ~require(.x, character.only = T)
)

sapply(
  c("data.table","dplyr","magrittr","ggplot2"),
  require,
  character.only = T
)

path2data = "w1/data/seminar"

if (!dir.exists(path2data)) {
  dir.create(path2data, recursive = T)
}

#-- PART 1 - LOADING TIPS & TRICKS ############################################

## download data @ https://drive.google.com/drive/folders/1sJjizO1ycLm4BEZiuFZ8Hmb1KJcD_XLk?usp=sharing

#--- 1.1 SEVERAL SIMILAR FILES ------------------------------------------------

## data origin https://www.kaggle.com/onlineauctions/online-auctions-dataset
## script used to create our data:
# auction = data.table::fread(file.path(path2data,"auction.csv"), dec = ".",
# data.table = F)
# str(auction)
# 
# auction[,c("bid", "bidtime", "bidderrate", "openbid", "price")]  %<>%
#       apply(., 2, as.numeric)
# str(auction)
# 
# data.table::fwrite(auction, file.path(path2data,"auction.csv"))
# 
# auction %<>% dplyr::mutate(days = ceiling(bidtime))
# 
# g_write = function(x){
#       write.csv(x,file.path(path2data,
#                           paste0("auction_day_",unique(x$days),".csv")),
#                 row.names = F)
#       return(x)
# }
# 
# auction %>%
#       group_by(days) %>%
#       do(g_write(.))
# 
# rm(auction, g_write)
#----

auction_Names = function() {
  return(
    list.files(path2data, full.names =  T, pattern = "auction_day")
  )
}

#---- 1.1.1 FOR LOOP ----------------------------------------------------------

auction_Names()

input = list()

# vector growing - BAD IDEA!!!
for (i in auction_Names()) {
  input[[length(input) + 1]] = read.csv(i)
}

sapply(input, colnames)
inputTable = do.call(rbind.data.frame, input)

rm(input, inputTable, i)

#---- 1.1.2. APPLY ------------------------------------------------------------

input = lapply(auction_Names(), read.csv)
inputTable = do.call(rbind.data.frame, input)

#---- 1.1.2. PURRR ------------------------------------------------------------

inputTable = purrr::map_df(auction_Names(), read.csv)

rm(input, inputTable)

#---- 1.1.3. DATA.TABLE -------------------------------------------------------

dtNames = data.table::data.table(file = auction_Names())
dtTable = dtNames[, fread(file), by = file][,-"file"]

## shorter version
dtTable = 
  data.table::data.table(file = auction_Names())[,fread(file), by = file]

rm(dtNames, dtTable, auction_Names)

#--- 1.2 SEVERAL DIFFERENT (CONNECTED) FILES ----------------------------------
## script used to create our data:
# auction = fread(file.path(path2data,"auction.csv"), dec = ".", data.table = F) %>%
#       mutate(ID = paste0(auctionid,bidtime,bidder))
# 
# apply(auction, 2, function(x) length(unique(x)))
# 
# itemTable = auction %>% select(ID, item, auction_type)
# priceTable = auction %>% select(ID, price)
# auctionTable = auction %>% select(-c(item, auction_type, price))
# 
# dtOut = data.table(file = c("itemTable","priceTable", "auctionTable"))
# dtOut[,fwrite(get(file),file.path(path2data,paste0(file,".csv"))),
#       by = file
#       ]
# 
# rm(auction, auctionTable, priceTable, itemTable, dtOut)
#----

inFileNames = list.files(path2data, "Table", full.names = T)
inFileNames

#---- 1.2.1 DATA.TABLE JOIN --------------------------------------------------

input = lapply(inFileNames, fread)
names(input) = inFileNames %>% gsub(".*/","",.) %>% gsub(".csv","",.)

dtTable = input$auctionTable[input$itemTable, on = "ID"
                             ][input$priceTable, on = "ID"]

rm(dtTable)

#---- 1.2.2 (D)PLYR JOIN ----------------------------------------------------

inputTable = 
  dplyr::full_join(input$auctionTable, input$itemTable, by = "ID") %>%
  full_join(input$priceTable)

inputTable2 = plyr::join_all(input)

rm(input, inputTable, inputTable2, inFileNames)

#-- PART 2 - CLEANING TIPS & TRICKS ###########################################

#--- 2.1 REGEX ----------------------------------------------------------------

#---- 2.1.1 DATA & REGEX
## data origin: https://www.datazar.com/project/p9d520430-ab0a-4f26-a44a-39b08d0e41bb/overview

unzip(
  file.path(path2data,
            "fixed-broadband-speeds-postcode-london-2016.xlsx.zip"),
  exdir = path2data
)

broadband = 
  readxl::read_excel(
    file.path(path2data,"fixed-broadband-speeds-postcode-london-2016.xlsx")
  )

str(broadband)

broadband = broadband[,c(1,8:11,16:19,35,36)]
colnames(broadband) =
  c("place", "downAvg", "downMed", "downMin", "downMax", "upAvg", "upMed",
    "upMin","upMax", "lat","lon")

summary(broadband)

broadband$downAvg %>%
  stringr::str_replace("[0-9]*","") %>%
  stringr::str_replace("[.,][0-9]*","") %>% unique()

# how to improve this?
removeStuff = function(x){
  x %>% stringr::str_replace("<4","2") %>%
    stringr::str_replace(",","\\.") %>%
    stringr::str_replace("N/A","") %>%
    as.numeric()
}

broadband[,-1] = apply(broadband[,-1], 2, removeStuff)

summary(broadband)

#--- 2.2 MANIPULATION ---------------------------------------------------------
#---- 2.2.1 DPLYR -------------------------------------------------------------

broadband %>% dplyr::filter(lon > 0, lat > 51.6)

broadband %>%
  dplyr::select(lon, lat) %>% 
  dplyr::filter(lon > 0, lat > 51.6)

broadband %>% 
  dplyr::arrange(downAvg)

broadband %>% dplyr::mutate(downRange = downMax - downMin,
                            upRange = upMax - upMin)

broadband %>%
  dplyr::group_by(downAvg) %>% 
  dplyr::summarise(meanMax = mean(downMax),
                   meanMin = mean(downMin)) %>% 
  dplyr::mutate(meanDif = meanMax - meanMin) %>% 
  dplyr::arrange(-meanDif)

#---- 2.2.2 DATA.TABLE --------------------------------------------------------

data.table::as.data.table(broadband)[
  ,.(meanMax = mean(downMax),
     meanMin = mean(downMin)),
  by = downAvg
  ][,.(downAvg, meanMax, meanMin,
       meanDif = meanMax - meanMin)
    ][order(-meanDif)]

#--- 2.3 LONG-WIDE -----------------------------------------------------------

broadbandLong = tidyr::gather(broadband[,1:5],
                              key = "metric",
                              value = "measure",
                              downAvg:downMax)

broadbandWide = tidyr::spread(broadbandLong, metric, measure)

rm(broadband, broadbandWide, broadbandLong, removeStuff)

#-- PART 3 - EXPLORING TIPS & TRICKS ##########################################

auction = data.table::fread(file.path(path2data,"auction.csv"),
                            dec = ".", data.table = F)

#--- 3.1 BASE R

colSelect = !colnames(auction) %in% c("auctionid","bidder","item","auction_type")

auction %>% 
  complete.cases() %>% 
  auction[.,colSelect] %>% 
  plot()

#--- 3.2 GGally

GGally::ggpairs(auction[,colSelect])

#--- 3.3 ORLY?
url =
  "http://www4.stat.ncsu.edu/~stefansk/NSF_Supported/Hidden_Images/orly_owl_files/orly_owl_Lin_4p_5_flat.txt"

orly = read.table(url, header = FALSE)
GGally::ggpairs(orly)

fit = lm(V1 ~ . - 1, data = orly)
summary(fit)

orlyRes = data.frame(fitted = fit$fitted.values, resid = fit$residuals)

ggplot(orlyRes, aes(x = fitted, y = resid)) + 
  geom_point(size = 2)
