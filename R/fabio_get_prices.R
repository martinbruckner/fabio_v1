library(reshape2)

rm(list=ls()); gc()

##########################################################################
# Make intitial settings
##########################################################################
# read region classification
regions <- read.csv(file="Regions.csv", header=TRUE, sep=";")
# read commodity classification
items <- read.csv(file="Items.csv", header=TRUE, sep=";")



##########################################################################
# Start loop for a series of years
##########################################################################
# year=1986
year=2013
for(year in 1995:2013){
  print(year)
  load(file=paste0("data/yearly/",year,"_BTD.RData"))
  
  # get commodity prices from BTD
  prices <- aggregate(cbind(tHead,tonnes,tUSD) ~ From.Country.Code + From.Country + Item.Code + Item, BTD, sum)
  prices$price <- prices$tUSD / prices$tonnes
  prices$price[prices$tHead>0] <- prices$tUSD[prices$tHead>0] / prices$tHead[prices$tHead>0]
  prices$price[!is.finite(prices$price)] <- 0
  worldprices <- aggregate(cbind(tHead,tonnes,tUSD) ~ Item.Code + Item, prices, sum)
  worldprices$price <- worldprices$tUSD / worldprices$tonnes
  worldprices$price[worldprices$tHead>0] <- worldprices$tUSD[worldprices$tHead>0] / worldprices$tHead[worldprices$tHead>0]
  worldprices$price[!is.finite(worldprices$price)] <- 0
  prices$worldprice <- worldprices$price[match(prices$Item.Code,worldprices$Item.Code)]
  # set minimum/maximum at 20%/500% of the world average price
  prices$price[(prices$price/5)>prices$worldprice] <- prices$worldprice[(prices$price/5)>prices$worldprice]*5
  prices$price[(prices$price*5)<prices$worldprice] <- prices$worldprice[(prices$price*5)<prices$worldprice]/5
  names(prices)[1:2] <- c("Country.Code","Country")
  
  save(prices, file=paste0("./data/yearly/",year,"_prices.RData"))
  
}
