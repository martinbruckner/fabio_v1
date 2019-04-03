##############################################################################################
##  
##  FABIO: BUILD MRIO TABLE BASED ON FAOSTAT COMMODITY BALANCE SHEETS AND TRADE DATA
##    1d Consolidate CBS and BTD
##  
##############################################################################################

library(reshape2)
library(data.table)

rm(list=ls()); gc()
##########################################################################
# Make intitial settings
##########################################################################
# read region classification
reg <- read.csv(file="./inst/fabio_input/Regions.csv", header=TRUE, sep=";")
# read commodity classification
items <- read.csv(file="./inst/fabio_input/Items.csv", header=TRUE, sep=";")


##########################################################################
# Start loop for a series of years
##########################################################################
# year=1988
year=2013
for(year in 1986:2013){
  print(year)
  ##########################################################################
  # Read data
  #-------------------------------------------------------------------------
  load(file=paste0("/mnt/nfs_fineprint/tmp/fabio/data/yearly/",year,"_CBS_est.RData"))
  load(file=paste0("/mnt/nfs_fineprint/tmp/fabio/data/yearly/",year,"_BTD.RData"))
  
  # aggregate countries that are not in the region list to RoW
  temp <- CBS[!CBS$Country.Code %in% reg$Country.Code,]
  temp$Country.Code <- 999
  temp$Country <- "RoW"
  temp <- aggregate(. ~ Country.Code + Country + Item.Code + Item + Year, temp, sum)
  temp$Original <- 0
  CBS <- rbind(CBS[CBS$Country.Code %in% reg$Country.Code,], temp)
  
  # remove unspecified (252) and adjustments (254)
  BTD <- BTD[! (BTD$From.Country.Code %in% c(252,254) | BTD$To.Country.Code %in% c(252,254)),]
  
  # replace countries in BTD that are not in CBS by RoW 999
  BTD$To.Country[! BTD$To.Country.Code %in% unique(CBS$Country.Code)] <- "RoW"
  BTD$From.Country[! BTD$From.Country.Code %in% unique(CBS$Country.Code)] <- "RoW"
  BTD$To.Country.Code[! BTD$To.Country.Code %in% unique(CBS$Country.Code)] <- 999
  BTD$From.Country.Code[! BTD$From.Country.Code %in% unique(CBS$Country.Code)] <- 999
  
  # aggregate double entries for RoW
  BTD <- as.data.table(BTD)
  BTD <- data.frame(BTD[, lapply(.SD, sum), by = eval(colnames(BTD)[1:8]), .SDcols = colnames(BTD)[9:11]])
  
  # remove BTD items that are not included in CBS
  BTD <- BTD[BTD$Item.Code %in% CBS$Item.Code,]
  
  # replace negative values in BTD with zero
  BTD[,9:11][BTD[,9:11]<0] <- 0
  
  # replace negative values in CBS with zero and balance supply and use
  CBS[,c(6,7,10:16)][CBS[,c(6,7,10:16)]<0] <- 0 
  # take supply and stock change as given and re-balance uses
  CBS$TotalSupply <- CBS$Production + CBS$Imports
  CBS[,10:16] <- round(CBS[,10:16] / rowSums(CBS[,10:16]) * (CBS$TotalSupply - CBS$StockVariation))
  CBS[is.na(CBS)] <- 0
  
  CBS$Balancing <- CBS$TotalSupply - rowSums(CBS[,c(8,10:16)])
  
  # Allocate supply (Prod + Imp - Exp - Bal) to Uses, where values missing
  # For hops and live animals: Processing use = Production + Imports - Exports - Balancing
  commodities <- c(677, 866, 946, 976, 1016, 1034, 2029, 1096, 1107, 1110, 1126, 1157, 1140, 1150, 1171)
  CBS$Processing[CBS$Item.Code %in% commodities] <- CBS$Processing[CBS$Item.Code %in% commodities] + 
    CBS$Balancing[CBS$Item.Code %in% commodities]
  CBS$Balancing[CBS$Item.Code %in% commodities] <- 0
  # For non-food crops (fibres, tobacco etc.): Other use = Production + Imports - Exports - Balancing
  commodities <- c(2662,2663,2664,2665,2666,2667,2671,2672,2659,2661,2746,2748,2747)
  CBS$OtherUses[CBS$Item.Code %in% commodities] <- CBS$OtherUses[CBS$Item.Code %in% commodities] + 
    CBS$Balancing[CBS$Item.Code %in% commodities]
  CBS$Balancing[CBS$Item.Code %in% commodities] <- 0
  # For feed crops: Feed = Production + Imports - Exports - Balancing - Seed - Processing
  commodities <- c(2536,2537,2555,2559,2544,2590,2591,2592,2593,2594,2595,2596,2597,2598)
  CBS$Feed[CBS$Item.Code %in% commodities] <- CBS$Feed[CBS$Item.Code %in% commodities] + 
    CBS$Balancing[CBS$Item.Code %in% commodities]
  CBS$Balancing[CBS$Item.Code %in% commodities] <- 0
  # For the rest: assume food use: Food = Production + Imports - Exports - Balancing - Seed - Processing
  CBS$Food <- CBS$Food + CBS$Balancing
  CBS$Balancing <- 0
  
  # # Check discrepancies between trade flows reported in CBS and BTD
  # #-------------------------------------------------------------------------
  # region=231    # test region, e.g. USA 231, Belize 23
  # test <- CBS[CBS$Country.Code==region,c(1:7,10)]
  # test$BTDimp <- 0
  # test$BTDexp <- 0
  # for(item in test$Item){
  #   test$BTDexp[test$Item==item] <- sum(BTD$tonnes[BTD$Item==item & BTD$From.Country.Code==region])
  #   test$BTDimp[test$Item==item] <- sum(BTD$tonnes[BTD$Item==item & BTD$To.Country.Code==region])
  # }
  # test$exp <- round(test$BTDexp / test$Exports,2)
  # test$imp <- round(test$BTDimp / test$Imports,2)
  
  # write files
  save(CBS, file = paste0("/mnt/nfs_fineprint/tmp/fabio/data/yearly/",year,"_CBS_cons.RData"))
  save(BTD, file = paste0("/mnt/nfs_fineprint/tmp/fabio/data/yearly/",year,"_BTD_cons.RData"))
  
  # write csv files for balancing in GAMS
  # write.table(CBS[,c(1:5,7,10)], file = paste0("C:/Users/mbruckne/Dropbox/FAOMRIO data/",year,"_CBS_trade.csv"), sep = ";", row.names = FALSE)
  # write.table(CBS[,c(1:5,18)], file = paste0("C:/Users/mbruckne/Dropbox/FAOMRIO data/",year,"_CBS_indicator.csv"), sep = ";", row.names = FALSE)
}
