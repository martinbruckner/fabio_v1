##############################################################################################
##  
##  FABIO: BUILD MRIO TABLE BASED ON FAOSTAT COMMODITY BALANCE SHEETS AND TRADE DATA
##    1b Prepare CBS and BTD data sets
##  
##############################################################################################

library(reshape2)
library(data.table)

rm(list=ls()); gc()
##########################################################################
# Make intitial settings
##########################################################################
# read region classification
reg <- read.csv(file="./inst/fabio_input/Regions_all.csv", header=TRUE, sep=";")
# read commodity classification
items <- read.csv(file="./inst/fabio_input/Items.csv", header=TRUE, sep=";")

# Read data
load(file="/mnt/nfs_fineprint/tmp/fabio/data/CBS.RData")
load(file="/mnt/nfs_fineprint/tmp/fabio/data/BTD.RData")
load(file="/mnt/nfs_fineprint/tmp/fabio/data/Forestry.RData")
load(file="/mnt/nfs_fineprint/tmp/fabio/data/ForTrade.RData")   # 1997-2013
load(file="/mnt/nfs_fineprint/tmp/fabio/data/Prod.RData")
load(file="/mnt/nfs_fineprint/tmp/fabio/data/Lvst.RData")
load(file="/mnt/nfs_fineprint/tmp/fabio/data/Prod_lvst.RData")
load(file="/mnt/nfs_fineprint/tmp/fabio/data/ProdEthanol.RData")
load(file="/mnt/nfs_fineprint/tmp/fabio/data/FishProd.RData")   # 1950-2015
load(file="/mnt/nfs_fineprint/tmp/fabio/data/comtrade.RData")   # 1988-1994
load(file="/mnt/nfs_fineprint/tmp/fabio/data/BACI.RData")       # 1995-2015

CBS_all <- CBS
BTD_all <- BTD
Forestry_all <- Forestry
ForTrade_all <- ForTrade
Prod_all <- Prod
Lvst_all <- Lvst
Prod_lvst_all <- Prod_lvst
ProdEthanol_all <- ProdEthanol
FishProd_all <- FishProd
comtrade_all <- comtrade
BACI_all <- BACI


##########################################################################
# Start loop for a series of years
##########################################################################
# year=1986
year=2013
for(year in 1986:2013){
  print(year)
  ##########################################################################
  # extract data for the defined year
  CBS <- CBS_all[CBS_all$Year==year,] 
  BTD <- BTD_all[BTD_all$Year==year,]
  Forestry <- Forestry_all[Forestry_all$Year==year,]
  if(year<1997){
    ForTrade <- ForTrade_all[ForTrade_all$Year==1997,]   # use trade shares for the year 1997 for all years prior to 1997 (no data available)
    ForTrade$Year <- year
  } else ForTrade <- ForTrade_all[ForTrade_all$Year==year,]
  ForTrade <- ForTrade[ForTrade$Reporter.Country.Code %in% CBS$Country.Code[CBS$Year==year] | 
                         ForTrade$Partner.Country.Code %in% CBS$Country.Code[CBS$Year==year],]
  Prod <- Prod_all[Prod_all$Year==year,]
  Lvst <- Lvst_all[Lvst_all$Year==year,]
  Prod_lvst <- Prod_lvst_all[Prod_lvst_all$Year==year,]
  ProdEthanol <- ProdEthanol_all[ProdEthanol_all$Year==year,]
  FishProd <- FishProd_all[FishProd_all$Year==year,]
  if(year<1988){
    comtrade <- comtrade_all[comtrade_all$Year==1988,]   # use trade shares for the year 1988 for all years prior to 1988 (no data available)
    comtrade$Year <- year
    BACI <- BACI_all[0,]
  } else if(year %in% 1988:1994){
    comtrade <- comtrade_all[comtrade_all$Year==year,]
    BACI <- BACI_all[0,]
  } else {
    BACI <- BACI_all[BACI_all$Year==year,]
    comtrade <- comtrade_all[0,]
  }
  regions <- reg
  # regions <- reg[reg[,1] %in% unique(c(CBS$Country.Code, BTD$Reporter.Country.Code, BTD$Partner.Country.Code, 
  #                                      Forestry$Country.Code, ForTrade$Reporter.Country.Code, ForTrade$Partner.Country.Code)), ]
  ##########################################################################
  
  
  ##########################################################################
  # Prepare trade data for a specific year
  ##########################################################################
  # Prepare BTD 
  #-------------------------------------------------------------------------
  # Change data structure from Reporting & Partner country to Receiving & Supplying country
  #---#---#---#---#---#---#---#---#---#---
  Imports <- BTD[BTD$Element=="Import",]
  Imports$From.Country.Code <- Imports$Partner.Country.Code
  Imports$From.Country <- Imports$Partner.Country
  Imports$To.Country.Code <- Imports$Reporter.Country.Code
  Imports$To.Country <- Imports$Reporter.Country
  Imports$Reporter <- "importer"
  Exports <- BTD[BTD$Element=="Export",]
  Exports$From.Country.Code <- Exports$Reporter.Country.Code
  Exports$From.Country <- Exports$Reporter.Country
  Exports$To.Country.Code <- Exports$Partner.Country.Code
  Exports$To.Country <- Exports$Partner.Country
  Exports$Reporter <- "exporter"
  BTD <- rbind(Imports,Exports)
  BTD <- BTD[,c(12:15,5:7,16,9:11)]
  rm(Imports,Exports)
  #---#---#---#---#---#---#---#---#---#---
  # Give preference to flows reported as Imports before Exports
  #---#---#---#---#---#---#---#---#---#---
  BTD$ID <- paste(BTD$From.Country.Code,BTD$To.Country.Code,BTD$Item.Code,BTD$Year)
  Imports <- BTD[BTD$Reporter=="importer",]
  Exports <- BTD[BTD$Reporter=="exporter",]
  Exports <- Exports[! Exports$ID %in% Imports$ID,]
  BTD <- rbind(Imports,Exports)
  BTD$ID <- NULL
  rm(Imports,Exports)
  # Exclude intra-region trade flows
  BTD <- BTD[! BTD$From.Country.Code==BTD$To.Country.Code,]
  ##########################################################################
  # Prepare ForTrade
  #---#---#---#---#---#---#---#---#---#---
  # Change data structure from Reporting & Partner country to Receiving & Supplying country
  #---#---#---#---#---#---#---#---#---#---
  Imports <- ForTrade[!ForTrade$Import==0,]
  Imports$From.Country.Code <- Imports$Partner.Country.Code
  Imports$From.Country <- Imports$Partner.Countries
  Imports$To.Country.Code <- Imports$Reporter.Country.Code
  Imports$To.Country <- Imports$Reporter.Countries
  Imports$Reporter <- "importer"
  Imports$Export <- NULL
  names(Imports)[8] <- "Value"
  Exports <- ForTrade[!ForTrade$Export==0,]
  Exports$From.Country.Code <- Exports$Reporter.Country.Code
  Exports$From.Country <- Exports$Reporter.Countries
  Exports$To.Country.Code <- Exports$Partner.Country.Code
  Exports$To.Country <- Exports$Partner.Countries
  Exports$Reporter <- "exporter"
  Exports$Import <- NULL
  names(Exports)[8] <- "Value"
  ForTrade <- rbind(Imports,Exports)
  ForTrade <- ForTrade[,c(9:12,5:7,13,8)]
  rm(Imports,Exports)
  #---#---#---#---#---#---#---#---#---#---
  # Give preference to flows reported as Imports before Exports
  #---#---#---#---#---#---#---#---#---#---
  ForTrade$ID <- paste(ForTrade$From.Country.Code,ForTrade$To.Country.Code,ForTrade$Item.Code,ForTrade$Year)
  Imports <- ForTrade[ForTrade$Reporter=="importer",]
  Exports <- ForTrade[ForTrade$Reporter=="exporter",]
  Exports <- Exports[! Exports$ID %in% Imports$ID,]
  ForTrade <- rbind(Imports,Exports)
  ForTrade$ID <- NULL
  rm(Imports,Exports)
  # Exclude intra-region trade flows
  ForTrade <- ForTrade[! ForTrade$From.Country.Code==ForTrade$To.Country.Code,]
  #---#---#---#---#---#---#---#---#---#---
  # Harmonize commodities with Forestry data
  #---#---#---#---#---#---#---#---#---#---
  # ForTrade items:
  # 1651  Ind Rwd Wir (C)
  # 1670  Ind Rwd Wir (NC) Other
  # 1657  Ind Rwd Wir (NC) Tropica
  # Forestry items:
  # 1864  Wood Fuel
  # 1866  Industrial Roundwood(C)
  # 1867	Industrial Roundwood(NC)
  ForTrade$Item <- as.character(ForTrade$Item)
  ForTrade$Item[ForTrade$Item.Code==1651] <- "Industrial Roundwood, coniferous"
  ForTrade$Item.Code[ForTrade$Item.Code==1651] <- 1866
  ForTrade$Item[ForTrade$Item.Code==1670] <- "Industrial roundwood, non-coniferous"
  ForTrade$Item.Code[ForTrade$Item.Code==1670] <- 1867
  ForTrade$Item[ForTrade$Item.Code==1657] <- "Industrial roundwood, non-coniferous"
  ForTrade$Item.Code[ForTrade$Item.Code==1657] <- 1867
  ForTrade$Item <- as.factor(ForTrade$Item)
  # aggregate
  ForTrade <- aggregate(Value ~ From.Country.Code + From.Country + To.Country.Code + To.Country + Item.Code + Item + 
                          Year, ForTrade, sum)
  ForTrade$Reporter <- ""
  ForTrade$tHead <- 0
  ForTrade$tUSD <- 0
  ForTrade <- ForTrade[,c(1:7,9,10,8,11)]
  names(ForTrade) <- names(BTD)
  BTD <- rbind(BTD, ForTrade)
  rm(ForTrade)
  ################################################################################################################################
  # Estimate missing kg and head based on prices
  #-------------------------------------------------------------------------
  item="Cattle"
  for(item in unique(BTD$Item)){
    if(nrow(BTD[BTD$tonnes > 0 & BTD$tUSD > 0 & BTD$Item==item,])>0){
      Price <- aggregate(cbind(tUSD, tonnes) ~ Year, BTD[BTD$tonnes > 0 & BTD$tUSD > 0 & BTD$Item==item,], sum)
      Price$perkg <- Price$tUSD / Price$tonnes # USD per kg
      BTD$tonnes[BTD$tonnes==0 & BTD$Item==item] <- BTD$tUSD[BTD$tonnes==0 & BTD$Item==item] / Price$perkg
    } else if(nrow(BTD[BTD$tHead > 0 & BTD$tUSD > 0 & BTD$Item==item,])>0){
      Price <- aggregate(cbind(tUSD, tHead) ~ Year, BTD[BTD$tHead > 0 & BTD$tUSD > 0 & BTD$Item==item,], sum)
      Price$perhead <- Price$tUSD / Price$tHead # USD per head
      BTD$tHead[BTD$tHead==0 & BTD$Item==item] <- BTD$tUSD[BTD$tHead==0 & BTD$Item==item] / Price$perhead
    }
  }
  ################################################################################################################################
  # Prepare BTDEthanol
  #-------------------------------------------------------------------------
  # Change data structure from Reporting & Partner country to Receiving & Supplying country
  #---#---#---#---#---#---#---#---#---#---
  if(year<1995){
    BTDEthanol <- comtrade[comtrade$commodity_code=="2207",]
    Imports <- BTDEthanol[BTDEthanol$Element=="Import",]
    Imports$From.Country.Code <- Imports$Partner.Country.Code
    Imports$From.Country <- Imports$Partner.Country
    Imports$To.Country.Code <- Imports$Reporter.Country.Code
    Imports$To.Country <- Imports$Reporter.Country
    Imports$Reporter <- "importer"
    Exports <- BTDEthanol[BTDEthanol$Element=="Export",]
    Exports$From.Country.Code <- Exports$Reporter.Country.Code
    Exports$From.Country <- Exports$Reporter.Country
    Exports$To.Country.Code <- Exports$Partner.Country.Code
    Exports$To.Country <- Exports$Partner.Country
    Exports$Reporter <- "exporter"
    BTDEthanol <- rbind(Imports,Exports)
    BTDEthanol <- BTDEthanol[,c(12:16,5,9:11)]
    rm(Imports,Exports)
    #---#---#---#---#---#---#---#---#---#---
    # Harmonize BTD data
    # a) Give preference to flows reported as Imports before Exports
    #---#---#---#---#---#---#---#---#---#---
    # BTDEthanol$ID <- paste(BTDEthanol$From.Country.Code,BTDEthanol$To.Country.Code,BTDEthanol$Year)
    # Imports <- BTDEthanol[BTDEthanol$Reporter=="importer",]
    # Exports <- BTDEthanol[BTDEthanol$Reporter=="exporter",]
    # Exports <- Exports[! Exports$ID %in% Imports$ID,]
    # BTDEthanol <- rbind(Imports,Exports)
    # BTDEthanol$ID <- NULL
    # rm(Imports,Exports)
    # # Exclude intra-region trade flows
    # BTDEthanol <- BTDEthanol[! BTDEthanol$From.Country.Code==BTDEthanol$To.Country.Code,]
    #---#---#---#---#---#---#---#---#---#---
    # Harmonize BTD data
    # b) Give preference to flows reported as Imports before Exports
    #---#---#---#---#---#---#---#---#---#---
    BTDEthanol$ID <- paste(BTDEthanol$From.Country.Code,BTDEthanol$To.Country.Code,BTDEthanol$Year)
    # # preserve exports reported by the US (231)
    # US <- BTDEthanol[BTDEthanol$Reporter=="exporter" & BTDEthanol$From.Country.Code==231,]
    # BTDEthanol <- BTDEthanol[! BTDEthanol$ID %in% US$ID,]
    # BTDEthanol <- rbind(US,BTDEthanol)
    # rm(US)
    Imports <- BTDEthanol[BTDEthanol$Reporter=="importer",]
    Exports <- BTDEthanol[BTDEthanol$Reporter=="exporter",]
    Exports <- Exports[! Exports$ID %in% Imports$ID,]
    BTDEthanol <- rbind(Imports,Exports)
    BTDEthanol$ID <- NULL
    rm(Imports,Exports)
    # Exclude intra-region trade flows
    BTDEthanol <- BTDEthanol[! BTDEthanol$From.Country.Code==BTDEthanol$To.Country.Code,]
    #---#---#---#---#---#---#---#---#---#---
    # fill gaps in Tonnes
    #---#---#---#---#---#---#---#---#---#---
    # estimate missing kg values based on density (0.7893 kg/l, source: https://en.wikipedia.org/wiki/Ethanol)
    BTDEthanol$kg[is.na(BTDEthanol$kg) & !is.na(BTDEthanol$litres)] <- 
      BTDEthanol$litres[is.na(BTDEthanol$kg) & !is.na(BTDEthanol$litres)] * 0.7893
    # calculate prices
    Price <- aggregate(cbind(usd, litres, kg) ~ Year, BTDEthanol[BTDEthanol$kg > 0 & BTDEthanol$usd > 0,], sum)
    Price$perkg <- Price$usd / Price$kg # USD per kg
    Price$perlitre <- Price$usd / Price$litres # USD per litre
    # estimate missing kg and l values based on prices
    BTDEthanol$kg[is.na(BTDEthanol$kg)] <- BTDEthanol$usd[is.na(BTDEthanol$kg)] / Price$perkg
    BTDEthanol$litres[is.na(BTDEthanol$litres)] <- BTDEthanol$usd[is.na(BTDEthanol$litres)] / Price$perlitre
    rm(Price)
    BTDEthanol$litres <- NULL
    BTDEthanol$kg <- BTDEthanol$kg / 1000
    BTDEthanol$usd <- BTDEthanol$usd / 1000
    names(BTDEthanol)[7:8] <- c("tUSD", "tonnes")
    #---#---#---#---#---#---#---#---#---#---
    # add BTDEthanol to BTD
    #---#---#---#---#---#---#---#---#---#---
    BTDEthanol$Item.Code <- 2659
    BTDEthanol$Item <- "Alcohol, Non-Food"
    BTDEthanol$tHead <- 0
    BTDEthanol <- BTDEthanol[,c(1:4,9:10,6,5,11,8,7)]
    # names(BTDEthanol) <- names(BTD)
  } else {
    BTDEthanol <- BACI[substr(BACI$hs6,1,4)=="2207",]
    BTDEthanol$Item.Code <- 2659
    BTDEthanol$Item <- "Alcohol, Non-Food"
    BTDEthanol$Reporter <- "importer"
    BTDEthanol$tHead <- 0
    BTDEthanol <- BTDEthanol[,c(1:4,9:10,5,11,12,8,7)]
    # estimate missing kg values based on prices
    Price <- aggregate(cbind(tUSD, tonnes) ~ Year, BTDEthanol[!is.na(BTDEthanol$tonnes) & BTDEthanol$tUSD > 0,], sum)
    Price$perkg <- Price$tUSD / Price$tonnes # USD per kg
    BTDEthanol$tonnes[is.na(BTDEthanol$tonnes)] <- BTDEthanol$tUSD[is.na(BTDEthanol$tonnes)] / Price$perkg
  }
  BTD <- rbind(BTD, BTDEthanol)
  rm(BTDEthanol)
  ################################################################################################################################
  # Prepare BTDFish
  #-------------------------------------------------------------------------
  # Change data structure from Reporting & Partner country to Receiving & Supplying country
  #---#---#---#---#---#---#---#---#---#---
  if(year<1995){
    BTDFish <- comtrade[comtrade$commodity_code %in% c("0301","0302","0303","0304"),]
    Imports <- BTDFish[BTDFish$Element=="Import",]
    Imports$From.Country.Code <- Imports$Partner.Country.Code
    Imports$From.Country <- Imports$Partner.Country
    Imports$To.Country.Code <- Imports$Reporter.Country.Code
    Imports$To.Country <- Imports$Reporter.Country
    Imports$Reporter <- "importer"
    Exports <- BTDFish[BTDFish$Element=="Export",]
    Exports$From.Country.Code <- Exports$Reporter.Country.Code
    Exports$From.Country <- Exports$Reporter.Country
    Exports$To.Country.Code <- Exports$Partner.Country.Code
    Exports$To.Country <- Exports$Partner.Country
    Exports$Reporter <- "exporter"
    BTDFish <- rbind(Imports,Exports)
    BTDFish <- BTDFish[,c(12:16,5,9:11)]
    rm(Imports,Exports)
    #---#---#---#---#---#---#---#---#---#---
    # Harmonize BTD data
    # b) Give preference to flows reported as Imports before Exports
    #---#---#---#---#---#---#---#---#---#---
    BTDFish$ID <- paste(BTDFish$From.Country.Code,BTDFish$To.Country.Code,BTDFish$Year)
    Imports <- BTDFish[BTDFish$Reporter=="importer",]
    Exports <- BTDFish[BTDFish$Reporter=="exporter",]
    Exports <- Exports[! Exports$ID %in% Imports$ID,]
    BTDFish <- rbind(Imports,Exports)
    BTDFish$ID <- NULL
    BTDFish$litres <- NULL
    rm(Imports,Exports)
    # Exclude intra-region trade flows
    BTDFish <- BTDFish[! BTDFish$From.Country.Code==BTDFish$To.Country.Code,]
    #---#---#---#---#---#---#---#---#---#---
    # fill gaps in Tonnes
    #---#---#---#---#---#---#---#---#---#---
    # calculate price and density
    Price <- aggregate(cbind(usd, kg) ~ Year, BTDFish[BTDFish$kg > 0 & BTDFish$usd > 0,], sum)
    Price$perkg <- Price$usd / Price$kg # USD per kg
    # estimate missing kg and l values based on prices
    BTDFish$kg[is.na(BTDFish$kg)] <- BTDFish$usd[is.na(BTDFish$kg)] / Price$perkg
    rm(Price)
    BTDFish$kg <- BTDFish$kg / 1000
    BTDFish$usd <- BTDFish$usd / 1000
    names(BTDFish)[7:8] <- c("tUSD", "tonnes")
    #---#---#---#---#---#---#---#---#---#---
    # add BTDFish to BTD
    #---#---#---#---#---#---#---#---#---#---
    BTDFish$Item.Code <- 2960
    BTDFish$Item <- "Fish, Seafood"
    BTDFish$tHead <- 0
    BTDFish <- BTDFish[,c(1:4,9:10,6,5,11,8,7)]
    # names(BTDFish) <- names(BTD)
  } else {
    BTDFish <- BACI[substr(BACI$hs6,1,3) %in% c(301,302,303,304),]
    BTDFish$Item.Code <- 2960
    BTDFish$Item <- "Fish, Seafood"
    BTDFish$Reporter <- "importer"
    BTDFish$tHead <- 0
    BTDFish <- BTDFish[,c(1:4,9:10,5,11,12,8,7)]
    # estimate missing kg values based on prices
    Price <- aggregate(cbind(tUSD, tonnes) ~ Year, BTDFish[!is.na(BTDFish$tonnes) & BTDFish$tUSD > 0,], sum)
    Price$perkg <- Price$tUSD / Price$tonnes # USD per kg
    BTDFish$tonnes[is.na(BTDFish$tonnes)] <- BTDFish$tUSD[is.na(BTDFish$tonnes)] / Price$perkg
  }
  BTD <- rbind(BTD, BTDFish)
  rm(BTDFish)
  save(BTD, file = paste0("/mnt/nfs_fineprint/tmp/fabio/data/yearly/",year,"_BTD.RData"))
  
  
  ##########################################################################
  # Prepare CBS for a specific year
  #-------------------------------------------------------------------------
  # Exclude all items that are not used in the FAO-MRIO (particularly fish and aggregate items)
  CBS <- CBS[CBS$Item.Code %in% items$Item.Code,]
  ##########################################################################
  # Prepare Forestry data for a specific year and add to CBS
  #-------------------------------------------------------------------------
  # Change structure of Forestry to be consistent with CBS
  #-------------------------------------------------------------------------
  Forestry$StockVariation <- 0
  Forestry$TotalSupply <- Forestry$Production + Forestry$Import
  Forestry <- Forestry[,c(1:5,8,7,9:10,6)]
  Forestry$Food <- 0
  Forestry$Feed <- 0
  Forestry$Processing <- 0
  Forestry$Seed <- 0
  Forestry$Waste <- 0
  Forestry$OtherUses <- Forestry$Production + Forestry$Import - Forestry$Export
  names(Forestry) <- names(CBS)
  CBS <- rbind(CBS, Forestry)
  rm(Forestry)
  ##########################################################################
  # Estimate CBS for non-CBS commodities (crops and livestock)
  #-------------------------------------------------------------------------
  addCBS <- data.frame(ID="",
                       Country.Code=rep(regions[,1], 20),
                       Country=rep(regions[,2], 20),
                       Item.Code=as.vector(sapply(c(328, 254, 677, 2000, 2001, 866, 946, 976, 1016, 1034, 2029, 1096, 
                                                    1107, 1110, 1126, 1157, 1140, 1150, 1171, 843), 
                                                  function (x) rep(x,length(regions[,1])))),
                       Item=as.vector(sapply(c("Seed cotton", "Oil, palm fruit", "Hops", "Fodder crops", "Grazing", "Cattle", "Buffaloes",
                                               "Sheep", "Goats", "Pigs", "Poultry Birds", "Horses", "Asses", "Mules", "Camels",
                                               "Camelids, other", "Rabbits and hares", "Rodents, other", "Live animals, other", "Pet food"), 
                                             function (x) rep(x,length(regions[,1])))),
                       Year=year, Production=0, Imports=0, StockVariation=0, TotalSupply=0, Exports=0, Food=0, Feed=0, Processing=0, 
                       Seed=0, Waste=0, OtherUses=0, Balancing=0)
  addCBS$ID <- paste(addCBS$Country.Code, addCBS$Item.Code, addCBS$Year)
  # write crop production into addCBS
  temp <- Prod[Prod$Element=="Production",c(1:4,7,9)]
  temp$ID <- paste(temp$Country.Code, temp$Item.Code, temp$Year)
  temp <- temp[temp$Country.Code %in% regions$Country.Code,]
  for(id in temp$ID){
    addCBS$Production[addCBS$ID==id] <- temp$Value[temp$ID==id]
  }
  # write crop seed use into addCBS
  temp <- Prod[Prod$Element=="Seed",c(1:4,7,9)]
  temp$ID <- paste(temp$Country.Code, temp$Item.Code, temp$Year)
  temp <- temp[temp$Country.Code %in% regions$Country.Code,]
  for(id in temp$ID){
    addCBS$Seed[addCBS$ID==id] <- temp$Value[temp$ID==id]
  }
  # # write livestock processing numbers (slaughtered animals) into addCBS
  # # note: processing is calculated below by adding production + imports - exports, i.e. total supply - exports
  # temp <- Prod_lvst[Prod_lvst$Element=="Producing Animals/Slaughtered",]
  # temp$Value[temp$Unit=="Head"] <- temp$Value[temp$Unit=="Head"] / 1000
  # temp$Unit[temp$Unit=="Head"] <- "1000 Head"
  # temp <- temp[,c(1,3,7,9)]
  # temp$Item.Code <- temp$Item.Code - 1 
  # temp$Item.Code[temp$Item.Code==1807] <- 2029
  # temp$ID <- paste(temp$Country.Code, temp$Item.Code, temp$Year)
  # for(id in temp$ID){
  #   addCBS$Processing[addCBS$ID==id] <- temp$Value[temp$ID==id]
  # }
  # write livestock production numbers (meat indigenous + exports) into addCBS
  # here only meat indigenous is considered; exports are added later
  temp <- Prod_lvst[Prod_lvst$Element=="Production" & Prod_lvst$Unit %in% c("Head", "1000 Head"),]
  temp$Value[temp$Unit=="Head"] <- temp$Value[temp$Unit=="Head"] / 1000
  temp$Unit[temp$Unit=="Head"] <- "1000 Head"
  concordance <- data.frame(Item.Code.lvst = c(1137,944,1032,1012,1775,1055,1120,1144,972,1161,1154,1122,1124),
                            Item.lvst = c("Meat indigenous, camel","Meat indigenous, cattle","Meat indigenous, goat","Meat indigenous, sheep",
                                          "Meat indigenous, poultry","Meat indigenous, pig","Meat indigenous, horse","Meat indigenous, rabbit",
                                          "Meat indigenous, buffalo","Meat indigenous, other camelids","Meat indigenous, rodents","Meat indigenous, ass",
                                          "Meat indigenous, mule"),
                            Item.Code = c(1126,866,1016,976,2029,1034,1096,1140,946,1157,1150,1107,1110),
                            Item = c("Camels","Cattle","Goats","Sheep","Poultry Birds","Pigs","Horses","Rabbits and hares","Buffaloes",
                                     "Camelids, other","Rodents, other","Asses","Mules"))
  names(temp)[3:4] <- names(concordance)[1:2]
  temp <- merge(temp, concordance[,-2], by="Item.Code.lvst", all.x = T)
  temp <- temp[!is.na(temp$Item.Code),]
  temp <- temp[,c(10,2,7,9)]
  temp$ID <- paste(temp$Country.Code, temp$Item.Code, temp$Year)
  for(id in temp$ID){
    addCBS$Production[addCBS$ID==id] <- temp$Value[temp$ID==id]
  }
  #---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---
  # write pet food production into addCBS
  # !!!!!! NOT IMPLEMENTED !!!!!! 
  #---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---
  # the only available data source gives pet food production quantities for only 8 countries without specifying the type of pet food (i.e. meat or cereal feed)
  # http://www.alltech.com/sites/default/files/global-feed-survey-2015.pdf
  # petfood <- data.frame(Production=c(400000,9000000,2000000,1000000,500000,500000,300000,1100000), 
  #                       Country=c("China, mainland","United States of America","Brazil","Mexico","Spain","Russian Federation","Japan","France"))
  # Check meet availability (feed and other use) for the production of pet food:
  # write.csv(CBS[CBS$Item.Code %in% c(2731,2732,2733,2734,2735,2736,2737,2848,2740,2744,2745,2746,2747,2748,2749),], file="CBSmeat.csv")
  # conclusion: numbers do not match with the production figures above, there are massive gaps, thus no meaningful way of using these data together
  #
  #---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---
  # write trade data from BTD into addCBS
  #---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---
  # imports of fodder crops, hops and pet food
  temp <- aggregate(tonnes ~ To.Country.Code + To.Country + Item.Code + Item + Year, BTD[BTD$Item.Code %in% addCBS$Item.Code,], sum)
  temp <- temp[!temp$tonnes==0,]
  temp$ID <- paste(temp$To.Country.Code, temp$Item.Code, temp$Year)
  for(id in temp$ID){
    addCBS$Imports[addCBS$ID==id] <- temp$tonnes[temp$ID==id]
  }
  # exports of fodder crops, hops and pet food
  temp <- aggregate(tonnes ~ From.Country.Code + From.Country + Item.Code + Item + Year, BTD[BTD$Item.Code %in% addCBS$Item.Code,], sum)
  temp <- temp[!temp$tonnes==0,]
  temp$ID <- paste(temp$From.Country.Code, temp$Item.Code, temp$Year)
  for(id in temp$ID){
    addCBS$Exports[addCBS$ID==id] <- temp$tonnes[temp$ID==id]
  }
  # livestock imports
  temp <- aggregate(tHead ~ To.Country.Code + To.Country + Item.Code + Item + Year, BTD[BTD$Item.Code %in% addCBS$Item.Code,], sum)
  temp <- temp[!temp$tHead==0,]
  temp$ID <- paste(temp$To.Country.Code, temp$Item.Code, temp$Year)
  for(id in temp$ID){
    addCBS$Imports[addCBS$ID==id] <- temp$tHead[temp$ID==id]
  }
  # livestock exports
  temp <- aggregate(tHead ~ From.Country.Code + From.Country + Item.Code + Item + Year, BTD[BTD$Item.Code %in% addCBS$Item.Code,], sum)
  temp <- temp[!temp$tHead==0,]
  temp$ID <- paste(temp$From.Country.Code, temp$Item.Code, temp$Year)
  for(id in temp$ID){
    addCBS$Exports[addCBS$ID==id] <- temp$tHead[temp$ID==id]
    # add exports to production (see above)
    addCBS$Production[addCBS$ID==id] <- addCBS$Production[addCBS$ID==id] + temp$tHead[temp$ID==id]
  }
  #---#---#---#---#---#---#---#---#---#---
  # Allocate supply (Prod + Imp - Exp) to Uses
  #---#---#---#---#---#---#---#---#---#---
  # avoid negatives in production and trade
  addCBS[addCBS[,6:16]<0, 6:16] <- 0
  # calculate total supply
  addCBS$TotalSupply <- addCBS$Production + addCBS$Imports
  # Estimate pet food production
  addCBS$Production[addCBS$Exports>(addCBS$TotalSupply) & addCBS$Item.Code==843] <- 
    rowSums(addCBS[addCBS$Exports>(addCBS$TotalSupply) & addCBS$Item.Code==843, 10:17]) - 
    addCBS$TotalSupply[addCBS$Exports>(addCBS$TotalSupply) & addCBS$Item.Code==843]
  # recalculate total supply
  addCBS$TotalSupply <- addCBS$Production + addCBS$Imports
  # Reduce exports where they surpass total supply
  addCBS$Exports[addCBS$Exports>addCBS$TotalSupply] <- addCBS$TotalSupply[addCBS$Exports>addCBS$TotalSupply]
  # Balance the new CBS, where exports are bigger than total supply (prod + imp)
  addCBS$Balancing[addCBS$Exports>(addCBS$TotalSupply)] <- 
    addCBS$TotalSupply[addCBS$Exports>(addCBS$TotalSupply)] - 
    rowSums(addCBS[addCBS$Exports>(addCBS$TotalSupply), 11:17])
  # Items in addCBS:
  # Item.Code=c(328, 254, 677, 2000, 2001, 866, 946, 976, 1016, 1034, 2029, 1096, 1107, 1110, 1126, 1157, 1140, 1150, 1171, 843)
  # Item=c("Seed cotton", "Oil, palm fruit", "Hops", "Fodder crops", "Grazing", "Cattle", "Buffaloes",
  #                      "Sheep", "Goats", "Pigs", "Poultry Birds", "Horses", "Asses", "Mules", "Camels",
  #                      "Camelids, other", "Rabbits", "Rodents, other", "Live animals, other", "Pet food")
  # For cotton, oil palm, hops and live animals: Processing use = Production + Imports - Exports - Balancing
  commodities <- c(328, 254, 677, 866, 946, 976, 1016, 1034, 2029, 1096, 1107, 1110, 1126, 1157, 1140, 1150, 1171)
  addCBS$Processing[addCBS$Item.Code %in% commodities] <- addCBS$TotalSupply[addCBS$Item.Code %in% commodities] - 
    rowSums(addCBS[addCBS$Item.Code %in% commodities, 11:18])
  # For pet food: Food use = Production + Imports - Exports - Balancing
  addCBS$Food[addCBS$Item.Code==843] <- addCBS$TotalSupply[addCBS$Item.Code==843] - 
    rowSums(addCBS[addCBS$Item.Code==843, 11:18])
  # For fodder crops: Feed use = Production + Imports - Exports - Balancing
  addCBS$Feed[addCBS$Item.Code==2000] <- addCBS$TotalSupply[addCBS$Item.Code==2000] - 
    rowSums(addCBS[addCBS$Item.Code==2000, 11:18])
  #---#---#---#---#---#---#---#---#---#---
  # integrate addCBS into existing CBS data.frame
  #---#---#---#---#---#---#---#---#---#---
  addCBS$ID <- NULL
  addCBS <- addCBS[!rowSums(addCBS[,-(1:5)])==0,]
  CBS$Balancing <- 0
  CBS <- rbind(CBS,addCBS)
  rm(addCBS, commodities)
  ##########################################################################
  # Prepare CBS for Ethanol (= Non-food Alcohol)
  #---#---#---#---#---#---#---#---#---#---
  ethanolCBS <- CBS[CBS$Item.Code==2659,]
  # replace production data
  ethanolCBS <- merge(ethanolCBS, ProdEthanol[,c(1,4)], by="Country.Code", all=TRUE)
  ethanolCBS <- ethanolCBS[ethanolCBS$Country.Code %in% regions$Country.Code, ]
  ethanolCBS[,-(1:5)][is.na(ethanolCBS[,-(1:5)])] <- 0
  # use EIA production values, where ethanol production is not (or under-) reported by FAO's CBS
  # but stick to FAO wherever possible
  ethanolCBS$Production.x[ethanolCBS$Production.x < ethanolCBS$Production.y] <- ethanolCBS$Production.y[ethanolCBS$Production.x < ethanolCBS$Production.y]
  ethanolCBS$Production.y <- NULL
  # calculate national aggregates for imports and exports
  exports <- aggregate(tonnes ~ From.Country.Code + From.Country, BTD[BTD$Item.Code==2659,c(1,2,10)], sum)
  imports <- aggregate(tonnes ~ To.Country.Code + To.Country, BTD[BTD$Item.Code==2659,c(3,4,10)], sum)
  ethanolCBS <- merge(ethanolCBS, exports[,c(1,3)], by.x="Country.Code", by.y="From.Country.Code", all=TRUE)
  ethanolCBS <- merge(ethanolCBS, imports[,c(1,3)], by.x="Country.Code", by.y="To.Country.Code", all=TRUE)
  ethanolCBS$Imports <- ethanolCBS$tonnes.y
  ethanolCBS$Exports <- ethanolCBS$tonnes.x
  ethanolCBS$tonnes.x <- NULL
  ethanolCBS$tonnes.y <- NULL
  ethanolCBS <- ethanolCBS[!is.na(ethanolCBS$Item),]
  ethanolCBS[,-(1:5)][is.na(ethanolCBS[,-(1:5)])] <- 0
  ethanolCBS$Item.Code <- ethanolCBS$Item.Code[1]
  ethanolCBS$Item <- ethanolCBS$Item[1]
  ethanolCBS$Year <- year
  ethanolCBS <- merge(ethanolCBS, reg[,1:2], by="Country.Code", all.x=TRUE)
  ethanolCBS$Country.x <- ethanolCBS$Country.y
  ethanolCBS$Country.y <- NULL
  names(ethanolCBS)[2] <- "Country"
  names(ethanolCBS)[6] <- "Production"
  ethanolCBS$TotalSupply <- ethanolCBS$Production + ethanolCBS$Imports
  ethanolCBS$OtherUses <- ethanolCBS$TotalSupply - ethanolCBS$Exports - ethanolCBS$StockVariation
  # Reduce exports where it surpasses total supply
  ethanolCBS$Exports[ethanolCBS$OtherUses<0] <- ethanolCBS$Exports[ethanolCBS$OtherUses<0] + ethanolCBS$OtherUses[ethanolCBS$OtherUses<0]
  ethanolCBS$OtherUses[ethanolCBS$OtherUses<0] <- 0
  ethanolCBS <- ethanolCBS[!rowSums(ethanolCBS[,-(1:6)])==0,] # delete rows with no values
  #---#---#---#---#---#---#---#---#---#---
  # replace old data in CBS.RData
  #---#---#---#---#---#---#---#---#---#---
  # write.table(ethanolCBS, "ethanolCBS.csv", sep=";")
  CBS <- CBS[!CBS$Item.Code==2659,]
  CBS <- rbind(CBS, ethanolCBS)
  rm(ethanolCBS, ProdEthanol, imports, exports)
  save(CBS, file = paste0("/mnt/nfs_fineprint/tmp/fabio/data/yearly/",year,"_CBS.RData"))
  gc()
}


