##############################################################################################
##  
##  FABIO: BUILD MRIO TABLE BASED ON FAOSTAT COMMODITY BALANCE SHEETS AND TRADE DATA
##    1a Prepare raw FAO data
##  
##############################################################################################

library(reshape2)
library(data.table)
library(magrittr)
library(dplyr)

rm(list=ls()); gc()

regions <- read.csv2(file = "Regions_all.csv")
years <- 1986:2013

##########################################################################
# Prepare COMMODITY BALANCE SHEETS (CBS)
##########################################################################
load(file="data/raw data/CBS_raw.RData")
# Transform raw data to pivot table
# Note: without Item.Code this line asks for a fun.aggregate, because there is one element ('eggs') which is included twice in the data set with two different item codes
names(CBS_raw)[1:2] <- c("Country.Code","Country")
CBS <- CBS_raw[CBS_raw$Country.Code<5000,]  # Country.Code >= 5000 = country groups, e.g. 'Africa'
CBS <- as.data.frame(dcast(as.data.table(CBS), Country.Code + Country + Item.Code + Item + Year ~ Element, value.var="Value"))
CBS[is.na(CBS)] <- 0
names(CBS) <- c(names(CBS)[1:5],"TotalSupply","Exports","Feed","Food","Imports","OtherUses","Processing","Production","Seed",
                "StockVariation","Waste")
CBS <- CBS[,c(1,2,3,4,5,13,10,15,6,7,9,8,12,14,16,11)]
# Change supply element from Prod+Imp-Exp to Prod+Imp
CBS$TotalSupply <- CBS$Production + CBS$Imports
# change element StockVariation from showing withdrawals from stock to showing additions to stock
CBS$StockVariation <- -CBS$StockVariation
# reduce additions to stock where they surpass total supply
CBS$StockVariation[CBS$StockVariation > CBS$TotalSupply] <- CBS$TotalSupply[CBS$StockVariation > CBS$TotalSupply]

# avoid negatives (except for stock changes)
CBS[,c(6,7,9:16)][CBS[,c(6,7,9:16)]<0] <- 0
# take supply and stock change as given and re-balance uses
CBS$TotalSupply <- CBS$Production + CBS$Imports
CBS[,10:16] <- CBS[,10:16] / rowSums(CBS[,10:16]) * (CBS$TotalSupply - CBS$StockVariation)
CBS[is.na(CBS)] <- 0

# rename Countries (i.e. Czechia and Netherlands Antilles)
CBS$Country <- regions$Country[match(CBS$Country.Code,regions$Country.Code)]
CBS$Country <- as.character(CBS$Country)
# rename Sudan (former) and Ethopia (PDR)
CBS$Country[CBS$Country.Code==206] <- "Sudan"
CBS$Country.Code[CBS$Country.Code==206] <- 276
CBS$Country[CBS$Country.Code==62] <- "Ethiopia"
CBS$Country.Code[CBS$Country.Code==62] <- 238
# CBS$Country <- as.factor(CBS$Country)

# detect missing years for countries in the regions list, which are missing for less than 6 years
# countries missing longer than 6 years are not estimated by extrapolating
missing <- data.frame(Country.Code = integer(), Year = integer(), stringsAsFactors = FALSE)
for(region in regions$Country.Code){
  if(length(years[! years %in% CBS$Year[CBS$Country.Code==region]])<6){
    for(year in years[! years %in% CBS$Year[CBS$Country.Code==region]]){
      missing[nrow(missing)+1,] <- c(region, year)
    }
  }
}
# add missing years 
region=151
for(region in unique(missing$Country.Code)){
  yearsreg <- missing$Year[missing$Country.Code==region]
  if(min(yearsreg)==1986){
    for(year in min(years):max(yearsreg)){
      addCBS <- CBS[CBS$Country.Code==region & CBS$Year==max(yearsreg)+1,]
      addCBS$Year <- year
      CBS <- rbind(CBS,addCBS)
    }
  } else if(max(yearsreg)==max(years)){
    for(year in min(yearsreg):max(years)){
      addCBS <- CBS[CBS$Country.Code==region & CBS$Year==min(yearsreg)-1,]
      addCBS$Year <- year
      CBS <- rbind(CBS,addCBS)
    }
  }
}
save(CBS, file="data/CBS.RData")
rm(CBS_raw)
gc()  # releases memory


##########################################################################
# Prepare BILATERAL TRADE DATA (BTD)
##########################################################################
load(file="data/raw data/BTD_raw.RData")
# exclude items: "Waters,ice etc", "Cotton waste", "Vitamins", "Hair, goat, coarse", "Beehives", "Beewax", 
#                "Hair fine", "Crude materials", "Waxes vegetable"
# and delete rows with 0 values
BTD <- BTD_raw[!BTD_raw$Item.Code %in% c(631,769,853,1031,1181,1183,1218,1293,1296) & !BTD_raw$Value==0,]
# add element names "Import" and "Export"
BTD$Imex[BTD$Element=="Import Quantity" | BTD$Element=="Import Value"] <- "Import"
BTD$Imex[BTD$Element=="Export Quantity" | BTD$Element=="Export Value"] <- "Export"
BTD$Imex <- as.factor(BTD$Imex)
BTD$Element.Code <- NULL
BTD$Element <- NULL
BTD$Flag <- NULL
BTD$Year.Code <- NULL
rm(BTD_raw)
gc()  # releases memory
#-----------------------------
# convert tonnes to primary equivalents
#-----------------------------
TCF <- read.csv(file="TCF_Trade.csv", header=TRUE, sep=";")
BTD.tonnes <- BTD[BTD$Unit=="tonnes",]
BTD.rest <- BTD[! BTD$Unit=="tonnes",]
# BTD.tonnes <- merge(BTD.tonnes, TCF, by="Item.Code")
# merging data.tables is much faster than merging data.frames:
BTD.tonnes <- data.frame(merge(data.table(BTD.tonnes, key="Item.Code"),data.table(TCF, key="Item.Code")))
BTD.tonnes$Value <- BTD.tonnes$Value / BTD.tonnes$TCF
BTD.tonnes$TCF <- NULL
BTD <- rbind(BTD.tonnes,BTD.rest)
BTD <- BTD[,c(2:5,1,6:10)]
rm(BTD.tonnes,BTD.rest,TCF)
#-----------------------------
# Aggregate to CBS items
#-----------------------------
Items <- read.csv(file="Items_BTD-CBS.csv", header=TRUE, sep=";")[,1:3]
# BTD <- merge(BTD, Item.Code, by="Item.Code")
# BTD <- merge(data.table(BTD, key="Item.Code"), data.table(Item.Code, key="Item.Code"))
BTD$Item.Code <- Items$CBS.Item.Code[match(BTD$Item.Code,Items$Item.Code)]
BTD$Item <- Items$CBS.Item[match(BTD$Item.Code,Items$CBS.Item.Code)]
rm(Items)
# aggregate from BTD items to CBS items (~22 Mio rows -> ~13 Mio rows)
# it's much faster, aggregating a data.table than a data.frame:
BTD <- data.frame(as.data.table(BTD)[,list(val = sum(Value)), by = 'Reporter.Country.Code,Reporter.Countries,Partner.Country.Code,Partner.Countries,Item.Code,Item,Year,Imex,Unit'])
names(BTD)[10] <- "Value"
#-----------------------------
# cast data.frame to get all values in different units for each data point in a single row
#-----------------------------
# BTD[is.na(BTD$Unit),]
BTD <- BTD[!is.na(BTD$Unit),]
BTD <- as.data.frame(dcast(as.data.table(BTD), Reporter.Country.Code + Reporter.Countries + Partner.Country.Code + Partner.Countries + 
               Item.Code + Item + Year + Imex ~ Unit, value.var="Value"))
BTD[is.na(BTD)] <- 0
names(BTD) <- c("Reporter.Country.Code","Reporter.Country","Partner.Country.Code","Partner.Country",
                "Item.Code","Item","Year","Element","tHead","tUSD","Head","tonnes")
BTD <- BTD[,c(1:8,11,9,12,10)]
#-----------------------------
# Fill tHead, where Head>0 and tHead=0
#-----------------------------
BTD$tHead[!BTD$Head==0 & BTD$tHead==0] <- BTD$Head[!BTD$Head==0 & BTD$tHead==0]/1000
BTD$Head <- NULL

# rename Sudan (former) and Ethiopia PRD
BTD$Reporter.Country <- as.character(BTD$Reporter.Country)
BTD$Reporter.Country[BTD$Reporter.Country.Code==206] <- "Sudan"
BTD$Reporter.Country.Code[BTD$Reporter.Country.Code==206] <- 276
BTD$Reporter.Country[BTD$Reporter.Country.Code==62] <- "Ethiopia"
BTD$Reporter.Country.Code[BTD$Reporter.Country.Code==62] <- 238
# BTD$Reporter.Country <- as.factor(BTD$Reporter.Country)
BTD$Partner.Country <- as.character(BTD$Partner.Country)
BTD$Partner.Country[BTD$Partner.Country.Code==206] <- "Sudan"
BTD$Partner.Country.Code[BTD$Partner.Country.Code==206] <- 276
BTD$Partner.Country[BTD$Partner.Country.Code==62] <- "Ethiopia"
BTD$Partner.Country.Code[BTD$Partner.Country.Code==62] <- 238
BTD$Reporter.Country[BTD$Reporter.Country.Code==151] <- "Netherlands Antilles"
BTD$Partner.Country[BTD$Partner.Country.Code==151] <- "Netherlands Antilles"
# BTD$Partner.Country <- as.factor(BTD$Partner.Country)

save(BTD, file="data/BTD.RData")
gc()  # releases memory


##########################################################################
# Prepare FORESTRY DATA
##########################################################################
load(file="data/raw data/Forestry_raw.RData")
names(Forestry_raw)[1:2] <- c("Country.Code","Country")
Forestry <- Forestry_raw[! Forestry_raw$Value==0,] # delete rows with 0 values
# exclude all items except 1864 Wood Fuel and 1866 Industrial Roundwood(C) and 1867 Industrial Roundwood(NC)
Forestry <- Forestry[Forestry$Item.Code %in% c(1864,1866,1867) & Forestry$Country.Code<5000,]
Forestry$Flag <- NULL
Forestry$Year.Code <- NULL
Forestry$Element.Code <- NULL
# exclude USD
Forestry <- Forestry[! Forestry$Unit=="1000 US$",]
# exclude China to avoid double counting with China mainland, Taiwan, etc.
Forestry <- Forestry[!Forestry$Country.Code==351,]
# add element names "Import" and "Export"
Forestry$Element <- as.character(Forestry$Element)
Forestry$Element[Forestry$Element=="Import Quantity"] <- "Import"
Forestry$Element[Forestry$Element=="Export Quantity"] <- "Export"
Forestry$Element <- as.factor(Forestry$Element)
rm(Forestry_raw)
#-----------------------------
# cast data.frame to get the different elements in columns
#-----------------------------
Forestry <- dcast(Forestry, Country.Code + Country + Item.Code + Item + Year ~ Element, value.var="Value")
Forestry[is.na(Forestry)] <- 0

# rename Countries (i.e. Czechia and Netherlands Antilles)
Forestry$Country <- regions$Country[match(Forestry$Country.Code,regions$Country.Code)]
Forestry$Country <- as.character(Forestry$Country)

# rename Sudan (former), Ethiopia PDR and Netherlands Antilles
Forestry$Country[Forestry$Country.Code==206] <- "Sudan"
Forestry$Country.Code[Forestry$Country.Code==206] <- 276
Forestry$Country[Forestry$Country.Code==62] <- "Ethiopia"
Forestry$Country.Code[Forestry$Country.Code==62] <- 238
Forestry$Country[Forestry$Country.Code==151] <- "Netherlands Antilles"
# Forestry$Country <- as.factor(Forestry$Country)

# detect missing years for countries in the regions list
missing <- data.frame(Country.Code = integer(), Year = integer(), stringsAsFactors = FALSE)
for(region in regions$Country.Code){
  if(length(years[! years %in% Forestry$Year[Forestry$Country.Code==region]])<6){
    for(year in years[! years %in% Forestry$Year[Forestry$Country.Code==region]]){
      missing[nrow(missing)+1,] <- c(region, year)
    }
  }
}
# add missing years 
region=151
for(region in unique(missing$Country.Code)){
  yearsreg <- missing$Year[missing$Country.Code==region]
  if(min(yearsreg)==1986){
    for(year in min(years):max(yearsreg)){
      add <- Forestry[Forestry$Country.Code==region & Forestry$Year==max(yearsreg)+1,]
      add$Year <- year
      Forestry <- rbind(Forestry,add)
    }
  } else if(max(yearsreg)==max(years)){
    for(year in min(yearsreg):max(years)){
      add <- Forestry[Forestry$Country.Code==region & Forestry$Year==min(yearsreg)-1,]
      add$Year <- year
      Forestry <- rbind(Forestry,add)
    }
  }
}
save(Forestry, file="data/Forestry.RData")


##########################################################################
# Prepare FORESTRY TRADE DATA
##########################################################################
load(file="data/raw data/ForTrade_raw.RData")
ForTrade <- ForTrade_raw[! ForTrade_raw$Value==0,] # delete rows with 0 values
# exclude all items except 1651  Ind Rwd Wir (C), 1657  Ind Rwd Wir (NC) Tropica, 1670  Ind Rwd Wir (NC) Other
ForTrade <- ForTrade[ForTrade$Item.Code %in% c(1651,1657,1670) & ForTrade$Reporter.Country.Code<5000 & ForTrade$Partner.Country.Code<5000,]
ForTrade$Flag <- NULL
ForTrade$Year.Code <- NULL
ForTrade$Element.Code <- NULL
# add element names "Import" and "Export"
ForTrade$Element <- as.character(ForTrade$Element)
ForTrade$Element[ForTrade$Element=="Import Quantity" | ForTrade$Element=="Import Value"] <- "Import"
ForTrade$Element[ForTrade$Element=="Export Quantity" | ForTrade$Element=="Export Value"] <- "Export"
ForTrade$Element <- as.factor(ForTrade$Element)
# exchange countries (41 for 351)
ForTrade$Reporter.Countries <- as.character(ForTrade$Reporter.Countries)
ForTrade$Partner.Countries <- as.character(ForTrade$Partner.Countries)
ForTrade$Reporter.Country.Code[ForTrade$Reporter.Country.Code==351] <- 41
ForTrade$Reporter.Countries[ForTrade$Reporter.Countries=="China"] <- "China, mainland"
ForTrade$Partner.Country.Code[ForTrade$Partner.Country.Code==351] <- 41
ForTrade$Partner.Countries[ForTrade$Partner.Countries=="China"] <- "China, mainland"
rm(ForTrade_raw)
ForTrade <- ForTrade[ForTrade$Unit=="m3",]
#-----------------------------
# cast data.frame to get the different elements in columns
#-----------------------------
ForTrade <- dcast(ForTrade, Reporter.Country.Code + Reporter.Countries + Partner.Country.Code + Partner.Countries + Item.Code + 
                    Item + Year ~ Element, value.var="Value")
ForTrade[is.na(ForTrade)] <- 0

# rename Countries (i.e. Czechia and Netherlands Antilles)
ForTrade$Reporter.Countries <- regions$Country[match(ForTrade$Reporter.Country.Code,regions$Country.Code)]
ForTrade$Partner.Countries <- regions$Country[match(ForTrade$Partner.Country.Code,regions$Country.Code)]
ForTrade$Reporter.Countries <- as.character(ForTrade$Reporter.Countries)
ForTrade$Partner.Countries <- as.character(ForTrade$Partner.Countries)
# rename Sudan (former)
ForTrade$Reporter.Countries[ForTrade$Reporter.Country.Code==206] <- "Sudan"
ForTrade$Reporter.Country.Code[ForTrade$Reporter.Country.Code==206] <- 276
ForTrade$Partner.Countries[ForTrade$Partner.Country.Code==206] <- "Sudan"
ForTrade$Partner.Country.Code[ForTrade$Partner.Country.Code==206] <- 276
# rename Ethiopia PRD
ForTrade$Reporter.Countries[ForTrade$Reporter.Country.Code==62] <- "Ethiopia"
ForTrade$Reporter.Country.Code[ForTrade$Reporter.Country.Code==62] <- 238
ForTrade$Partner.Countries[ForTrade$Partner.Country.Code==62] <- "Ethiopia"
ForTrade$Partner.Country.Code[ForTrade$Partner.Country.Code==62] <- 238
ForTrade$Reporter.Countries[ForTrade$Reporter.Country.Code==151] <- "Netherlands Antilles"
ForTrade$Partner.Countries[ForTrade$Partner.Country.Code==151] <- "Netherlands Antilles"

save(ForTrade, file="data/ForTrade.RData")



##########################################################################
# Prepare CROP PRODUCTION DATA
##########################################################################
load(file="data/raw data/Prod_raw.RData")
names(Prod_raw)[1:2] <- c("Country.Code","Country")
Prod_raw <- Prod_raw[is.finite(Prod_raw$Value),]
# delete rows with 0 values and countries with Code >= 5000 (= country groups, e.g. 'Africa')
Prod_raw <- Prod_raw[! Prod_raw$Value==0 & Prod_raw$Country.Code<5000,]
Prod <- Prod_raw[,c(1,2,3,4,5,6,8,9,10)]
rm(Prod_raw)
names(Prod)[c(3,4)] <- c("Prod.Code","Prod")
# convert to CBS item codes and aggregate Fodder crops
cropcom <- read.csv2(file="Items_Prod-CBS.csv")
# merg data.tables -> much faster than merging data.frames
# Prod <- merge(Prod, cropcom[,c(1,3,4)], by="Prod.Code", all.x=TRUE)
Prod <- merge(data.table(Prod, key="Prod.Code"), data.table(cropcom[,c(1,3,4,5)], key="Prod.Code"))
rm(cropcom)
Prod$TCF <- as.numeric(as.character(Prod$TCF))
Prod$Value <- Prod$Value * Prod$TCF
Prod$TCF <- NULL
# it's so much faster aggregating a data.table than a data.frame:
Prod <- data.frame(Prod[,list(val = sum(Value)), by = 'Country.Code,Country,Element.Code,Element,Year,Unit,Item.Code,Item'])
names(Prod)[9] <- "Value"
Prod <- Prod[,c(1:2,7:8,3:6,9)]
# add fodder crops
load(file="data/raw data/Primary_raw.RData")
foddercrops <- data.frame(Item.Code=c(rep(2000,16)),
                          Item=c(rep("Fodder crops",16)),
                          Prod.Code=c(636,637,638,639,640,641,642,643,644,645,646,647,648,649,651,655), 
                          Prod=c("Forage and silage, maize","Forage and silage, sorghum","Forage and silage, rye grass","Forage and silage, grasses nes",
                                 "Forage and silage, clover","Forage and silage, alfalfa","Forage and silage, green oilseeds","Forage and silage, legumes",
                                 "Cabbage for fodder","Mixed Grasses and Legumes","Turnips for fodder","Beets for fodder","Carrots for fodder",
                                 "Swedes for fodder","Forage products","Vegetables and roots fodder"))
Fodder <- Primary_raw[Primary_raw$ItemCode %in% foddercrops$Prod.Code,3:10]
names(Fodder) <- c("Country.Code","Country","Element.Code","Element","Prod.Code","Prod","Year","Value")
Fodder <- merge(Fodder, foddercrops[,1:3], by="Prod.Code", all.x=TRUE)
Fodder <- aggregate(Value ~ ., Fodder[,c(2,3,4,5,7,8,9,10)],sum)
Fodder <- Fodder[Fodder$Element!="Yield",]
Fodder$Unit <- "tonnes"
Fodder$Unit[Fodder$Element=="Area harvested"] <- "ha"
Fodder$Unit <- as.factor(Fodder$Unit)
Fodder <- Fodder[,c(1,2,6,7,3,4,5,9,8)]
Prod <- rbind(Prod,Fodder)
Prod <- Prod[Prod$Value>0,]
Prod <- Prod[!is.na(Prod$Item.Code),]
rm(Fodder,foddercrops,Primary_raw)

##########################################################################
# Prepare PROCESSING PRODUCTION DATA
##########################################################################
load(file="data/raw data/Proc_crop_raw.RData")
load(file="data/raw data/Proc_lvst_raw.RData")
Proc_raw <- rbind(Proc_crop_raw, Proc_lvst_raw)
rm(Proc_crop_raw,Proc_lvst_raw)
names(Proc_raw)[1:2] <- c("Country.Code","Country")
Proc_raw <- Proc_raw[is.finite(Proc_raw$Value),]
# delete rows with 0 values and countries with Code >= 5000 (= country groups, e.g. 'Africa')
Proc_raw <- Proc_raw[! Proc_raw$Value==0 & Proc_raw$Country.Code<5000,]
Proc <- Proc_raw[,c(1:6,8:10)]
rm(Proc_raw)
names(Proc)[c(3,4)] <- c("Prod.Code","Prod")
# convert to CBS item codes and aggregate Fodder crops
cropcom <- read.csv2(file="Items_Proc-CBS.csv")
# merg data.tables -> much faster than merging data.frames
# Prod <- merge(Prod, cropcom[,c(1,3,4)], by="Prod.Code", all.x=TRUE)
Proc <- merge(data.table(Proc, key="Prod.Code"), data.table(cropcom[,c(1,3,4,5)], key="Prod.Code"))
rm(cropcom)
Proc$TCF <- as.numeric(as.character(Proc$TCF))
Proc$Value <- Proc$Value * Proc$TCF
Proc$TCF <- NULL
Proc <- Proc[Proc$Value>0,]
# it's so much faster aggregating a data.table than a data.frame:
Proc <- data.frame(Proc[,list(val = sum(Value)), by = 'Country.Code,Country,Element.Code,Element,Year,Unit,Item.Code,Item'])
names(Proc)[9] <- "Value"
Proc <- Proc[,c(1:2,7:8,3:6,9)]
Prod <- rbind(Prod, Proc)

# rename Countries (i.e. Czechia and Netherlands Antilles)
Prod$Country <- regions$Country[match(Prod$Country.Code,regions$Country.Code)]
Prod$Country <- as.character(Prod$Country)

# rename Sudan (former) and Ethiopia PDR
Prod$Country[Prod$Country.Code==206] <- "Sudan"
Prod$Country.Code[Prod$Country.Code==206] <- 276
Prod$Country[Prod$Country.Code==62] <- "Ethiopia"
Prod$Country.Code[Prod$Country.Code==62] <- 238

# Prod$Country <- as.factor(Prod$Country)
# exclude China to avoid double counting with China mainland, Taiwan, etc.
Prod <- Prod[!Prod$Country.Code==351,]
Butter <- Prod[Prod$Item=="Butter, Ghee",]
Prod <- Prod[!Prod$Item=="Butter, Ghee",]
save(Prod, file="data/Prod.RData")


##########################################################################
# Prepare LVST PRODUCTION DATA
##########################################################################
load(file="data/raw data/Lvst_raw.RData")
names(Lvst_raw)[1:2] <- c("Country.Code","Country")
# delete rows with 0 values & country groups
Lvst <- Lvst_raw[! Lvst_raw$Value==0 & ! is.na(Lvst_raw$Value) & Lvst_raw$Country.Code<5000,]
rm(Lvst_raw)
Lvst <- Lvst[,c(1,2,3,4,6,8,9,10)]
# exclude the items "Chickens", "Ducks", "Turkeys", "Geeese and guinea fowls" and "Pigeons, other birds" and use the aggregate "Poultry Birds"
# exclude the aggregates "Cattle and Buffaloes" and "Sheep and Goats" and the item "Beehives"
Lvst <- Lvst[! Lvst$Item.Code %in% c(1057,1068,1079,1072,1083,1746,1749,1181),]

# rename Countries (i.e. Czechia and Netherlands Antilles)
Lvst$Country <- regions$Country[match(Lvst$Country.Code,regions$Country.Code)]
Lvst$Country <- as.character(Lvst$Country)

# rename Sudan (former)
Lvst$Country[Lvst$Country.Code==206] <- "Sudan"
Lvst$Country.Code[Lvst$Country.Code==206] <- 276
Lvst$Country[Lvst$Country.Code==62] <- "Ethiopia"
Lvst$Country.Code[Lvst$Country.Code==62] <- 238

# exclude China to avoid double counting with China mainland, Taiwan, etc.
Lvst <- Lvst[!Lvst$Country.Code==351,]

save(Lvst, file="data/Lvst.RData")


##########################################################################
# Prepare LVST PRIMARY PRODUCTION DATA
##########################################################################
load(file="data/raw data/LvstPrimary_raw.RData")
names(LvstPrimary_raw)[1:2] <- c("Country.Code","Country")
# delete rows with 0 values & country groups
Prod_lvst <- LvstPrimary_raw[! LvstPrimary_raw$Value==0 & ! is.na(LvstPrimary_raw$Value) & LvstPrimary_raw$Country.Code<5000,]
rm(LvstPrimary_raw)
Prod_lvst <- Prod_lvst[,c(1:6,8:10)]
# rename Countries (i.e. Czechia and Netherlands Antilles)
Prod_lvst$Country <- regions$Country[match(Prod_lvst$Country.Code,regions$Country.Code)]
Prod_lvst$Country <- as.character(Prod_lvst$Country)
# rename Sudan (former)
Prod_lvst$Country[Prod_lvst$Country.Code==206] <- "Sudan"
Prod_lvst$Country.Code[Prod_lvst$Country.Code==206] <- 276
Prod_lvst$Country[Prod_lvst$Country.Code==62] <- "Ethiopia"
Prod_lvst$Country.Code[Prod_lvst$Country.Code==62] <- 238
# Prod_lvst$Country <- as.factor(Prod_lvst$Country)
# exclude China to avoid double counting with China mainland, Taiwan, etc.
Prod_lvst <- Prod_lvst[!Prod_lvst$Country.Code==351,]
Prod_lvst <- rbind(Prod_lvst,Butter)
save(Prod_lvst, file="data/Prod_lvst.RData")



##########################################################################
# Bio-Ethanol production data
##########################################################################
load(file="data/raw data/ProdEthanol_raw.RData")
# prepare IEA data
ProdEthanol_IEA <- ProdEthanol_IEA[-1,-2]
names(ProdEthanol_IEA)[1] <- "Country.IEA"
ProdEthanol_IEA <- melt(ProdEthanol_IEA, "Country.IEA", variable.names="Year", value.name = "Production")
ProdEthanol_IEA$Production <- as.numeric(ProdEthanol_IEA$Production)
ProdEthanol_IEA <- ProdEthanol_IEA[is.finite(ProdEthanol_IEA$Production) & ProdEthanol_IEA$Production > 0,]
names(ProdEthanol_IEA)[2] <- "Year"
regfit <- read.csv(file="Regions_IEA-FAO.csv", sep=";")
# reg <- read.csv(file="Regions.csv", sep=";")
ProdEthanol_IEA$Country <- regfit$Country[match(ProdEthanol_IEA$Country.IEA, regfit$Country.IEA)]
ProdEthanol_IEA$Country <- as.character(ProdEthanol_IEA$Country)
ProdEthanol_IEA <- ProdEthanol_IEA[!is.na(ProdEthanol_IEA$Country),]
ProdEthanol_IEA$Country.Code <- regions$Country.Code[match(ProdEthanol_IEA$Country, regions$Country)]
ProdEthanol_IEA <- ProdEthanol_IEA[,c(5,4,2,3)]
# convert from ktoe to tonnes
# conversion factors: 1 tonne = 1 ktoe / 0.64 toe/tonne * 1000 tonnes/kt
ProdEthanol_IEA$Production <- round(ProdEthanol_IEA$Production / 0.64 * 1000, 2)
# prepare EIA data
ProdEthanol_EIA <- ProdEthanol_EIA[-(1:3),-2]
names(ProdEthanol_EIA)[1] <- "Country.EIA"
ProdEthanol_EIA <- melt(ProdEthanol_EIA, "Country.EIA", variable.names="Year", value.name = "Production")
ProdEthanol_EIA$Production <- as.numeric(ProdEthanol_EIA$Production)
ProdEthanol_EIA <- ProdEthanol_EIA[is.finite(ProdEthanol_EIA$Production),]
names(ProdEthanol_EIA)[2] <- "Year"
regfit <- read.csv(file="Regions_EIA-FAO.csv", sep=";")
# reg <- read.csv(file="Regions.csv", sep=";")
ProdEthanol_EIA$Country <- regfit$Country[match(ProdEthanol_EIA$Country.EIA, regfit$Country.EIA)]
ProdEthanol_EIA$Country <- as.character(ProdEthanol_EIA$Country)
ProdEthanol_EIA <- ProdEthanol_EIA[!is.na(ProdEthanol_EIA$Country),]
ProdEthanol_EIA$Country.Code <- regions$Country.Code[match(ProdEthanol_EIA$Country, regions$Country)]
ProdEthanol_EIA <- ProdEthanol_EIA[,c(5,4,2,3)]

# convert from thousand barrels per day to tonnes per year 
# conversion factors: ton/y = 1000 bbl/d * 365.25 d/y * 158.9873 L/bbl * 0.7893 kg/L
ProdEthanol_EIA$Production <- round(ProdEthanol_EIA$Production*365.25*158.9873*0.7893, 2)

# merge EIA and IEA data
names(ProdEthanol_IEA)[4] <- "Production_IEA"
names(ProdEthanol_EIA)[4] <- "Production_EIA"

ProdEthanol <- merge(ProdEthanol_IEA, ProdEthanol_EIA, all = TRUE)
ProdEthanol[is.na(ProdEthanol)] <- 0
ProdEthanol$Production <- apply(ProdEthanol[,4:5], 1, max)
ProdEthanol[,4:5] <- NULL
rm(ProdEthanol_EIA, ProdEthanol_IEA)

ProdEthanol$Year <- as.numeric(as.character(ProdEthanol$Year))

# change Sudan (former) to Sudan and Ethiopia PDR to Ethiopia
ProdEthanol$Country[ProdEthanol$Country.Code==206] <- "Sudan"
ProdEthanol$Country.Code[ProdEthanol$Country.Code==206] <- 276
ProdEthanol$Country[ProdEthanol$Country.Code==62] <- "Ethiopia"
ProdEthanol$Country.Code[ProdEthanol$Country.Code==62] <- 238

save(ProdEthanol, file="data/ProdEthanol.RData")


##########################################################################
# Comtrade bilateral trade data (Bio-Ethanol and Fish)
##########################################################################
load(file="data/raw data/comtrade_raw.RData")
comtrade$netweight_kg[is.na(comtrade$netweight_kg)] <- 0
#------------------------------
# trade flows with nes-regions (2.5% of all reported netweight_kg) are neglected:
# View(unique(comtrade[is.na(comtrade$partner_iso), 12:14]))
# 129	Caribbean, nes
# 490	Other Asia, nes
# 492	Europe EU, nes
# 527	Oceania, nes
# 536	Neutral Zone
# 568	Other Europe, nes
# 577	Other Africa, nes
# 636	Rest of America, nes
# 837	Bunkers
# 838	Free Zones
# 839	Special Categories
# 849	US Misc. Pacific Isds
# 879	Western Asia, nes
# 899	Areas, nes
# sum(comtrade$netweight_kg[is.na(comtrade$partner_iso)]) / sum(comtrade$netweight_kg)
# sum(comtrade$trade_value_usd[comtrade$netweight_kg==0]) / sum(comtrade$trade_value_usd)
#------------------------------
comtrade %<>% 
  dplyr::select(year, trade_flow, reporter_iso, partner_iso, commodity_code, commodity, qty_unit, qty, netweight_kg, trade_value_usd)
# comtrade <- comtrade[,c(2,8,11,14,22,23,25,28,30,32)]
# regfit <- read.csv(file="Regions_Comtrade-FAO.csv", sep=";")
regfit <- read.csv(file="Regions_ISO3Comtrade-FAO.csv", sep=";")
comtrade <- merge(comtrade, regfit, by.x="reporter_iso", by.y="ISO3")
comtrade <- merge(comtrade, regfit, by.x="partner_iso", by.y="ISO3")
comtrade <- comtrade[,c(11:14,3:10)]
names(comtrade)[1:6] <- c("Reporter.Country.Code","Reporter.Country","Partner.Country.Code","Partner.Country","Year","Element")
comtrade$Reporter.Country <- as.character(comtrade$Reporter.Country)
comtrade$Partner.Country <- as.character(comtrade$Partner.Country)
# unique(comtrade[,c(7,9)])
comtrade <- dcast(comtrade, Reporter.Country.Code + Reporter.Country + Partner.Country.Code + Partner.Country + Year + Element + 
                    commodity_code + commodity + trade_value_usd ~ qty_unit, value.var="qty")
comtrade$`No Quantity` <- NULL
comtrade$`Number of items` <- NULL
names(comtrade)[9:11] <- c("usd","litres","kg")
# convert kg in litres for ethanol (0.7893 kg/l, source: https://en.wikipedia.org/wiki/Ethanol)
comtrade$litres[is.na(comtrade$litres) & comtrade$commodity_code=="2207"] <- 
  comtrade$kg[is.na(comtrade$litres) & comtrade$commodity_code=="2207"] / 0.7893
comtrade$kg[is.na(comtrade$kg) & comtrade$commodity_code=="2207"] <- 
  comtrade$litres[is.na(comtrade$kg) & comtrade$commodity_code=="2207"] * 0.7893

# change Sudan (former) and Ethiopia
comtrade$Reporter.Country[comtrade$Reporter.Country.Code==206] <- "Sudan"
comtrade$Reporter.Country.Code[comtrade$Reporter.Country.Code==206] <- 276
comtrade$Partner.Country[comtrade$Partner.Country.Code==206] <- "Sudan"
comtrade$Partner.Country.Code[comtrade$Partner.Country.Code==206] <- 276
comtrade$Reporter.Country[comtrade$Reporter.Country.Code==62] <- "Ethiopia"
comtrade$Reporter.Country.Code[comtrade$Reporter.Country.Code==62] <- 238
comtrade$Partner.Country[comtrade$Partner.Country.Code==62] <- "Ethiopia"
comtrade$Partner.Country.Code[comtrade$Partner.Country.Code==62] <- 238

save(comtrade, file="data/comtrade.RData")


##########################################################################
# BACI bilateral trade data (Bio-Ethanol and Fish)
# data structure: value of trade (v, in thousands of US dollars), quantity (q, in tons), exporter (i), importer (j), 
#                 product category (k), year (t)
##########################################################################
load(file="data/Raw data/BACI_selected.RData")
regfit <- read.csv(file="Regions_BACI-FAO.csv", sep=";")
BACI <- merge(BACI, regfit, by.x="i", by.y="Baci.Code")
BACI <- merge(BACI, regfit, by.x="j", by.y="Baci.Code")
BACI <- BACI[,c(7:10,3:6)]
names(BACI) <- c("From.Country.Code","From.Country","To.Country.Code","To.Country","Year","hs6","tUSD","tonnes")
BACI$From.Country <- as.character(BACI$From.Country)
BACI$To.Country <- as.character(BACI$To.Country)

# change Sudan (former) to Sudan
BACI$From.Country[BACI$From.Country.Code==206] <- "Sudan"
BACI$From.Country.Code[BACI$From.Country.Code==206] <- 276
BACI$To.Country[BACI$To.Country.Code==206] <- "Sudan"
BACI$To.Country.Code[BACI$To.Country.Code==206] <- 276
# change Ethiopia PDR to Ethiopia
BACI$From.Country[BACI$From.Country.Code==62] <- "Ethiopia"
BACI$From.Country.Code[BACI$From.Country.Code==62] <- 238
BACI$To.Country[BACI$To.Country.Code==62] <- "Ethiopia"
BACI$To.Country.Code[BACI$To.Country.Code==62] <- 238

save(BACI, file="data/BACI.RData")


##########################################################################
# Fish production data
##########################################################################
# read and prepare fish production data
#-----------------------------
load(file="data/raw data/FishProd_raw.RData")
names(FishProd_raw)[1] <- "Fish.Code"
regfit <- read.csv(file="Regions_FISH-FAO.csv", sep=";")
# reg <- read.csv(file="Regions.csv", sep=";")
FishProd <- merge(FishProd_raw, regfit)
FishProd <- FishProd[,c(8,9,5,6,3)]
FishProd <- aggregate(Quantity ~ ., FishProd, sum)
FishProd$Source <- as.character(FishProd$Source)
# FishProd$Source[FishProd$Source==1] <- "Aquaculture (freshwater)"
# FishProd$Source[FishProd$Source==2] <- "Aquaculture (brackishwater)"
# FishProd$Source[FishProd$Source==3] <- "Aquaculture (marine)"
FishProd$Source[FishProd$Source %in% 1:3] <- "Aquaculture"
FishProd$Source[FishProd$Source==4] <- "Capture production"
# change Sudan (former) to Sudan
FishProd$Country <- as.character(FishProd$Country)
FishProd$Country[FishProd$Country.Code==206] <- "Sudan"
FishProd$Country.Code[FishProd$Country.Code==206] <- 276
FishProd$Country[FishProd$Country.Code==62] <- "Ethiopia"
FishProd$Country.Code[FishProd$Country.Code==62] <- 238
save(FishProd, file="data/FishProd.RData")
##########################################################################

