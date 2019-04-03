##############################################################################################
##  
##  FABIO: BUILD MRIO TABLE BASED ON FAOSTAT COMMODITY BALANCE SHEETS AND TRADE DATA
##    1c Estimate missing CBS
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


##########################################################################
# Start loop for a series of years
##########################################################################
load(file="/mnt/nfs_fineprint/tmp/fabio/data/Prod.RData")
load(file="/mnt/nfs_fineprint/tmp/fabio/data/Prod_lvst.RData")
Prod_crop_all <- Prod
Prod_lvst_all <- Prod_lvst

# year=1988
year=2013
for(year in 1986:2013){
  print(year)
  ##########################################################################
  # Read data
  #-------------------------------------------------------------------------
  load(file=paste0("/mnt/nfs_fineprint/tmp/fabio/data/yearly/",year,"_CBS.RData"))
  load(file=paste0("/mnt/nfs_fineprint/tmp/fabio/data/yearly/",year,"_BTD.RData"))
  ##########################################################################
  # start loop over all countries
  #-------------------------------------------------------------------------
  Prod_lvst <- Prod_lvst_all[Prod_lvst_all$Year==year, ]
  Prod_lvst <- Prod_lvst[Prod_lvst$Element=="Production" & Prod_lvst$Unit %in% c("Head", "1000 Head"),]
  Prod_lvst$Value[Prod_lvst$Unit=="Head"] <- Prod_lvst$Value[Prod_lvst$Unit=="Head"] / 1000
  Prod_lvst$Unit[Prod_lvst$Unit=="Head"] <- "1000 Head"
  concordance <- data.frame(Item.Code.lvst = c(1137,944,1032,1012,1775,1055,1120,1144,972,1161,1154,1122,1124),
                            Item.lvst = c("Meat indigenous, camel","Meat indigenous, cattle","Meat indigenous, goat","Meat indigenous, sheep",
                                          "Meat indigenous, poultry","Meat indigenous, pig","Meat indigenous, horse","Meat indigenous, rabbit",
                                          "Meat indigenous, buffalo","Meat indigenous, other camelids","Meat indigenous, rodents","Meat indigenous, ass",
                                          "Meat indigenous, mule"),
                            Item.Code = c(1126,866,1016,976,2029,1034,1096,1140,946,1157,1150,1107,1110),
                            Item = c("Camels","Cattle","Goats","Sheep","Poultry Birds","Pigs","Horses","Rabbits and hares","Buffaloes",
                                     "Camelids, other","Rodents, other","Asses","Mules"))
  names(Prod_lvst)[3:4] <- names(concordance)[1:2]
  Prod_lvst <- merge(Prod_lvst, concordance[,-2], by="Item.Code.lvst", all.x = T)
  Prod_lvst <- Prod_lvst[!is.na(Prod_lvst$Item.Code),]
  
  addCBS_all <- CBS[0,]
  # k=0
  # country = 250
  for(country in unique(c(unique(Prod_crop_all$Country.Code), unique(Prod_lvst_all$Country.Code)))){
    # k=k+1
    # if(k %% round(length(unique(c(unique(Prod_crop_all$Country.Code), unique(Prod_lvst_all$Country.Code))))/5) == 0) 
    #   print(paste((k %/% round(length(unique(c(unique(Prod_crop_all$Country.Code), unique(Prod_lvst_all$Country.Code))))/10))*10, "%"))
    Prod <- Prod_crop_all[Prod_crop_all$Year==year & Prod_crop_all$Country.Code==country, ]
    # Prod_lvst <- Prod_lvst_all[Prod_lvst_all$Year==year & Prod_lvst_all$Country.Code==country, ]
    # TCF <- read.table(file = "TCF_prod_lvst.csv", sep=";", header = T)
    # names(Prod_lvst)[3:4] <- names(TCF)[1:2]
    # Prod_lvst <- merge(Prod_lvst, TCF, by = "Prod.Code", all.x = T)
    # Prod_lvst <- Prod_lvst[Prod_lvst$Element=="Production",]
    # Prod_lvst <- Prod_lvst[! is.na(Prod_lvst$Item.Code),]
    # Prod_lvst$Value <- Prod_lvst$Value * Prod_lvst$TCF
    # Prod_lvst <- Prod_lvst[,c("Country.Code","Country","Item.Code","Item","Year","Unit","Value")]
    # if(nrow(Prod_lvst)>0)
    #   Prod_lvst <- aggregate(Value ~ ., data=Prod_lvst, sum)
    #---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---
    # prepare CBS
    #---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---
    addCBS <- data.frame(Country.Code=rep(country, nrow(items)),
                         Country=rep(reg$Country[reg$Country.Code==country], nrow(items)),
                         Item.Code=items$Item.Code,
                         Item=items$Item,
                         Year=year, Production=0, Imports=0, StockVariation=0, TotalSupply=0, Exports=0, Food=0, Feed=0, Processing=0, 
                         Seed=0, Waste=0, OtherUses=0, Balancing=0, stringsAsFactors = FALSE)
    # only estimate CBS values for items not already included in CBS
    addCBS <- addCBS[! addCBS$Item %in% CBS$Item[CBS$Country.Code==country], ]
    if(country %in% (Prod_crop_all$Country.Code) & nrow(Prod)>0){
      #---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---
      # write production and seed
      #---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---
      for(item in Prod$Item[Prod$Element=="Production"]){
        addCBS$Production[addCBS$Item==item] <- Prod$Value[Prod$Item==item & Prod$Element=="Production"]
      }
      for(item in addCBS$Item[addCBS$Item %in% Prod$Item[Prod$Element=="Seed"]]){
        addCBS$Seed[addCBS$Item==item] <- Prod$Value[Prod$Item==item & Prod$Element=="Seed"]
      }
      #---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---
      # for processed products: estimate required processing inputs
      #---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---
      TCF <- read.table(file = "./inst/fabio_input/TCF_prod.csv", sep = ";", header = T)
      TCF$Processing <- 0
      TCF$Production <- 0
      # item = "Beer"
      for(item in addCBS$Item){
        source <- TCF$Source.Item[TCF$Item==item]
        TCF$Processing[TCF$Item==item] <- addCBS$Production[addCBS$Item==item] / TCF$TCF[TCF$Item==item]
        TCF$Production[TCF$Item==item] <- addCBS$Production[addCBS$Item==item]
      }
      # estimate share of sugar beet and sugar cane in sugar and molasses production
      Sugar <- TCF[TCF$Item.Code %in% c(2544, 2818),]
      if(length(addCBS$Production[addCBS$Item.Code==2818])>0){
        Sugar$Production[Sugar$Source.Item=="Sugar beet"] <- addCBS$Production[addCBS$Item=="Sugar beet"]
        Sugar$Production[Sugar$Source.Item=="Sugar cane"] <- addCBS$Production[addCBS$Item=="Sugar cane"]
        Sugar$Processing[Sugar$Source.Item=="Sugar beet"] <- Sugar$Processing[Sugar$Source.Item=="Sugar beet"] / 
          (addCBS$Production[addCBS$Item=="Sugar beet"] + addCBS$Production[addCBS$Item=="Sugar cane"]) * 
          addCBS$Production[addCBS$Item=="Sugar beet"]
        Sugar$Processing[Sugar$Source.Item=="Sugar cane"] <- Sugar$Processing[Sugar$Source.Item=="Sugar cane"] / 
          (addCBS$Production[addCBS$Item=="Sugar beet"] + addCBS$Production[addCBS$Item=="Sugar cane"]) * 
          addCBS$Production[addCBS$Item=="Sugar cane"]
        TCF$Processing[TCF$Source.Item=="Sugar beet" & TCF$Item.Code==2544] <- Sugar$Processing[Sugar$Source.Item=="Sugar beet" & Sugar$Item.Code==2544]
        TCF$Processing[TCF$Source.Item=="Sugar beet" & TCF$Item.Code==2818] <- Sugar$Processing[Sugar$Source.Item=="Sugar beet" & Sugar$Item.Code==2818]
        TCF$Processing[TCF$Source.Item=="Sugar cane" & TCF$Item.Code==2544] <- Sugar$Processing[Sugar$Source.Item=="Sugar cane" & Sugar$Item.Code==2544]
        TCF$Processing[TCF$Source.Item=="Sugar cane" & TCF$Item.Code==2818] <- Sugar$Processing[Sugar$Source.Item=="Sugar cane" & Sugar$Item.Code==2818]
        TCF$Production[TCF$Source.Item=="Sugar cane" & TCF$Item.Code %in% c(2544,2818)] <- 
          TCF$Production[TCF$Source.Item=="Sugar cane" & TCF$Item.Code %in% c(2544,2818)] / 
          (Sugar$Production[Sugar$Source.Item=="Sugar beet"] + Sugar$Production[Sugar$Source.Item=="Sugar cane"]) * 
          Sugar$Production[Sugar$Source.Item=="Sugar cane"]
        TCF$Production[TCF$Source.Item=="Sugar beet" & TCF$Item.Code %in% c(2544,2818)] <- 
          TCF$Production[TCF$Source.Item=="Sugar beet" & TCF$Item.Code %in% c(2544,2818)] / 
          (Sugar$Production[Sugar$Source.Item=="Sugar beet"] + Sugar$Production[Sugar$Source.Item=="Sugar cane"]) * 
          Sugar$Production[Sugar$Source.Item=="Sugar beet"]
      }
      rm(Sugar)
      # estimate production gaps for co-products
      couples <- data.frame(Item.Code = c(2578, 2575, 2572, 2582, 2562, 2586, 2576, 2574, 2559, 2579, 2571, 2818, 2573),
                            Couple.Code = c(2596, 2594, 2591, 2598, 2577, 2598, 2595, 2593, 2661, 2597, 2590, 2544, 2592),
                            Processing.Item = 0,
                            Processing.Couple = 0)
      couples <- merge(couples, TCF[,c(1,3)], by = "Item.Code", all.x = TRUE)
      # i=1
      for(i in 1:nrow(couples)){
        couples$Processing.Item[i] <- TCF$Processing[TCF$Item.Code==couples$Item.Code[i] & TCF$Source.Item==couples$Source.Item[i]]
        couples$Processing.Couple[i] <- TCF$Processing[TCF$Item.Code==couples$Couple.Code[i] & TCF$Source.Item==couples$Source.Item[i]]
      }
      couples[is.na(couples)] <- 0
      couples$Processing.Item[couples$Processing.Item==0] <- couples$Processing.Couple[couples$Processing.Item==0]
      couples$Processing.Couple[couples$Processing.Couple==0] <- couples$Processing.Item[couples$Processing.Couple==0]
      # Take mean of the two processing input estimates
      couples$Processing.Item <- couples$Processing.Couple <- (couples$Processing.Item + couples$Processing.Couple) / 2
      names(couples)[3] <- "Processing"
      couples[,4] <- NULL
      for(item in couples$Item.Code){
        for(source in TCF$Source.Item[TCF$Item.Code==item]){
          TCF$Processing[TCF$Item.Code==item & TCF$Source.Item==source] <- 
            couples$Processing[couples$Item.Code==item & couples$Source.Item==source] / 2
        }
      }
      for(item in couples$Couple.Code){
        for(source in TCF$Source.Item[TCF$Item.Code==item]){
          TCF$Processing[TCF$Item.Code==item & TCF$Source.Item==source] <- 
            couples$Processing[couples$Couple.Code==item & couples$Source.Item==source] / 2
        }
      }
      rm(couples)
      # estimate co-product production quantity
      TCF[is.na(TCF)] <- 0
      TCF$Production[TCF$Production==0] <- TCF$Processing[TCF$Production==0] * TCF$TCF[TCF$Production==0]
      addCBS$Item <- as.character(addCBS$Item)
      for(i in 1:nrow(TCF)){
        item <- TCF$Item.Code[i]
        source <- TCF$Source.Item[i]
        addCBS$Production[addCBS$Item.Code==item] <- TCF$Production[i]
        addCBS$Processing[addCBS$Item==source] <- addCBS$Processing[addCBS$Item==source] + TCF$Processing[i]
      }
      rm(TCF)
    }
    if(country %in% (Prod_lvst$Country.Code)){
      #---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---
      # write production and seed
      #---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---
      for(item in Prod_lvst$Item[Prod_lvst$Country.Code==country]){
        addCBS$Production[addCBS$Item==item] <- Prod_lvst$Value[Prod_lvst$Item==item & Prod_lvst$Country.Code==country]
      }
    }
    #---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---
    # write trade data from BTD into addCBS
    #---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---
    # imports 
    if(nrow(BTD[BTD$Item.Code %in% addCBS$Item.Code & BTD$To.Country.Code==country,])>0){
      imports <- aggregate(tonnes ~ To.Country.Code + To.Country + Item.Code + Item + Year, BTD[BTD$Item.Code %in% addCBS$Item.Code & BTD$To.Country.Code==country,], sum)
      imports <- imports[!imports$tonnes==0,]
      for(item in imports$Item){
        addCBS$Imports[addCBS$Item==item] <- imports$tonnes[imports$Item==item]
      }
      # livestock imports
      temp <- aggregate(tHead ~ To.Country.Code + To.Country + Item.Code + Item + Year, BTD[BTD$Item.Code %in% addCBS$Item.Code & BTD$To.Country.Code==country,], sum)
      temp <- temp[!temp$tHead==0,]
      for(item in temp$Item){
        addCBS$Imports[addCBS$Item==item] <- temp$tHead[temp$Item==item]
      }
    }
    # exports
    if(nrow(BTD[BTD$Item.Code %in% addCBS$Item.Code & BTD$From.Country.Code==country,])>0){
      exports <- aggregate(tonnes ~ From.Country.Code + From.Country + Item.Code + Item + Year, BTD[BTD$Item.Code %in% addCBS$Item.Code & BTD$From.Country.Code==country,], sum)
      exports <- exports[!exports$tonnes==0,]
      for(item in exports$Item){
        addCBS$Exports[addCBS$Item==item] <- exports$tonnes[exports$Item==item]
      }
      # livestock exports
      temp <- aggregate(tHead ~ From.Country.Code + From.Country + Item.Code + Item + Year, BTD[BTD$Item.Code %in% addCBS$Item.Code & BTD$From.Country.Code==country,], sum)
      temp <- temp[!temp$tHead==0,]
      temp$ID <- paste(temp$From.Country.Code, temp$Item.Code, temp$Year)
      for(item in temp$Item){
        addCBS$Exports[addCBS$Item==item] <- temp$tHead[temp$Item==item]
        # add exports to production (see above)
        addCBS$Production[addCBS$Item==item] <- addCBS$Production[addCBS$Item==item] + temp$tHead[temp$Item==item]
      }
    }
    
    #---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---
    # calculate TotalSupply and Balancing
    #---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---
    # avoid negatives in production and trade
    addCBS[addCBS[,6:16]<0, 6:16] <- 0
    # calculate total supply
    addCBS$TotalSupply <- addCBS$Production + addCBS$Imports
    # Reduce exports where it surpasses total supply
    addCBS$Exports[addCBS$Exports > addCBS$TotalSupply] <- addCBS$TotalSupply[addCBS$Exports>addCBS$TotalSupply]
    # Reduce processing where it surpasses total supply
    addCBS$Processing[addCBS$Processing > addCBS$TotalSupply] <- addCBS$TotalSupply[addCBS$Processing>addCBS$TotalSupply]
    # Reduce Seed where it surpasses total supply
    addCBS$Seed[addCBS$Seed > addCBS$TotalSupply] <- addCBS$TotalSupply[addCBS$Seed>addCBS$TotalSupply]
    # Reduce exports where it surpasses available supply after processing and seed
    addCBS$Exports[addCBS$Exports > (addCBS$TotalSupply - addCBS$Processing - addCBS$Seed)] <- 
      addCBS$TotalSupply[addCBS$Exports > (addCBS$TotalSupply - addCBS$Processing - addCBS$Seed)] - 
      addCBS$Processing[addCBS$Exports > (addCBS$TotalSupply - addCBS$Processing - addCBS$Seed)] - 
      addCBS$Seed[addCBS$Exports > (addCBS$TotalSupply - addCBS$Processing - addCBS$Seed)]
    # Balance the new CBS, where exports are bigger than the supply (prod + imp)
    addCBS$Balancing[rowSums(addCBS[, 10:17])>(addCBS$TotalSupply)] <- 
      addCBS$TotalSupply[rowSums(addCBS[, 10:17])>(addCBS$TotalSupply)] - rowSums(addCBS[, 10:17])[rowSums(addCBS[, 10:17])>(addCBS$TotalSupply)]
    # Adjust processing use where higher than total supply
    addCBS$Processing[addCBS$Processing>(addCBS$TotalSupply-addCBS$Exports)] <- addCBS$TotalSupply[addCBS$Processing>(addCBS$TotalSupply-addCBS$Exports)] - 
      addCBS$Exports[addCBS$Processing>(addCBS$TotalSupply-addCBS$Exports)] - addCBS$Seed[addCBS$Processing>(addCBS$TotalSupply-addCBS$Exports)] - 
      addCBS$Balancing[addCBS$Processing>(addCBS$TotalSupply-addCBS$Exports)]
    #---#---#---#---#---#---#---#---#---#---
    # Allocate supply (Prod + Imp - Exp - Bal) to Uses
    #---#---#---#---#---#---#---#---#---#---
    # For hops and live animals: Processing use = Production + Imports - Exports - Balancing
    commodities <- c(677, 866, 946, 976, 1016, 1034, 2029, 1096, 1107, 1110, 1126, 1157, 1140, 1150, 1171)
    addCBS$Processing[addCBS$Item.Code %in% commodities] <- addCBS$TotalSupply[addCBS$Item.Code %in% commodities] - 
      rowSums(addCBS[addCBS$Item.Code %in% commodities, 10:17])
    # For non-food crops (fibres, tobacco etc.): Other use = Production + Imports - Exports - Balancing
    commodities <- c(2662,2663,2664,2665,2666,2667,2671,2672,2659,2661,2746,2748,2747)
    addCBS$OtherUses[addCBS$Item.Code %in% commodities] <- addCBS$TotalSupply[addCBS$Item.Code %in% commodities] - 
      rowSums(addCBS[addCBS$Item.Code %in% commodities, 10:17])
    # For feed crops: Feed = Production + Imports - Exports - Balancing - Seed - Processing
    commodities <- c(2536,2537,2555,2559,2544,2590,2591,2592,2593,2594,2595,2596,2597,2598)
    addCBS$Feed[addCBS$Item.Code %in% commodities] <- addCBS$TotalSupply[addCBS$Item.Code %in% commodities] - 
      rowSums(addCBS[addCBS$Item.Code %in% commodities, 10:17])
    # For the rest: assume food use: Food = Production + Imports - Exports - Balancing - Seed - Processing
    addCBS$Food <- addCBS$TotalSupply - rowSums(addCBS[,10:17])
    
    #---#---#---#---#---#---#---#---#---#---
    # integrate addCBS into addCBS_all data.frame
    #---#---#---#---#---#---#---#---#---#---
    addCBS_all <- rbind(addCBS_all,addCBS)
  }
  
  #---#---#---#---#---#---#---#---#---#---
  # integrate addCBS into existing CBS data.frame
  #---#---#---#---#---#---#---#---#---#---
  addCBS_all$Original <- 0
  CBS$Original <- 1
  CBS <- rbind(CBS,addCBS_all)
  # remove duplicated rows (i.e. palm oil, oil palm fruit and seed cotton, which were already included in the CBS in step 1b)
  CBS <- CBS[!duplicated(CBS[,1:5]),]
  
  save(CBS, file = paste0("/mnt/nfs_fineprint/tmp/fabio/data/yearly/",year,"_CBS_est.RData"))
}
