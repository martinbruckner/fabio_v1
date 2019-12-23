##############################################################################################
##  
##  FABIO: BUILD MRIO TABLE BASED ON FAOSTAT COMMODITY BALANCE SHEETS AND TRADE DATA
##    2 Build Supply and Use tables
##  
##############################################################################################

library(reshape2)
library(data.table)

rm(list=ls()); gc()

##########################################################################
# Make intitial settings
##########################################################################
# read region classification
regions <- read.csv(file="./inst/fabio_input/Regions.csv", header=TRUE, sep=";")
# read commodity classification
items <- read.csv(file="./inst/fabio_input/Items.csv", header=TRUE, sep=";")
# read technical conversion factors (TCF)
TCF <- read.csv(file="./inst/fabio_input/TCF_use.csv", header=TRUE, sep=";")
# read livestock data
load(file = "/mnt/nfs_fineprint/tmp/fabio/data/Lvst.RData")
load(file="/mnt/nfs_fineprint/tmp/fabio/data/Prod_lvst.RData")
Lvst_all <- Lvst
Prod_lvst_all <- Prod_lvst


##########################################################################
# Start loop for a series of years
##########################################################################
# year=1986
# year=2012
# for(year in 1986:2013){
fabio_use <- function(year, Prod_lvst_all, Lvst, regions, items, TCF){
  print(year)
  is.finite.data.frame <- function(x) do.call(cbind, lapply(x, is.finite))
  require(tidyverse)
  #-------------------------------------------------------------------------
  # Read data
  #-------------------------------------------------------------------------
  load(file=paste0("/mnt/nfs_fineprint/tmp/fabio/data/yearly/",year,"_CBS_balanced.RData"))
  load(file=paste0("/mnt/nfs_fineprint/tmp/fabio/data/yearly/",year,"_sup.RData"))
  load(file=paste0("/mnt/nfs_fineprint/tmp/fabio/data/yearly/",year,"_sup_usd.RData"))
  load(file=paste0("/mnt/nfs_fineprint/tmp/fabio/data/yearly/",year,"_BTD_balanced.RData"))
  # read use structure
  use <- read.csv(file="./inst/fabio_input/Items_use.csv", header=TRUE, sep=";")
  use[,as.character(regions$ISO)] <- 0
  
  
  #-------------------------------------------------------------------------
  # allocation of 100% crops (i.e. crops that directly go into a specific process such as oil crops and hops)
  #-------------------------------------------------------------------------
  # iso="USA"
  for(iso in as.character(regions$ISO)){
    region.code <- regions$Country.Code[regions$ISO==iso]
    # item=328
    for(item in use$Item.Code[use$Type=="100%"]){
      if(item %in% CBS$Item.Code[CBS$Country.Code==region.code]){
        use[use$Item.Code==item & use$Type=="100%", iso] <- CBS$Processing[CBS$Country.Code==region.code & CBS$Item.Code==item]
        CBS$Processing[CBS$Country.Code==region.code & CBS$Item.Code==item] <- 0
      }
    }
  }
  
  
  #-------------------------------------------------------------------------
  # allocation of live animals to slaughtering, rest to husbandry
  # (producing animals =  imported and domestically grown animals slaughtered in a certain country)
  #-------------------------------------------------------------------------
  # Prod_lvst <- Prod_lvst_all[Prod_lvst_all$Element=="Producing Animals/Slaughtered" & Prod_lvst_all$Year==year,]
  # Prod_lvst$Value[Prod_lvst$Unit=="Head"] <- Prod_lvst$Value[Prod_lvst$Unit=="Head"] / 1000
  # Prod_lvst$Unit[Prod_lvst$Unit=="Head"] <- "1000 Head"
  # concordance <- data.frame(Item.Code.lvst = c(1127,867,1017,977,1808,1035,1097,1141,947,1158,1151,1108,1111),
  #                           Item.Code = c(1126,866,1016,976,2029,1034,1096,1140,946,1157,1150,1107,1110),
  #                           Item = c("Camels","Cattle","Goats","Sheep","Poultry Birds","Pigs","Horses","Rabbits and hares","Buffaloes",
  #                                    "Camelids, other","Rodents, other","Asses","Mules"))
  # names(Prod_lvst)[3:4] <- c("Item.Code.lvst","Item.lvst")
  # Prod_lvst <- merge(Prod_lvst, concordance, by="Item.Code.lvst", all.x = T)
  # Prod_lvst <- Prod_lvst[!is.na(Prod_lvst$Item.Code),]
  # # region="USA"
  # for(region in regions$ISO){
  #   region.code <- regions$Country.Code[regions$ISO==region]
  #   # item=866
  #   for(item in use$Item.Code[use$Type=="slaughtering"]){
  #     if(item %in% Prod_lvst$Item.Code[Prod_lvst$Country.Code==region.code]){
  #       use[use$Item.Code==item & use$Type=="slaughtering", region] <- Prod_lvst$Value[Prod_lvst$Item.Code==item & Prod_lvst$Country.Code==region.code]
  #       CBS$Processing[CBS$Country.Code==region.code & CBS$Item.Code==item] <- CBS$Processing[CBS$Country.Code==region.code & CBS$Item.Code==item] -
  #         use[use$Item.Code==item & use$Type=="slaughtering", region]
  #     }
  #     if(item %in% CBS$Item.Code[CBS$Country.Code==region.code]){
  #       use[use$Item.Code==item & use$Type=="slaughtering", region] <- CBS$Processing[CBS$Country.Code==region.code & CBS$Item.Code==item]
  #       CBS$Processing[CBS$Country.Code==region.code & CBS$Item.Code==item] <- 0
  #     }
  #     # avoid negative values and reduce instead the producing animals used in slaughtering
  #     if(use[use$Item.Code==item & use$Type=="animal husbandry", region] < 0){
  #       use[use$Item.Code==item & use$Type=="slaughtering", region] <- use[use$Item.Code==item & use$Type=="slaughtering", region] +
  #         use[use$Item.Code==item & use$Type=="animal husbandry", region]
  #       use[use$Item.Code==item & use$Type=="animal husbandry", region] <- 0
  #     }
  #   }
  # }
  
  #-------------------------------------------------------------------------
  # allocation of all live animals to slaughtering (domestically produced and imported animals)
  #-------------------------------------------------------------------------
  # region="USA"
  for(region in regions$ISO){
    region.code <- regions$Country.Code[regions$ISO==region]
    # item=866
    for(item in use$Item.Code[use$Type=="slaughtering"]){
      if(item %in% CBS$Item.Code[CBS$Country.Code==region.code]){
        use[use$Item.Code==item & use$Type=="slaughtering", region] <- CBS$Processing[CBS$Country.Code==region.code & CBS$Item.Code==item]
        CBS$Processing[CBS$Country.Code==region.code & CBS$Item.Code==item] <- 0
      }
    }
  }
  
  
  #-------------------------------------------------------------------------
  # allocation of TCF crops (i.e. crop uses that are estimated based on the process output and the respective technical conversion factor)
  #-------------------------------------------------------------------------
  # region="AFG"
  for(region in regions$ISO){
    region.code <- regions$Country.Code[regions$ISO==region]
    # process="p064"
    for(process in unique(use$Proc.Code[use$Type=="TCF"])){
      item <- use$Item.Code[use$Proc.Code==process & use$Type=="TCF"]
      prod <- unique(TCF$Prod.Code[TCF$Proc.Code==process])
      supply <- CBS$Production[CBS$Country.Code==region.code & CBS$Item.Code==prod]
      if(length(item)==1 & length(supply)>0){
        use[use$Proc.Code==process & use$Type=="TCF" & use$Item.Code==item, region] <- 
          supply / TCF[TCF$Proc.Code==process & TCF$Prod.Code==prod, region]
        if(use[use$Proc.Code==process & use$Type=="TCF" & use$Item.Code==item, region] > CBS$Processing[CBS$Country.Code==region.code & CBS$Item.Code==item])
          use[use$Proc.Code==process & use$Type=="TCF" & use$Item.Code==item, region] <- CBS$Processing[CBS$Country.Code==region.code & CBS$Item.Code==item]
        # reduce values in CBS by those used here
        CBS$Processing[CBS$Country.Code==region.code & CBS$Item.Code==item] <- CBS$Processing[CBS$Country.Code==region.code & CBS$Item.Code==item] - 
          use[use$Proc.Code==process & use$Type=="TCF" & use$Item.Code==item, region]
      } else if(length(item)>1 & length(supply)>0){
        if(supply>0){
          temp <- data.frame(Processing = CBS$Processing[CBS$Country.Code==region.code & CBS$Item.Code %in% item],
                             Item.Code = CBS$Item.Code[CBS$Country.Code==region.code & CBS$Item.Code %in% item],
                             Production = CBS$Production[CBS$Country.Code==region.code & CBS$Item.Code==prod])
          temp$TCF <- 0
          for(code in temp$Item.Code){
            temp$TCF[temp$Item.Code==code] <- as.numeric(TCF[TCF$Item.Code==code & TCF$Proc.Code==process, region])
          }
          # calculate potential production of 'prod' with each input 'item' in a specific process 'proc'
          temp$pot <- temp$Processing * temp$TCF
          if(sum(temp$pot) > temp$Production[1]){
            temp$use <- temp$Processing / sum(temp$pot) * temp$Production
          } else temp$use <- temp$Processing
          for(code in temp$Item.Code){
            use[use$Proc.Code==process & use$Item.Code==code, region] <- temp$use[temp$Item.Code==code]
            if(use[use$Proc.Code==process & use$Item.Code==code, region] > CBS$Processing[CBS$Country.Code==region.code & CBS$Item.Code==code])
              use[use$Proc.Code==process & use$Item.Code==code, region] <- CBS$Processing[CBS$Country.Code==region.code & CBS$Item.Code==code]
            # reduce values in CBS by those used here
            CBS$Processing[CBS$Country.Code==region.code & CBS$Item.Code==code] <- CBS$Processing[CBS$Country.Code==region.code & CBS$Item.Code==code] - 
              use[use$Proc.Code==process & use$Item.Code==code, region]
          }
        }
      }
    }
  }
  
  
  #-------------------------------------------------------------------------
  # Allocation of ethanol feedstock use
  #-------------------------------------------------------------------------
  # Define Bio-Ethanol feedstocks
  TCFfeedstocks <- data.frame(Item.Code=c(2511,2513,2514,2515,2518,2520,2531,2532,2536,2537,2544,2617,2620,2625,2655,2804),
                              Item=c("Wheat and products","Barley and products","Maize and products","Rye and products","Sorghum and products",
                                     "Cereals, Other","Potatoes and products","Cassava and products","Sugar cane","Sugar beet","Molasses",
                                     "Apples and products","Grapes and products (excl wine)","Fruits, Other","Wine","Rice (Paddy Equivalent)"),
                              TCF=c(0.28,0.2044,0.35,0.27,0.294,0.275,0.09,0.09,0.075,0.065,0.19934,0.19934,0.19934,0.19934,0.19934,0.297),
                              # shares of ethanol production from each crop for the two major European (FR,DE) and global producers (US,BR)
                              FRA=c(0.17,0,0.12,0,0,0,0,0,0,0.71,0,0,0,0,0,0), # source: IISD.org
                              DEU=c(0.27,0.1,0.1,0.23,0,0,0,0,0,0.3,0,0,0,0,0,0), # source: destatis
                              USA=c(0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0), # source: EIA
                              BRA=c(0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0)) # source: USD GAIN Report 
  # estimate the use of different feedstocks based on their availability according to the CBS
  feedstocks <- CBS[CBS$Item.Code %in% TCFfeedstocks$Item.Code,]
  feedstocks <- merge(feedstocks, TCFfeedstocks[,c(1,3)], all.x=TRUE)
  feedstocks$pot.ethanol.o <- feedstocks$OtherUses * feedstocks$TCF
  feedstocks$pot.ethanol.p <- feedstocks$Processing * feedstocks$TCF
  feedstocks$pot.ethanol.o[feedstocks$pot.ethanol.o<0] <- 0
  feedstocks$pot.ethanol.p[feedstocks$pot.ethanol.p<0] <- 0
  pot.total.o <- stats::aggregate(pot.ethanol.o ~ Country.Code, feedstocks[,c("Country.Code","pot.ethanol.o")], sum)
  pot.total.p <- stats::aggregate(pot.ethanol.p ~ Country.Code, feedstocks[,c("Country.Code","pot.ethanol.p")], sum)
  names(pot.total.o)[2] <- "pot.total.o"
  names(pot.total.p)[2] <- "pot.total.p"
  feedstocks <- merge(feedstocks, pot.total.o, by="Country.Code", all.x=TRUE)
  feedstocks <- merge(feedstocks, pot.total.p, by="Country.Code", all.x=TRUE)
  feedstocks$share.o <- feedstocks$pot.ethanol.o / feedstocks$pot.total.o
  feedstocks$share.p <- feedstocks$pot.ethanol.p / feedstocks$pot.total.p
  feedstocks[,-(1:6)][is.na(feedstocks[,-(1:6)])] <- 0
  feedstocks <- merge(feedstocks, CBS[CBS$Item.Code==2659,c(1,6)], by="Country.Code", all.x=TRUE)
  feedstocks$Production.y[!is.finite(feedstocks$Production.y)] <- 0
  # first use up reported "other use" of feedstocks
  # if not enough, use also "processing use"
  feedstocks$feedstock.o <- feedstocks$Production.y * feedstocks$share.o / feedstocks$TCF
  feedstocks$feedstock.o[feedstocks$feedstock.o>feedstocks$OtherUses] <- feedstocks$OtherUses[feedstocks$feedstock.o>feedstocks$OtherUses]
  feedstocks$feedstock.p <- 0
  feedstocks$feedstock.p[feedstocks$pot.total.o<feedstocks$Production.y] <- 
    (feedstocks$Production.y[feedstocks$pot.total.o<feedstocks$Production.y] - feedstocks$pot.total.o[feedstocks$pot.total.o<feedstocks$Production.y]) * 
    feedstocks$share.p[feedstocks$pot.total.o<feedstocks$Production.y] / feedstocks$TCF[feedstocks$pot.total.o<feedstocks$Production.y]
  feedstocks$feedstock.p[feedstocks$feedstock.p > feedstocks$Processing] <- feedstocks$Processing[feedstocks$feedstock.p > feedstocks$Processing]
  
  # replace estimates for US (231) and BR (21) with reported shares
  # (do not replace estimates for FR (68) and DE (79), as the feedstock assumptions are too uncertain)
  for(i in TCFfeedstocks$Item.Code){
    # if(length(feedstocks[feedstocks$Country.Code==68 & feedstocks$Item.Code==i,]$share.o)>0)
    #   feedstocks[feedstocks$Country.Code==68 & feedstocks$Item.Code==i,]$share.o <- TCFfeedstocks$FRA[TCFfeedstocks$Item.Code==i]
    # if(length(feedstocks[feedstocks$Country.Code==79 & feedstocks$Item.Code==i,]$share.o)>0)
    #   feedstocks[feedstocks$Country.Code==79 & feedstocks$Item.Code==i,]$share.o <- TCFfeedstocks$DEU[TCFfeedstocks$Item.Code==i]
    if(length(feedstocks[feedstocks$Country.Code==231 & feedstocks$Item.Code==i,]$share.o)>0)
      feedstocks[feedstocks$Country.Code==231 & feedstocks$Item.Code==i,]$share.o <- TCFfeedstocks$USA[TCFfeedstocks$Item.Code==i]
    if(length(feedstocks[feedstocks$Country.Code==21 & feedstocks$Item.Code==i,]$share.o)>0)
      feedstocks[feedstocks$Country.Code==21 & feedstocks$Item.Code==i,]$share.o <- TCFfeedstocks$BRA[TCFfeedstocks$Item.Code==i]
    # if(length(feedstocks[feedstocks$Country.Code==68 & feedstocks$Item.Code==i,]$share.p)>0)
    #   feedstocks[feedstocks$Country.Code==68 & feedstocks$Item.Code==i,]$share.p <- TCFfeedstocks$FRA[TCFfeedstocks$Item.Code==i]
    # if(length(feedstocks[feedstocks$Country.Code==79 & feedstocks$Item.Code==i,]$share.p)>0)
    #   feedstocks[feedstocks$Country.Code==79 & feedstocks$Item.Code==i,]$share.p <- TCFfeedstocks$DEU[TCFfeedstocks$Item.Code==i]
    if(length(feedstocks[feedstocks$Country.Code==231 & feedstocks$Item.Code==i,]$share.p)>0)
      feedstocks[feedstocks$Country.Code==231 & feedstocks$Item.Code==i,]$share.p <- TCFfeedstocks$USA[TCFfeedstocks$Item.Code==i]
    if(length(feedstocks[feedstocks$Country.Code==21 & feedstocks$Item.Code==i,]$share.p)>0)
      feedstocks[feedstocks$Country.Code==21 & feedstocks$Item.Code==i,]$share.p <- TCFfeedstocks$BRA[TCFfeedstocks$Item.Code==i]
  }
  # for US, BR use exactly the given shares
  # c=21
  for(c in c(231,21)){
    temp <- feedstocks[feedstocks$Country.Code==c,]
    temp$feedstock.o <- temp$Production.y * temp$share.o / temp$TCF
    temp$feedstock.p <- 0
    temp$feedstock.p[temp$feedstock.o > temp$OtherUses] <- temp$feedstock.o[temp$feedstock.o > temp$OtherUses] - temp$OtherUses[temp$feedstock.o > temp$OtherUses]
    temp$feedstock.o[temp$feedstock.o > temp$OtherUses] <- temp$OtherUses[temp$feedstock.o > temp$OtherUses]
    temp$feedstock.p[temp$feedstock.p > temp$Processing] <- temp$Processing[temp$feedstock.p > temp$Processing]
    feedstocks <- rbind(feedstocks[!feedstocks$Country.Code==c,],temp)
  }
  # write.table(feedstocks,file="feedstocks.csv",sep=";")
  rm(TCFfeedstocks,pot.total.o,pot.total.p,temp,c,i)
  # Allocation of bio-ethanol feedstocks to use table
  # region="USA"
  for(region in regions$ISO){
    region.code <- regions$Country.Code[regions$ISO==region]
    # item=2518
    for(item in feedstocks$Item.Code[feedstocks$Country.Code==region.code]){
      use[use$Process=="Alcohol production, Non-Food" & use$Item.Code==item, region] <- 
        feedstocks$feedstock.o[feedstocks$Country.Code==region.code & feedstocks$Item.Code==item] + 
        feedstocks$feedstock.p[feedstocks$Country.Code==region.code & feedstocks$Item.Code==item]
      # reduce values in CBS by those used for ethanol production (rest will be used for alcoholic beverages & other uses)
      CBS$Processing[CBS$Country.Code==region.code & CBS$Item.Code==item] <- CBS$Processing[CBS$Country.Code==region.code & CBS$Item.Code==item] - 
        feedstocks$feedstock.p[feedstocks$Country.Code==region.code & feedstocks$Item.Code==item]
      CBS$OtherUses[CBS$Country.Code==region.code & CBS$Item.Code==item] <- CBS$OtherUses[CBS$Country.Code==region.code & CBS$Item.Code==item] -
        feedstocks$feedstock.o[feedstocks$Country.Code==region.code & feedstocks$Item.Code==item]
    }
  }
  
  
  
  #-------------------------------------------------------------------------
  # Allocation of feed uses
  #-------------------------------------------------------------------------
  feedsupply <- CBS[CBS$Feed>0,c(1:5,12)]
  # get moisture content from use table
  feedsupply <- merge(feedsupply, items[,c(2,6,7)], by="Item.Code", all.x = T)
  # convert into dry matter
  feedsupply$DM <- feedsupply$Feed * (1 - feedsupply$Moisture)
  
  #--------------------------------------------------------------------
  # estimate feed for horses et al. (based on Krausmann et al. 2008)
  #--------------------------------------------------------------------
  # read conversion rates
  conversion_K <- data.frame(Proc.Code = c("p091","p092","p093","p094","p095","p096","p097","p098"),
                     Process = c("Horses husbandry", "Asses husbandry", "Mules husbandry", "Camels husbandry", "Camelids husbandry, other", 
                                 "Rabbits husbandry", "Rodents husbandry, other", "Live animals husbandry, other"),
                     Item.Code = c(1096, 1107, 1110, 1126, 1157, 1140, 1150, 1171),
                     Item = c("Horses", "Asses", "Mules", "Camels", "Camelids, other", "Rabbits and hares", 
                              "Rodents, other", "Animals live nes"),
                     value = c(10,6,6,10,10,0.1,0.1,5)*365/1000)  # kg dry matter/head/day converted into t DM/head/year
  
  # prepare dataframe with information on feed requirements for each meat and milk item and country
  commodities <- unique(sup[sup$Item.Code %in% c(866:1171,2029,2731:2737,2748:2749,843), 1:5])
  feedrequ_K <- cbind(regions[rep(seq_len(nrow(regions)),each=nrow(commodities)),1:3], commodities)
  feedrequ_K$Crops <- 0
  feedrequ_K$Grass <- 0
  feedrequ_K$Residues <- 0
  feedrequ_K$Scavenging <- 0
  feedrequ_K$Fodder <- 0
  feedrequ_K$Animalproducts <- 0
  feedrequ_K$Total <- 0
  
  # calculate feed requirements with Krausmann values (no distinction between crops and grass)
  # region="USA"
  for(region in regions$ISO){
    region.code <- regions$Country.Code[regions$ISO==region]
    # item=1107
    for(item in Lvst$Item.Code[Lvst$Country.Code==region.code & Lvst$Year==year & Lvst$Item.Code %in% conversion_K$Item.Code]){
      process <- as.character(conversion_K$Proc.Code[conversion_K$Item.Code==item])
      feedrequ_K$Total[feedrequ_K$Proc.Code==process & feedrequ_K$Country.Code==region.code] <- 
        Lvst$Value[Lvst$Country.Code==region.code & Lvst$Item.Code==item & Lvst$Year==year] * 
        conversion_K$value[conversion_K$Proc.Code==process]
      if(Lvst$Unit[Lvst$Country.Code==region.code & Lvst$Item.Code==item & Lvst$Year==year]=="1000 Head"){
        feedrequ_K$Total[feedrequ_K$Proc.Code==process & feedrequ_K$Country.Code==region.code] <- 
          feedrequ_K$Total[feedrequ_K$Proc.Code==process & feedrequ_K$Country.Code==region.code] * 1000
      }
    }
  }
  
  feedrequ_K <- feedrequ_K[feedrequ_K$Total>0,c(1,3,7,4,9:15)]
  
  #--------------------------------------------------------------------
  # estimate feed for cattle et al. (based on Bouwman et al. 2013)
  #--------------------------------------------------------------------
  # read conversion rates
  conversion_B <- read.csv(file="./inst/fabio_input/Feed conversion rates_Bouwman.csv", header=TRUE, sep=";", stringsAsFactors = F)
  # find closest higher and lower years in conversion in relation to the current year 
  closest<-function(xv,sv){
    unique(xv[which(abs(xv-sv)==min(abs(xv-sv)))]) }
  closest_high<-function(xv,sv){
    closest(xv[which(xv>sv)],sv)}
  closest_low<-function(xv,sv){
    closest(xv[which(xv<sv)],sv)}
  
  high <- closest_high(conversion_B$Year,year)
  low <- closest_low(conversion_B$Year,year)
  # interpolate conversion data to current year
  temp <- conversion_B$Coefficient[conversion_B$Year==low] + 
    (conversion_B$Coefficient[conversion_B$Year==high] - conversion_B$Coefficient[conversion_B$Year==low]) / (high - low) * (year - low)
  conversion_B <- conversion_B[conversion_B$Year==low,]
  conversion_B$Coefficient <- temp
  conversion_B$Year <- year
  conversion_B <- conversion_B %>% 
    spread(Feed, Coefficient)
  conversion_B[is.na(conversion_B)] <- 0
  names(conversion_B)[5:6] <- c("Animalproducts", "Crops")
  
  # calculate feed demand
  feedrequ_B <- Prod_lvst_all[Prod_lvst_all$Element=="Production" & Prod_lvst_all$Unit=="tonnes" & Prod_lvst_all$Year==year,]
  reg_feed <- read.csv(file="./inst/fabio_input/Regions_feed_Bouwman.csv", header=TRUE, sep=";")
  feedrequ_B$Region.Code <- reg_feed$Region.Code[match(feedrequ_B$Country.Code,reg_feed$Country.Code)]
  feedrequ_B$ISO <- regions$ISO[match(feedrequ_B$Country.Code,regions$Country.Code)]
  feedrequ_B$ISO[is.na(feedrequ_B$ISO)] <- "ROW"
  feedrequ_B$Country[is.na(feedrequ_B$Country)] <- "RoW"
  concordance <- data.frame(Item.Code.lvst = c(944,972,1012,1032,1055,1775, 882,951,982,1020,1130),
                            Item.lvst = c("Meat indigenous, cattle","Meat indigenous, buffalo","Meat indigenous, sheep","Meat indigenous, goat",
                                          "Meat indigenous, pig","Meat indigenous, poultry","Milk, whole fresh cow",
                                          "Milk, whole fresh buffalo","Milk, whole fresh sheep",
                                          "Milk, whole fresh goat", "Milk, whole fresh camel"),
                            Item.Code = c(866,946,976,1016,1034,2029, 2848,2848,2848,2848,2848),
                            Item = c("Cattle","Buffaloes","Sheep","Goats","Pigs","Poultry Birds", "Milk - Excluding Butter",
                                     "Milk - Excluding Butter","Milk - Excluding Butter","Milk - Excluding Butter","Milk - Excluding Butter"),
                            Proc.Code=c("p085","p086","p087","p088","p089","p090","p099","p100","p101","p102","p103"),
                            Animal=c("Beef cattle","Beef cattle","Sheep and goats","Sheep and goats","Pigs","Poultry","Dairy cattle","Dairy cattle"
                                     ,"Dairy cattle","Dairy cattle","Dairy cattle"),
                            stringsAsFactors = F)
  names(feedrequ_B)[3:4] <- names(concordance)[1:2]
  feedrequ_B <- merge(feedrequ_B, concordance[,-2], by="Item.Code.lvst")
  feedrequ_B <- merge(feedrequ_B, conversion_B, by=c("Animal","Region.Code","Year"))
  # assume a share of fodder crops in residues of 90% for ruminants, 10% for pigs and 0% poultry
  feedrequ_B$Fodder <- feedrequ_B$Residues * 0.9
  feedrequ_B$Fodder[feedrequ_B$Animal=="Pigs"] <- feedrequ_B$Residues[feedrequ_B$Animal=="Pigs"] * 0.1
  feedrequ_B$Fodder[feedrequ_B$Animal=="Poultry"] <- feedrequ_B$Fodder[feedrequ_B$Animal=="Poultry"] * 0
  feedrequ_B$Residues <- feedrequ_B$Residues - feedrequ_B$Fodder
  # multiply feed requirement coefficients with meat and milk production values
  feedrequ_B[,c("Animalproducts", "Crops","Grass","Residues","Fodder","Scavenging")] <- 
    feedrequ_B[,c("Animalproducts", "Crops","Grass","Residues","Fodder","Scavenging")] * feedrequ_B$Value
  feedrequ_B$Country.Code[! feedrequ_B$Country.Code %in% regions$Country.Code] <- 999
  feedrequ_B <- feedrequ_B %>% 
    dplyr::select(-Country, -Element.Code, -Element, -Unit, -Animal, -Region, -Region.Code, -Item.Code.lvst, -Item.lvst, -Value, -Year) %>% 
    dplyr::group_by(Country.Code, ISO, Item.Code, Proc.Code) %>% 
    dplyr::summarise(Crops = sum(Crops), Grass = sum(Grass), Residues = sum(Residues), Scavenging = sum(Scavenging), Fodder = sum(Fodder), Animalproducts = sum(Animalproducts)) %>% 
    dplyr::mutate(Total = Crops + Grass + Residues + Scavenging + Fodder + Animalproducts) %>% 
    as.data.frame()
  
  # allocate total feed demand estimated with Krausmann-approach to crops, grass, residues etc. according to the average feed distribution based on Bouwman-approach
  # region="USA"
  for(region in unique(feedrequ_K$ISO)){
    sums <- colSums(feedrequ_B[feedrequ_B$ISO==region, c(5:10)])
    total <- sum(feedrequ_B$Total[feedrequ_B$ISO==region])
    # for countries where no cattle, poultry and pigs are produced, assume the following feed types
    if(total==0){
      sums <- c(0.3,0.4,0.1,0.1,0.1,0)
      names(sums) <- c("Crops","Grass","Residues","Scavenging","Fodder","Animalproducts")
      total <- sum(sums)
    }
    #i="Crops"
    for(i in names(sums)){
      feedrequ_K[feedrequ_K$ISO==region, i] <- feedrequ_K$Total[feedrequ_K$ISO==region] / total * sums[i]
    }
  }
  
  # integrate Krausmann and Bouwman feed balances
  feedrequ <- rbind(feedrequ_K, feedrequ_B)
  
  
  # adapt feed crop demand in order to comply with feed crops supply
  # region=231
  for(region in unique(feedrequ$Country.Code)){
    cropsupply <- sum(feedsupply$DM[feedsupply$Feedtype=="Feed crops" & feedsupply$Country.Code==region])
    cropdemand <- sum(feedrequ$Crops[feedrequ$Country.Code==region])
    # process="p085"
    for(process in unique(feedrequ$Proc.Code[feedrequ$Country.Code==region])){
      feedrequ$Crops[feedrequ$Country.Code==region & feedrequ$Proc.Code==process] <-  
        feedrequ$Crops[feedrequ$Country.Code==region & feedrequ$Proc.Code==process] / cropdemand * cropsupply
    }
  }
  
  # adapt animal feed demand in order to comply with animal product supply
  # region=231
  for(region in unique(feedrequ$Country.Code)){
    animalsupply <- sum(feedsupply$DM[feedsupply$Feedtype=="Animal products" & feedsupply$Country.Code==region])
    animaldemand <- sum(feedrequ$Animalproducts[feedrequ$Country.Code==region])
    # process="p085"
    for(process in unique(feedrequ$Proc.Code[feedrequ$Country.Code==region])){
      feedrequ$Animalproducts[feedrequ$Country.Code==region & feedrequ$Proc.Code==process] <- 
        feedrequ$Animalproducts[feedrequ$Country.Code==region & feedrequ$Proc.Code==process] / animaldemand * animalsupply
    }
  }
  
  # adapt fodder crop demand in order to comply with fodder crop supply
  # region=231
  for(region in unique(feedrequ$Country.Code)){
    foddercropsupply <- sum(feedsupply$DM[feedsupply$Item=="Fodder crops" & feedsupply$Country.Code==region])
    foddercropdemand <- sum(feedrequ$Fodder[feedrequ$Country.Code==region])
    # process="p085"
    for(process in unique(feedrequ$Proc.Code[feedrequ$Country.Code==region])){
      # if(foddercropsupply>foddercropdemand){
        feedrequ$Fodder[feedrequ$Country.Code==region & feedrequ$Proc.Code==process] <- 
          feedrequ$Fodder[feedrequ$Country.Code==region & feedrequ$Proc.Code==process] / foddercropdemand * foddercropsupply
      # }
    }
  }
  
  feedrequ[is.na(feedrequ)] <- 0
  # feedrequ$check <- round((feedrequ$Total - (feedrequ$Crops+feedrequ$Residues+feedrequ$Grass+feedrequ$Scavenging+feedrequ$Animalproducts)) / feedrequ$Total * 100, 0)
  #--------------------------------------------------------------------
  # Allocation of crop and animal feed use
  #--------------------------------------------------------------------
  feeduse <- use[use$Type=="feed",]
  # region=231
  for(region in unique(feedrequ$Country.Code)){
    iso <- as.character(regions$ISO[regions$Country.Code==region])
    totalcrops <- sum(feedsupply$DM[feedsupply$Feedtype=="Feed crops" & feedsupply$Country.Code==region])
    totalanimal <- sum(feedsupply$DM[feedsupply$Feedtype=="Animal products" & feedsupply$Country.Code==region])
    # process="p085"
    for(process in unique(feedrequ$Proc.Code[feedrequ$Country.Code==region])){
      feed <- "Feed crops"
      item <- unique(feedsupply$Item.Code[feedsupply$Feedtype==feed])
      demand <- feedrequ$Crops[feedrequ$Country.Code==region & feedrequ$Proc.Code==process]
      supply <- feedsupply[feedsupply$Feedtype==feed & feedsupply$Country.Code==region & feedsupply$Item.Code %in% feeduse$Item.Code[feeduse$Proc.Code==process],]
      feeduse[feeduse$Proc.Code==process & feeduse$Item.Code %in% supply$Item.Code,iso] <- supply$DM[match(feeduse$Item.Code[feeduse$Proc.Code==process & feeduse$Item.Code %in% supply$Item.Code],supply$Item.Code)] * demand / totalcrops
      
      feed <- "Animal products"
      item <- unique(feedsupply$Item.Code[feedsupply$Feedtype==feed])
      demand <- feedrequ$Animalproducts[feedrequ$Country.Code==region & feedrequ$Proc.Code==process]
      supply <- feedsupply[feedsupply$Feedtype==feed & feedsupply$Country.Code==region & feedsupply$Item.Code %in% feeduse$Item.Code[feeduse$Proc.Code==process],]
      feeduse[feeduse$Proc.Code==process & feeduse$Item.Code %in% supply$Item.Code,iso] <- supply$DM[match(feeduse$Item.Code[feeduse$Proc.Code==process & feeduse$Item.Code %in% supply$Item.Code],supply$Item.Code)] * demand / totalanimal
      
    }
  }
  
  #--------------------------------------------------------------------
  # Allocation of grazing and fodder use
  #--------------------------------------------------------------------
  # region=11
  for(region in unique(feedrequ$Country.Code)){
    iso <- as.character(regions$ISO[regions$Country.Code==region])
    totalfodderdemand <- sum(feedrequ$Fodder[feedrequ$Country.Code==region])
    totalfoddersupply <- sum(feedsupply$DM[feedsupply$Country.Code==region & feedsupply$Item.Code==2000])
    totalgrassdemand <- sum(feedrequ$Grass[feedrequ$Country.Code==region])
    for(process in unique(feedrequ$Proc.Code[feedrequ$Country.Code==region & (feedrequ$Residues + feedrequ$Grass)>0])){
      feed <- "Grass"
      demandfodder <- feedrequ$Fodder[feedrequ$Country.Code==region & feedrequ$Proc.Code==process]
      demandgrass <- feedrequ$Grass[feedrequ$Country.Code==region & feedrequ$Proc.Code==process]
      feeduse[feeduse$Proc.Code==process & feeduse$Item=="Fodder crops",iso] <- demandfodder / totalfodderdemand * totalfoddersupply
      feeduse[feeduse$Proc.Code==process & feeduse$Item=="Grazing",iso] <- demandgrass
    }
  }
  feeduse[!is.finite(feeduse)] <- 0
  
  use[use$Type=="feed",-(1:6)] <- feeduse[,-(1:6)] / (1 - items$Moisture[match(feeduse$Item.Code,items$Item.Code)])
  
  
  #-------------------------------------------------------------------------
  # add grazing to supply table
  #-------------------------------------------------------------------------
  grazing <- feeduse[feeduse$Item=="Grazing",]
  sup[sup$Item=="Grazing",-(1:5)] <- colSums(grazing[,-(1:6)])
  sup_usd[sup_usd$Item=="Grazing",-(1:5)] <- colSums(grazing[,-(1:6)])
  data <- as.data.frame(reshape2::melt(data.table::as.data.table(grazing), id=1:6, variable.name = "ISO"))
  data <- stats::aggregate(value ~ ISO, data, sum)
  CBS$ISO <- regions$ISO[match(CBS$Country.Code,regions$Country.Code)]
  CBS$Production[CBS$Item=="Grazing"] <- data$value[match(CBS$ISO[CBS$Item=="Grazing"],data$ISO)]
  CBS$TotalSupply <- CBS$Production + CBS$Imports
  
  
  #-------------------------------------------------------------------------
  # Allocation of feedstocks for the production of alc. beverages & sweeteners
  #-------------------------------------------------------------------------
  # Prepare I & O for optimization for all countries
  # I = Processing use for 25 selected commodities reduced by pre-allocated processing uses
  # O = Domestic production for 5 selected commodities
  TCF <- read.csv(file="./inst/fabio_input/TCF_optim.csv", header=TRUE, sep=";")
  IN <- data.frame(Item.Code=c(2511,2513,2514,2515,2516,2517,2518,2520,2531,2532,2533,2536,2537,2541,2543,2544,2615,2616,2617,2619,2620,2625,2655,2804,2818),
                   Item=c("Wheat and products","Barley and products","Maize and products","Rye and products","Oats","Millet and products",
                          "Sorghum and products","Cereals, Other","Potatoes and products","Cassava and products","Sweet potatoes","Sugar cane",
                          "Sugar beet","Sugar non-centrifugal","Sweeteners, Other","Molasses","Bananas","Plantains","Apples and products",
                          "Dates","Grapes and products (excl wine)","Fruits, Other","Wine","Rice (Paddy Equivalent)","Sugar, Refined Equiv"))
  # OUT <- data.frame(Item.Code=c(2543,2656,2657,2658,2659),
  #                   Item=c("Sweeteners, Other","Beer","Beverages, Fermented","Beverages, Alcoholic","Alcohol, Non-Food"))
  # "Alcohol, non-food" is obviously not covered with the reported processing inputs, probably because processing is defined as food processing only.
  # Therefore, only the following items are defined as outputs of the processing optimization:
  OUT <- data.frame(Item.Code=c(2543,2656,2657,2658),
                    Item=c("Sweeteners, Other","Beer","Beverages, Fermented","Beverages, Alcoholic"))
  
  #---#---#---#---#---#---#---#---#---#---
  # run optimization
  #---#---#---#---#---#---#---#---#---#---
  print("Start optim")
  results <- list()
  # region = 231  # 231 = USA, 21 = BRA
  for(region in regions$Country.Code){
    iso <- as.character(regions$ISO[regions$Country.Code==region])
    # Processing inputs (alpha)
    # print(paste("optim",iso,region))
    input <- merge(IN, CBS[CBS$Country.Code==region & CBS$Item.Code %in% IN$Item.Code,c(3,13)], by="Item.Code", all.x=TRUE)
    input$Processing[! is.finite(input$Processing)] <- 0
    input <- input[input$Processing > 0,]

    # Processing outputs (beta)
    output <- merge(OUT, CBS[CBS$Country.Code==region & CBS$Item.Code %in% OUT$Item.Code,c(3,6)], by="Item.Code", all.x=TRUE)
    output$Production[! is.finite(output$Production)] <- 0
    output <- output[output$Production > 0,]

    # Technical conversion factors (sigma)
    TCF_ij <- cbind(TCF[,1:4],TCF=TCF[,iso])
    TCF_ij <- TCF_ij[is.finite(TCF_ij$TCF),]
    output_weights <- data.frame(Item.Code=c(2543,2656,2657,2658,2659), weight = stats::aggregate(TCF ~ Output.Code, TCF_ij, mean)[,2])
    TCF_ij <- TCF_ij[TCF_ij$Input.Code %in% input$Item.Code & TCF_ij$Output.Code %in% output$Item.Code,]

    # Remove inputs and outputs, where no TCF is available
    input <- input[input$Item.Code %in% TCF_ij$Input.Code,]
    output <- output[output$Item.Code %in% TCF_ij$Output.Code,]

    # proceed only, if input && output && TCF contain at least one observation each
    if(nrow(input) < 1 || nrow(output) < 1 || nrow(TCF_ij) < 1) {
      next
    } else {

      # Optimization function
      fun <- function(input_ijt){
        input_it <- as.data.frame(cbind(input_ijt,TCF_ij$Input.Code))
        input_it <- as.vector(stats::aggregate(. ~ V2, input_it, sum)[,2])
        output_ijt <- input_ijt*TCF_ij$TCF
        output_jt <-  as.data.frame(cbind(output_ijt,TCF_ij$Output.Code))
        output_jt <- as.vector(stats::aggregate(. ~ V2, output_jt, sum)[,2])
        # output error is weighted according to input-output ratio, in order not to give higher weight to beer just because of the big amount of water in it
        ((sum(((output$Production-output_jt)/output_weights[2])^2)) + (sum((input$Processing-input_it)^2)))
      }

      # run optimization with initial values = 0
      result <- stats::optim(par = rep(0,nrow(TCF_ij)), fn = fun, method = "L-BFGS-B", lower = 0)
      # run optimization with initial values from file Param.csv
      # result <- optim(par = read.csv(file="Param.csv", header=TRUE, sep=";")[,5], fn = fun, method = "L-BFGS-B", lower = 0)

      # prepare optimization results to be incorporated into the Processing matrix (Proc)
      res <- cbind(TCF_ij, result$par)
      names(res) <- c(names(res)[1:5],"resultIN")
      res$resultOUT <- res$resultIN * res$TCF
      results[[region]] <- res
    }
  }
  print("End optim")

  save(results, file = paste0("/mnt/nfs_fineprint/tmp/fabio/data/yearly/",year,"_Optim.RData"))
  # load(file = paste0("/mnt/nfs_fineprint/tmp/fabio/data/yearly/",year,"_Optim.RData"))
  
  # region=231
  for(region in regions$Country.Code){
    iso <- as.character(regions$ISO[regions$Country.Code==region])
    if(region <= length(results)){
      res <- results[[region]]
      if(length(res)>0){
        # Downscale in-flows to the available processing quantities, where these are exceeded by the optimization algorithm
        resIn <- stats::aggregate(resultIN ~ Input.Code + Input, res, sum)
        input <- merge(IN, CBS[CBS$Country.Code==region & CBS$Item.Code %in% IN$Item.Code & CBS$Year==year,c(3,13)], by="Item.Code", all.x=TRUE)
        input$Processing[! is.finite(input$Processing)] <- 0
        # input <- input[input$Processing > 0,]
        resIn <- merge(resIn, input[,c(1,3)], by.x="Input.Code", by.y="Item.Code")
        resIn$error <- resIn$resultIN / resIn$Processing
        resIn$error[!is.finite(resIn$error)] <- 0
        res <- merge(res, resIn[,c(1,5)], by="Input.Code")
        for(flow in 1:nrow(res)){
          if(res$error[flow] > 1){
            res$resultIN[flow] <- res$resultIN[flow] / res$error[flow]
            res$resultOUT[flow] <- res$resultIN[flow] * res$TCF[flow]
          }
        }
        res$error <- NULL
        # Downscale out-flows to the reported production quantities, where these are exceeded by the optimization algorithm
        resOut <- stats::aggregate(resultOUT ~ Output.Code + Output, res, sum)
        output <- merge(OUT, CBS[CBS$Country.Code==region & CBS$Item.Code %in% OUT$Item.Code & CBS$Year==year,c(3,6)], by="Item.Code", all.x=TRUE)
        output$Production[! is.finite(output$Production)] <- 0
        # output <- output[output$Production > 0,]
        resOut <- merge(resOut, output[,c(1,3)], by.x="Output.Code", by.y="Item.Code")
        resOut$error <- resOut$resultOUT / resOut$Production
        res <- merge(res, resOut[,c(1,5)], by="Output.Code")
        for(flow in 1:nrow(res)){
          if(res$error[flow] > 1){
            res$resultOUT[flow] <- res$resultOUT[flow] / res$error[flow]
            res$resultIN[flow] <- res$resultOUT[flow] / res$TCF[flow]
          }
        }
        res$error <- NULL
        res$Proc.code <- "p066"
        res$Proc.code[res$Output.Code==2656] <- "p081"
        res$Proc.code[res$Output.Code==2657] <- "p082"
        res$Proc.code[res$Output.Code==2658] <- "p083"
        
        results[[region]] <- res
        
      }
    }
  }
  
  # write optim results into use table
  # region=231
  for(region in regions$Country.Code){
    iso <- as.character(regions$ISO[regions$Country.Code==region])
    if(region <= length(results)){
      res <- results[[region]]
      if(length(res)>0){
        for(flow in 1:nrow(res)){
          use[use$Proc.Code==res$Proc.code[flow] & use$Item.Code==res$Input.Code[flow] & use$Type=="optim",iso] <- res$resultIN[flow]
          CBS$Processing[CBS$Country.Code==region & CBS$Item.Code==res$Input.Code[flow]] <- 
            CBS$Processing[CBS$Country.Code==region & CBS$Item.Code==res$Input.Code[flow]] - res$resultIN[flow]
        }
      }
    }
  }
  
  #-------------------------------------------------------------------------
  # Allocation of seed and waste
  #-------------------------------------------------------------------------
  seedwaste <- CBS[(CBS$Seed)>0,c(1:4,14,15)]
  # waste is now excluded, i.e. only seed is considered an own use
  seedwaste$seedwaste <- seedwaste$Seed # + seedwaste$Waste
  supply <- reshape2::melt(sup, id=c("Proc.Code","Process","Com.Code","Item.Code","Item"), variable.name = "ISO", value.name = "Value")
  # supply$share <- 0
  sums <- stats::aggregate(Value ~ ISO + Item.Code, supply, sum)
  names(sums)[3] <- "total"
  supply <- merge(supply, sums, all.x = T)
  supply$share <- supply$Value / supply$total
  supply$share[!is.finite(supply$share)] <- 0
  supply <- merge(supply, regions[,c(1,3)], all.x = T)
  supply <- merge(supply, seedwaste[,c(1,3,7)], all.x = T)
  supply$seedwaste[!is.finite(supply$seedwaste)] <- 0
  supply$share <- supply$share * supply$seedwaste
  supply <- stats::aggregate(share ~ Proc.Code + Item.Code + ISO, supply, sum)
  supply <- supply[supply$share>0,]
  
  # Allocation of seed and waste to use table
  # iso="USA"
  userows <- use[,1:6]
  userows$ID <- paste(userows$Proc.Code,userows$Item.Code, sep = ".")
  supply$ID <- paste(supply$Proc.Code,supply$Item.Code, sep = ".")
  for(iso in unique(supply$ISO)){
    regsupply <- supply[supply$ISO==iso,]
    use[use$Type=="seedwaste", iso] <- regsupply$share[match(userows$ID[userows$Type=="seedwaste"],regsupply$ID)]
  }
  use[is.na(use)] <- 0
  
  
  # Allocate rest of processing use to food use
  CBS$Food[CBS$Processing>0] <- CBS$Food[CBS$Processing>0] + CBS$Processing[CBS$Processing>0]
  CBS$Processing[CBS$Processing>0] <- 0
  
  
  
  #-------------------------------------------------------------------------
  # allocation of final demand from CBS to use_fd
  #-------------------------------------------------------------------------
  # define use_fd structure
  use_FD <- data.table::data.table(Com.Code = items$Com.Code,
                       Item.Code = items$Item.Code,
                       Item = items$Item)
  fdcolumns <- apply(cbind(rep(as.character(regions$ISO), each = 4), rep(c("Food", "OtherUses", "StockVariation", "Balancing"), nrow(regions))), 1, paste, collapse="_")
  use_FD[,fdcolumns] <- 0
  
  # iso="USA"
  for(iso in regions$ISO){
    data <- CBS[CBS$Country.Code==regions$Country.Code[regions$ISO==iso],]
    use_FD[,(paste(iso,"Food",sep = "_")) := data$Food[match(use_FD$Item.Code,data$Item.Code)]]
    use_FD[,(paste(iso,"OtherUses",sep = "_")) := data$OtherUses[match(use_FD$Item.Code,data$Item.Code)]]
    use_FD[,(paste(iso,"StockVariation",sep = "_")) := data$StockVariation[match(use_FD$Item.Code,data$Item.Code)]]
    use_FD[,(paste(iso,"Balancing",sep = "_")) := data$Balancing[match(use_FD$Item.Code,data$Item.Code)]]
  }
  
  
  # save results
  save(CBS, file=paste0("/mnt/nfs_fineprint/tmp/fabio/data/yearly/",year,"_CBS_balanced_postuseallocation.RData"))
  save(sup, file=paste0("/mnt/nfs_fineprint/tmp/fabio/data/yearly/",year,"_sup.RData"))
  save(sup_usd, file=paste0("/mnt/nfs_fineprint/tmp/fabio/data/yearly/",year,"_sup_usd.RData"))
  save(use, use_FD, file=paste0("/mnt/nfs_fineprint/tmp/fabio/data/yearly/",year,"_use.RData"))
  
  return(year)
}


library(parallel)
# Calculate the number of cores
no_cores <- detectCores() - 1
# Initiate cluster
cl <- makeCluster(no_cores)
# Years to run
years <- 1986:2013
# start parallel
parLapply(cl, years, fabio_use, Prod_lvst_all=Prod_lvst_all, Lvst=Lvst, regions=regions, items=items, TCF=TCF)
# stop cluster
stopCluster(cl)
