##############################################################################################
##  
##  FABIO: BUILD MRIO TABLE BASED ON FAOSTAT COMMODITY BALANCE SHEETS AND TRADE DATA
##    2 Build Supply and Use tables
##  
##############################################################################################

library(reshape2)

rm(list=ls()); gc()
##########################################################################
# Make intitial settings
##########################################################################
# read region classification
regions <- read.csv(file="Regions.csv", header=TRUE, sep=";")
# read commodity classification
items <- read.csv(file="Items.csv", header=TRUE, sep=";")
# read supply share items
share_items <- read.csv(file="Items_supply-shares.csv", header=TRUE, sep=";")
# read primary livestock product data
load(file="data/Prod_lvst.RData")
# exclude unused items
Prod_lvst <- Prod_lvst[Prod_lvst$Item.Code %in% share_items$Basis.Code,]
Prod_lvst_all <- Prod_lvst


##########################################################################
# Start loop for a series of years
##########################################################################
# year=1986
year=2013
for(year in 1986:2013){
  print(year)
  ##########################################################################
  # Read data
  #-------------------------------------------------------------------------
  load(file=paste0("data/yearly/",year,"_CBS_balanced.RData"))
  load(file=paste0("data/yearly/",year,"_BTD.RData"))
  Prod_lvst <- Prod_lvst_all[Prod_lvst_all$Year==year,]
  # read supply structure
  sup <- read.csv(file="Items_supply.csv", header=TRUE, sep=";")
  sup[,as.character(regions$ISO)] <- 0
  ##########################################################################
  
  # allocation of total supply quantities of all items to supplying processes (incl. double counting, e.g. for milk)
  # region="USA"
  for(region in regions$ISO){
    region.code <- regions$Country.Code[regions$ISO==region]
    # item="c002"
    for(item in items$Com.Code){
      item.code <- unique(items$Item.Code[items$Com.Code==item])
      if(item.code %in% CBS$Item.Code[CBS$Country.Code==region.code] & item.code %in% sup$Item.Code){
        sup[sup$Com.Code == item, region] <- CBS$Production[CBS$Country.Code==region.code & CBS$Item.Code==item.code]
      }
    }
  }
  
  # calculate supply shares for meat, hides&skins and milk
  shares <- merge(share_items[share_items$Source=="Prod_lvst",], Prod_lvst[Prod_lvst$Element=="Production",c(1:3,9)], 
                  by.x = "Basis.Code", by.y = "Item.Code", all.x = TRUE)
  shares <- aggregate(Value ~ Country.Code + Country + Proc.Code + Process + Com.Code + Item.Code + Item, shares, sum)
  basis <- aggregate(Value ~ Country.Code + Country + Com.Code + Item.Code + Item, shares, sum)
  names(basis)[6] <- "Total"
  shares <- merge(shares, basis, by=c("Country.Code", "Country", "Com.Code", "Item.Code", "Item"), all.x = TRUE)
  shares$shares <- shares$Value / shares$Total
  
  # region="USA"
  for(region in regions$ISO){
    region.code <- regions$Country.Code[regions$ISO==region]
    # process="Cattle slaughtering"
    for(process in unique(sup$Process)){
      # item="c115"
      for(item in sup$Com.Code[sup$Process==process]){
        if(paste(process, item) %in% paste(shares$Process[shares$Country.Code == region.code], shares$Com.Code[shares$Country.Code == region.code])){
          sup[sup$Process == process & sup$Com.Code == item, region] <- sup[sup$Process == process & sup$Com.Code == item, region] * 
            shares$shares[shares$Process == process & shares$Com.Code == item & shares$Country.Code == region.code]
        } else if(item %in% shares$Com.Code){
          sup[sup$Process == process & sup$Com.Code == item, region] <- 0
        }
      }
    }
  }
  
  # calculate supply shares for other animal products based on meat supply shares
  # region=174
  for(region in 1:nrow(regions)){
    region.code <- regions$Country.Code[region]
    meat <- sup[sup$Com.Code %in% c("c115", "c116", "c117", "c118", "c119"),c(1:5,region+5)]
    colnames(meat)[6] <- "Value"
    meat$shares <- meat$Value / sum(meat$Value)
    # process="Cattle slaughtering"
    for(process in meat$Process){
      sup[sup$Process==process & sup$Com.Code %in% c("c120", "c121", "c123", "c124"),region+5] <- 
        sup[sup$Process==process & sup$Com.Code %in% c("c120", "c121", "c123", "c124"),region+5] * meat$shares[meat$Process==process]
    }
  }
  sup[is.na(sup)] <- 0
  
  # calculate supply shares for 'Oilseed Cakes, Other'
  # region=174
  for(region in 1:nrow(regions)){
    region.code <- regions$Country.Code[region]
    oil <- sup[sup$Item.Code %in% c(2581, 2582, 2586),c(1:5,region+5)]
    colnames(oil)[6] <- "Value"
    oil$shares <- oil$Value / sum(oil$Value)
    # process="Ricebran Oil extraction"
    for(process in oil$Process){
      sup[sup$Process==process & sup$Com.Code=="c090",region+5] <- 
        sup[sup$Process==process & sup$Com.Code=="c090",region+5] * oil$shares[oil$Process==process]
    }
  }
  sup[is.na(sup)] <- 0
  
  
  
  #------------------------------------------------------
  # Convert tonnes into thousand USD
  #------------------------------------------------------
  sup_usd <- sup
  sup_usd$ID <- paste(sup_usd$Proc.Code,sup_usd$Item.Code,sep = ".")
  
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
  
  # iso="USA"
  for(iso in colnames(sup)[-(1:5)]){
    region.code <- regions$Country.Code[regions$ISO==iso]
    # convert sup into thousand USD
    data <- data.frame(Proc.Code = sup$Proc.Code,
                       Item.Code = sup$Item.Code,
                       Item = sup$Item,
                       Value = sup[,iso])
    
    data <- merge(data, prices[prices$Country.Code==region.code, c(3,8)], all.x = T)
    data$worldprice <- worldprices$price[match(data$Item.Code, worldprices$Item.Code)]
    # use world average price, if no country specific value available
    # use 1 where no world average available
    data$price[!is.finite(data$price)] <- data$worldprice[!is.finite(data$price)]
    data$price[!is.finite(data$price)] <- 1
    data$price[data$price==0] <- data$worldprice[data$price==0]
    data$price[data$price==0] <- 1
    data$Value <- data$Value * data$price
    data$ID <- paste(data$Proc.Code,data$Item.Code,sep = ".")
    sup_usd[,iso] <- data$Value[match(sup_usd$ID, data$ID)]
    
  }
  
  sup_usd$ID <- NULL
  sup$ID <- NULL
  
  
  # #------------------------------------------------------
  # # separate imported and domestic supply
  # #------------------------------------------------------
  # sup_dom <- sup
  # sup_imp <- sup
  # sup_usd_dom <- sup_usd
  # sup_usd_imp <- sup_usd
  # # region=2
  # for(region in 1:nrow(regions)){
  #   region.code <- regions$Country.Code[regions$ISO==colnames(sup[region+5])]
  #   origin <- data.frame(Item.Code = sup$Item.Code, Item = sup$Item,
  #                        dom = CBS$Production[CBS$Country.Code==region.code][match(sup$Item.Code,CBS$Item.Code[CBS$Country.Code==region.code])],
  #                        imp = CBS$Imports[CBS$Country.Code==region.code][match(sup$Item.Code,CBS$Item.Code[CBS$Country.Code==region.code])])
  #   origin[is.na(origin)] <- 0
  #   origin$total <- origin$dom + origin$imp
  #   origin$dom <- origin$dom / origin$total
  #   origin$imp <- origin$imp / origin$total
  #   origin[is.na(origin)] <- 0
  #   sup_dom[,region+5] <- sup_dom[,region+5] * origin$dom
  #   sup_imp[,region+5] <- sup_imp[,region+5] * origin$imp
  #   sup_usd_dom[,region+5] <- sup_usd_dom[,region+5] * origin$dom
  #   sup_usd_imp[,region+5] <- sup_usd_imp[,region+5] * origin$imp
  # }
  
  
  # save results
  save(sup, file=paste0("data/yearly/",year,"_sup.RData"))
  save(sup_usd, file=paste0("data/yearly/",year,"_sup_usd.RData"))
  
}

