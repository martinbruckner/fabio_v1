#################################################################
#
# WATER EXTENSION for FABIO
#
#################################################################

library(tidyverse)
rm(list=ls())

# read data
water <- read.csv("./inst/fabio_input/water_data.csv", stringsAsFactors = FALSE)
water_lvst <- read.csv2("./inst/fabio_input/water_lvst.csv", stringsAsFactors = FALSE)
water_grass <- read.csv("./inst/fabio_input/water_pasture.csv", stringsAsFactors = FALSE)
countries <- read.csv("./inst/fabio_input/regions_concordance_water.csv", stringsAsFactors = FALSE)
items <- read.csv("./inst/fabio_input/items_concordance_water.csv", stringsAsFactors = FALSE)
load("/mnt/nfs_fineprint/tmp/fabio/data/Prod.RData")

years <- 1986:2013

for(year in years){
  print(year)
  E <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_E.rds"))
  blue <- water[water$Water=="blue",]
  green <- water[water$Water=="green",]
  
  prod <- Prod[Prod$Year==year & Prod$Unit=="tonnes",]
  prod$Country_code_water <- countries$Code_water[match(prod$Country.Code,countries$Area.Code)]
  prod$Country_code_fabio <- countries$Code_fabio[match(prod$Country.Code,countries$Area.Code)]
  prod$Item_water <- items$Prod_water[match(prod$Item.Code,items$Item_Code)]
  
  prod$blue <- blue$Value[match(paste(prod$Country_code_water,prod$Item_water), paste(blue$FAO.code,blue$Product))]
  prod$green <- green$Value[match(paste(prod$Country_code_water,prod$Item_water), paste(green$FAO.code,green$Product))]
  prod$blue[is.na(prod$blue)] <- 0
  prod$green[is.na(prod$green)] <- 0
  prod$Value[is.na(prod$Value)] <- 0
  
  prod$blue_water <- prod$blue * prod$Value
  prod$green_water <- prod$green * prod$Value
  
  blue_water <- prod %>% 
    group_by(Country.Code,Item) %>% 
    summarise(value = sum(blue_water))
  
  green_water <- prod %>% 
    group_by(Country.Code,Item) %>% 
    summarise(value = sum(green_water))
  
  E$ID <- NULL
  E$Blue_water <- blue_water$value[match(paste(E$Country.Code,E$Item), paste(blue_water$Country.Code,blue_water$Item))]
  E$Green_water <- green_water$value[match(paste(E$Country.Code,E$Item), paste(green_water$Country.Code,green_water$Item))]
  E$Blue_water[is.na(E$Blue_water)] <- 0
  E$Green_water[is.na(E$Green_water)] <- 0
  
  # water footprint of grazing
  E$Green_water[E$Item=="Grazing"] <- water_grass$Green_water[match(E$Country.Code,water_grass$Code)][E$Item=="Grazing"]
  E$Green_water[E$Item=="Grazing"] <- E$Green_water[E$Item=="Grazing"] * E$Biomass[E$Item=="Grazing"]
  
  
  saveRDS(E, paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_E.rds"))
}

