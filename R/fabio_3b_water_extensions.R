#################################################################
#
# WATER EXTENSION for FABIO
#
#################################################################

library(tidyr)

water_data <- read.csv2("water_footprint_average.csv")
# green - blue - grey - total
country_conc <- read.csv2("country_hoekstra_fabio.csv", stringsAsFactors = FALSE)
country_conc$Country_water[country_conc$Country_fabio=="RoW"] <- "Global average"
items_water <- read.csv2("Items_concordance.csv")
items_water <- items_water[,c(2,5,6,7)]
countries <- read.csv2("fabio_countries.csv")
countries$Country_water <- country_conc$Country_water[match(countries$Country,country_conc$Country_fabio)]
items <- read.csv2("items.csv")
# read production data
data <- as.data.frame(readRDS("./data/crop_prod.rds"))
data <- data[data$Year==2013 & data$Element=="Production",c(1,2,4,6,8,9,10)]
output <- readRDS("/mnt/nfs_fineprint/tmp/fabio/2013_X.rds")

water_fabio <- cbind(countries[,1:2], water_data[match(countries$Country_water,water_data$Country),-(1:2)])
colnames(water_fabio)[-(1:2)] <- rep(colnames(water_fabio)[-(1:2)][((0:146)*4+1)], each = 4)

# ---------------------------------------------------------------------------
water_extension <- function(water=""){
  if(water=="blue") water_selection <- water_fabio[,c(1,2,2+(1:(ncol(water_fabio)/4)-1)*4+2)] %>% 
      tidyr::gather(Product, Value, -FAO.Code, -Country)
  if(water=="green") water_selection <- water_fabio[,c(1,2,2+(1:(ncol(water_fabio)/4)-1)*4+1)] %>% 
      tidyr::gather(Product, Value, -FAO.Code, -Country)
  # ---------------------------------------------------------------------------
  # calculate total blue and green water requirements
  water <- items_water[items_water$Prod_water!="", 1:2]
  water$Prod_FAO <- as.character(water$Prod_FAO)
  water$Item_FABIO <- as.character(water$Item_FABIO)
  water$Prod_FAO[water$Prod_FAO==""] <- water$Item_FABIO[water$Prod_FAO==""]
  water$Prod_water <- as.character(items_water$Prod_water[match(water$Prod_FAO,items_water$Prod_FAO)])
  water$Prod_water[161:162] <- as.character(items_water$Prod_water[168:169])
  water$Prod_colnames <- as.character(items_water$Prod_colnames[match(water$Prod_FAO,items_water$Prod_FAO)])
  water$Prod_colnames[161:162] <- as.character(items_water$Prod_colnames[168:169])
  
  water <- do.call("rbind", replicate(216, water, simplify = FALSE))
  water$Country.code <- rep(unique(data$`Area Code`)[1:216], each=162)
  water$Country_FAO <- rep(unique(data$Area)[1:216], each=162)
  water <- water[water$Country.code!=351,]
  water$coeff <- water_selection$Value[match(paste(water$Country.code,water$Prod_colnames),
                                             paste(water_selection$FAO.Code,water_selection$Product))]
  # add ROW water coefficients for countries not included in water data set
  water$coeff[is.na(water$coeff)] <- water_selection$Value[match(water$Prod_colnames[is.na(water$coeff)],
                                                                 water_selection$Product)]
  water$production <- data$Value[match(paste(water$Country_FAO,water$Prod_FAO),
                                       paste(data$Area,data$Item))]
  water$production[is.na(water$production)] <- 0
  # add fodder crop and grazing
  output <- data.frame(output = output,
                       item = rep(items$FAO.Name, 192),
                       country = rep(countries$Country, each=130))
  water$Country_FAO[water$Country_FAO=="Czechia"] <- "Czech Republic"
  water$output <- output$output[match(paste(water$Item_FABIO,water$Country_FAO),
                                      paste(output$item,output$country))]
  water$production[water$Item_FABIO %in% c("Fodder crops", "Grazing")] <-
    water$output[water$Item_FABIO %in% c("Fodder crops", "Grazing")]
  water$output <- NULL
  # total water requirements
  water$water <- water$coeff * water$production
  # aggregate to fabio classification
  water$Country <- countries$Country[match(water$Country.code,countries$FAO.Code)]
  water$Country.code[is.na(water$Country)] <- 999
  water$Country[water$Country.code==999] <- "RoW"
  water <- aggregate(cbind(water, production) ~ Country + Item_FABIO, data = water, sum)
  
  water$Com.Code <- items$Com.Code[match(water$Item_FABIO,items$FAO.Name)]
  water$Country.Code <- countries$FAO.Code[match(water$Country,countries$Country)]
  water <- water[order(water$Country.Code,water$Com.Code),]
  water$water[water$water<0] <- 0
  return(water)
}

# -------------------------------------------------------------------------
# integrate water footprint into Extension
E <- readRDS("/mnt/nfs_fineprint/tmp/fabio/2013_E.rds")
blue <- water_extension("blue")
green <- water_extension("green")
E$Blue_water <- blue$water[match(paste(E$Country,E$Item),paste(blue$Country,blue$Item_FABIO))]
E$Green_water <- green$water[match(paste(E$Country,E$Item),paste(green$Country,green$Item_FABIO))]
E$Blue_water[is.na(E$Blue_water)] <- 0
E$Green_water[is.na(E$Green_water)] <- 0

saveRDS(E, "/mnt/nfs_fineprint/tmp/fabio/2013_E.rds")