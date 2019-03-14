#' @title FABIO extensions
#' @author Martin Bruckner, \email{martin.bruckner@@wu.ac.at}
#' 
#' @description Prepare land use and biomass extensions for FABIO model. 
#' 
#' @param ... some argument 
#' 
#' @return returns some data
#' 
#' @export
fabio_extensions <- function(wd, years){
  
  
  library(dplyr)
  library(data.table)
  library(reshape2)
  year <- 2013
  
  # current_wd <- getwd()
  # setwd(wd)
  # setwd("W:/WU/Projekte/SRU-Projekte/01_Projekte/1305_Bonn BioMRIO/02_FABIO/01_code & data") #temp
  
  ##########################################################################
  # Make intitial settings
  ##########################################################################
  # read region classification
  regions <- utils::read.csv(file="./input/Regions.csv", header=TRUE, sep=";")
  # read commodity classification
  items <- utils::read.csv(file="./input/Items.csv", header=TRUE, sep=";")
  # load production data with yields
  load(file=paste0("./output/Prod.RData"))
  Prod <- Prod[Prod$Element %in% c("Area harvested","Production"),]
  # aggregate RoW
  Prod$Country[! Prod$Country.Code %in% regions$Country.Code] <- "RoW"
  Prod$Country.Code[! Prod$Country.Code %in% regions$Country.Code] <- 999
  
  Prod <- as.data.frame(Prod %>% dplyr::group_by(Country.Code, Country, Item.Code, Item, Element, Year, Unit) %>% 
                          dplyr::summarise(Value = sum(Value)))
  Prod$ID <- paste(Prod$Country.Code,Prod$Item.Code,sep="_")
  
  ##########################################################################
  # Start loop for a series of years
  ##########################################################################
  for(year in years){
    print(year)
    
    #-------------------------------------------------------------------------
    # Read data
    #-------------------------------------------------------------------------
    load(file=paste0("./output/yearly/",year,"_X.RData"))
    Landuse <- Prod[Prod$Unit=="ha" & Prod$Year==year,]
    Biomass <- Prod[Prod$Unit=="tonnes" & Prod$Year==year,]
    nrreg <- nrow(regions)
    nrcom <- length(X) / nrreg
    
    
    #-------------------------------------------------------------------------
    # Prepare Extension
    #-------------------------------------------------------------------------
    E <- data.frame(Country.Code = rep(regions$Country.Code, each = nrcom),
                      Country = rep(regions$Country, each = nrcom),
                      Item.Code = rep(items$Item.Code, nrreg),
                      Item = rep(items$Item, nrreg),
                      Com.Code = rep(items$Com.Code, nrreg),
                      Group = rep(items$Group, nrreg),
                      Landuse = 0,
                      Biomass = 0)
    
    E$ID <- paste(E$Country.Code,E$Item.Code,sep="_")
    E$Landuse <- Landuse$Value[match(E$ID,Landuse$ID)]
    E$Landuse[!is.finite(E$Landuse)] <- 0
    E$Landuse[E$Group != "Primary crops"] <- 0
    E$Biomass <- Biomass$Value[match(E$ID,Biomass$ID)]
    E$Biomass[!is.finite(E$Biomass)] <- 0
    E$Biomass[E$Group != "Primary crops"] <- 0
    
    save(E, file=paste0("./output/yearly/",year,"_E.RData"))
  }
}