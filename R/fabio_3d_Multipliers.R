##############################################################################################
##  
##  FABIO: BUILD MRIO TABLE BASED ON FAOSTAT COMMODITY BALANCE SHEETS AND TRADE DATA
##    4b Calculate Footprints
##  
##############################################################################################

library(reshape2)
library(data.table)

rm(list=ls()); gc()

is.finite.data.frame <- function(x) do.call(cbind, lapply(x, is.finite))
# read region classification
regions <- read.csv(file="Regions.csv", header=TRUE, sep=";")
# read commodity classification
items <- read.csv(file="Items.csv", header=TRUE, sep=";")

nrcom <- nrow(items)
nrreg <- nrow(regions)

##########################################################################
# Start loop for a series of years
##########################################################################
# year=1986
year=2004
for(year in 1986:2013){
  print(year)
  #-------------------------------------------------------------------------
  # Read data
  #-------------------------------------------------------------------------
  load(file=paste0("./data/yearly/",year,"_L.RData"))
  load(file=paste0("./data/yearly/",year,"_X.RData"))
  load(file=paste0("./data/yearly/",year,"_Y.RData"))
  load(file=paste0("./data/yearly/",year,"_E.RData"))
  
  #-------------------------------------------------------------------------
  # Prepare Extension
  #-------------------------------------------------------------------------
  ext <- data.frame(Country.Code = rep(regions$Country.Code, each = nrcom),
                    Country = rep(regions$Country, each = nrcom),
                    Item.Code = rep(items$Item.Code, nrreg),
                    Item = rep(items$Item, nrreg),
                    Com.Code = rep(items$Com.Code, nrreg),
                    Group = rep(items$Group, nrreg),
                    Value = 0)
  # ext$Value[!items$Group=="Primary crops"] <- 0
  ext$ID <- paste(ext$Country.Code,ext$Item.Code,sep="_")
  ext$Value <- E$Landuse[match(ext$ID,E$ID)]
  ext$Value[!is.finite(ext$Value)] <- 0
  
  #-------------------------------------------------------------------------
  # Prepare Multipliers
  #-------------------------------------------------------------------------
  MP <- as.vector(ext$Value) / X
  MP[!is.finite(MP)] <- 0
  MP <- MP * L   # is identical with L * MP
  rm(L); gc()
  save(MP, file=paste0("./data/yearly/",year,"_MP_landuse.RData"))
  
}

