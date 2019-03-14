##############################################################################################
##  
##  FABIO: BUILD MRIO TABLE BASED ON FAOSTAT COMMODITY BALANCE SHEETS AND TRADE DATA
##    4b Calculate Footprints for Vegetable Oils and Ethanol
##  
##############################################################################################

library(reshape2)
library(data.table)
library(Matrix)
library(MASS)
# source("http://bioconductor.org/biocLite.R")
# biocLite("rhdf5")
# library(rhdf5)
library(magrittr)
library(dplyr)

rm(list=ls()); gc()

is.finite.data.frame <- function(x) do.call(cbind, lapply(x, is.finite))
##########################################################################
# Make intitial settings
##########################################################################
# read region classification
regions <- read.csv(file="Regions.csv", header=TRUE, sep=";")
# read commodity classification
items <- read.csv(file="Items.csv", header=TRUE, sep=";")
# set year
year=2011
# set nrreg and nrcom
nrreg <- nrow(regions)
nrcom <- nrow(items)
# select Oils or Ethanol
a <- "Oils"
b <- "Ethanol"
biofuel <- a


#-------------------------------------------------------------------------
# Load data
#-------------------------------------------------------------------------
# load production data with yields
load(file=paste0("./data/Prod.RData"))
Prod <- Prod[Prod$Element %in% c("Area harvested","Production"),]
# aggregate RoW
Prod$Country[! Prod$Country.Code %in% regions$Country.Code] <- "RoW"
Prod$Country.Code[! Prod$Country.Code %in% regions$Country.Code] <- 999
Prod <- as.data.frame(Prod %>% group_by(Country.Code, Country, Item.Code, Item, Element, Year, Unit) %>% 
                        summarise(Value = sum(Value)))
Prod$ID <- paste(Prod$Country.Code,Prod$Item.Code,sep="_")
Landuse <- Prod[Prod$Unit=="ha" & Prod$Year==year,]

# load Z and Y
load(file=paste0("./data/yearly/",year,"_Z.RData"))
load(file=paste0("./data/yearly/",year,"_Y.RData"))
load(file=paste0("./data/yearly/",year,"_X.RData"))


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
ext$Value <- Landuse$Value[match(ext$ID,Landuse$ID)]
ext$Value <- ext$Value / x
ext$Value[!is.finite(ext$Value)] <- 0


#-------------------------------------------------------------------------
# calculate A-matrix
#-------------------------------------------------------------------------
load(file=paste0("./data/yearly/",year,"_A.RData"))

A <- t(t(Z)/x)
rm(Z); gc()
A[!is.finite(A)] <- 0
gc()


#-------------------------------------------------------------------------
# aggregate continents in Y
#-------------------------------------------------------------------------
Y <- as.data.frame(t(Y))
Y$ISO <- substr(rownames(Y),1,3)
Y$Continent <- regions$Continent[match(Y$ISO,regions$ISO)]
Y$FD <- substr(rownames(Y),5,100)
Y$ISO <- NULL
Y <- Y[Y$Continent=="EU",]
Y <- aggregate(. ~ Continent + FD, Y, sum)
Y <- as.data.frame(t(Y[, -(1:2)]))
colnames(Y) <- c("Balancing","Food","OtherUses","StockVariation")


#-------------------------------------------------------------------------
# cut specific commodities from A, Y and ext
#-------------------------------------------------------------------------
if(biofuel=="Oils"){
  # all oilseeds and oils (25 commodities)
  commodities <- c(21:30,63:64,69:81)
}
if(biofuel=="Ethanol"){
  # all cereals, sugar crops, fruits and ethanol (17 commodities)
  commodities <- c(1:5,8:11,15:16,40,43:44,66,91,95)
}
select <- rep(commodities,nrreg)+rep((0:(nrreg-1))*nrcom, each = length(commodities))
A_select <- A[select,select]
Y_select <- Y[select,]
ext_select <- ext[select,]


#-------------------------------------------------------------------------
# calculate L-Inverse
#-------------------------------------------------------------------------
L <- solve(diag(nrow(A_select)) - A_select)
# L <- ginv(diag(nrow(A_select)) - A_select)


#-------------------------------------------------------------------------
# Prepare Multipliers
#-------------------------------------------------------------------------
MP <- as.vector(ext_select$Value) * L
# rm(L); gc()


#-------------------------------------------------------------------------
# Calculate Footprints
#-------------------------------------------------------------------------
# calculate footprint for a specific final demand class ("Balancing","Food","OtherUses","StockVariation")
FP <- as.data.table(t(t(MP) * as.vector(Y_select$OtherUses)))

# Rearrange results as list
colnames(FP) <- rep(as.character(items$Com.Code[commodities]), nrreg)
FP$From.Country.Code <- rep(regions$Country.Code, each=length(commodities))
FP$From.Country <- rep(regions$Country, each=length(commodities))
FP$From.ISO <- rep(regions$ISO, each=length(commodities))
FP$From.Com <- rep(as.character(items$Com.Code[commodities]), nrreg)
FP <- melt(FP, id.vars = c("From.Country.Code","From.Country","From.ISO","From.Com"), variable.name = "Com.Code")
FP <- as.data.frame(FP %>% group_by(From.Country.Code, From.Country, From.ISO, From.Com, Com.Code) %>% 
                        summarise(Value = sum(value)))

fwrite(FP, file=paste0("results/Biofuels/",year,"_FP_EU_",biofuel,"_OtherUses.csv"), sep=";")


