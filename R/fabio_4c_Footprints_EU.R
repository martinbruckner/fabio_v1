##############################################################################################
##  
##  FABIO: BUILD MRIO TABLE BASED ON FAOSTAT COMMODITY BALANCE SHEETS AND TRADE DATA
##    4b Calculate Footprints
##  
##############################################################################################

library(reshape2)
library(data.table)
library(Matrix)
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
regions <- read.csv(file="./inst/fabio_input/Regions.csv", header=TRUE, sep=";")
# read commodity classification
items <- read.csv(file="./inst/fabio_input/Items.csv", header=TRUE, sep=";")
# load production data with yields
load(file=paste0("/mnt/nfs_fineprint/tmp/fabio/data/Prod.RData"))
Prod <- Prod[Prod$Element %in% c("Area harvested","Production"),]
# aggregate RoW
Prod$Country[! Prod$Country.Code %in% regions$Country.Code] <- "RoW"
Prod$Country.Code[! Prod$Country.Code %in% regions$Country.Code] <- 999
# # test speed (it's faster to use pipes)
# system.time(a <- Prod %>% group_by(Country, Item, Year, Unit) %>% summarise(Value = sum(Value)))
# system.time(b <- aggregate(Value ~ Country + Item + Year + Unit, Prod, sum))
# a %>% filter(Country == "RoW", Item == "Abaca", Year == 1961)
# b %>% filter(Country == "RoW", Item == "Abaca", Year == 1961)
Prod <- as.data.frame(Prod %>% group_by(Country.Code, Country, Item.Code, Item, Element, Year, Unit) %>% 
                        summarise(Value = sum(Value)))
Prod$ID <- paste(Prod$Country.Code,Prod$Item.Code,sep="_")


##########################################################################
# Start loop for a series of years
##########################################################################
# year=1986
year=2013
#for(year in 1986:2013){
print(year)
#-------------------------------------------------------------------------
# Read data
#-------------------------------------------------------------------------
# h5ls(paste0("FABIO matlab/",year,"_L.mat"))
# L <- h5read(paste0("./FABIO matlab/",year,"_L.mat"), "L")
L <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/120/",year,"_L_price.rds"))
X <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/120/",year,"_X.rds"))
Y <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/120/",year,"_Y.rds"))
E <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/120/",year,"_E.rds"))
Landuse <- Prod[Prod$Unit=="ha" & Prod$Year==year,]
nrreg <- nrow(regions)
nrcom <- nrow(Y) / nrreg


#-------------------------------------------------------------------------
# Prepare Multipliers
#-------------------------------------------------------------------------
ext <- as.vector(E$Landuse) / X
ext[!is.finite(ext)] <- 0
MP <- ext * L   # is identical with L * MP

# aggregate multipliers A) by country of origin
MP <- as.data.frame(MP)
MP$ID <- rep(regions$ISO, each = nrcom)
MP_A <- setDT(MP)[, lapply(.SD, sum), by = .(ID)]
MP_A$ID <- NULL
MP_A <- as.matrix(MP_A)
# rownames(MP_A) <- paste(1:192,regions$ISO, sep = "_")
gc()


#-------------------------------------------------------------------------
# Calculate Footprints
#-------------------------------------------------------------------------
# aggregate continents in Y
Y <- as.data.frame(t(Y))
Y$ISO <- substr(rownames(Y),1,3)
Y$Continent <- regions$Continent[match(Y$ISO,regions$ISO)]
Y$FD <- substr(rownames(Y),5,100)
Y$ISO <- NULL
Y <- aggregate(. ~ Continent + FD, Y, sum)  # 164 sec
# Y <- as.data.frame(setDT(Y)[, lapply(.SD, sum), by = .(Continent,FD)])  # 300 sec
# Y <- as.data.frame(setDT(Y)[, lapply(.SD, sum), by = "Continent,FD"])  # 244 sec
# Y <- as.data.frame(Y %>% group_by(Continent, FD) %>% summarise_all(sum))  # 607 sec


# Total EU Footprint
Yreg <- rowSums(t(Y[Y$Continent=="EU", -(1:2)]))
FP <- as.data.table(t((t(MP_A) * as.vector(Yreg))))
# Rearrange results as list
f <- FP
colnames(f) <- rep(as.character(items$Item[items$X120]), nrreg)
f$From.Country.Code <- regions$Country.Code
f$From.Country <- regions$Country
f$From.ISO <- regions$ISO
f <- melt(f, id.vars = c("From.Country.Code","From.Country","From.ISO"), variable.name = "Com.Code")
f <- aggregate(value ~ ., f, sum)
fwrite(f, file=paste0("./output/",year,"_FP_EU_Total_2019-04-05.csv"), sep=";")


for(fd in unique(Y$FD)){
  Yreg <- t(Y[Y$Continent=="EU" & Y$FD==fd, -(1:2)])
  FP <- as.data.table(t((t(MP_A) * as.vector(Yreg))))
  # Rearrange results as list
  f <- FP
  colnames(f) <- rep(as.character(items$Com.Code), nrreg)
  f$From.Country.Code <- regions$Country.Code
  f$From.Country <- regions$Country
  f$From.ISO <- regions$ISO
  f$From.Continent <- regions$Continent
  f <- melt(f, id.vars = c("From.Country.Code","From.Country","From.ISO","From.Continent"), variable.name = "Com.Code")
  f <- aggregate(value ~ ., f, sum)
  f$Com.Group <- items$Com.Group[match(f$Com.Code,items$Com.Code)]
  fwrite(f, file=paste0("results/",year,"_FP_EU_",fd,"_2018-04-24.csv"), sep=";")
}



#}







###################################################
# Check MP and Z for palm oil
###################################################
a <- t(MP_A)
colnames(a) <- regions$Country.Code
a <- cbind(ext,a)
fwrite(a, file=paste0("results/Biofuels/",year,"_MP.csv"), sep=";")

b <- MP
b$ID <- NULL
b <- cbind(Item = as.character(rep(items$Item, nrreg)), b)
b <- b[b$Item %in% c("Oil, palm fruit", "Palm kernels", "Palmkernel Oil", "Palm Oil", "Palmkernel Cake"),]
colnames(b) <- c("Item", as.character(rep(items$Com.Code, nrreg)))
a <- b$Item
b$Item <- NULL
b <- as.data.frame(t(b))
colnames(b) <- a

b <- cbind(Item = as.character(rep(items$Item, nrreg)), b)
b <- b[b$Item %in% c("Oil, palm fruit", "Palm kernels", "Palmkernel Oil", "Palm Oil", "Palmkernel Cake"),]

fwrite(as.data.frame(b), file=paste0("results/Biofuels/",year,"_MP.csv"), sep=";")



b <- h5read(paste0("./FABIO matlab/",year,"_L.mat"), "L")
b <- as.data.frame(b)
b <- cbind(Item = as.character(rep(items$Item, nrreg)), b)
b <- b[b$Item %in% c("Oil, palm fruit", "Palm kernels", "Palmkernel Oil", "Palm Oil", "Palmkernel Cake"),]
colnames(b) <- c("Item", as.character(rep(items$Com.Code, nrreg)))
a <- b$Item
b$Item <- NULL
b <- as.data.frame(t(b))
colnames(b) <- a

b <- cbind(Item = as.character(rep(items$Item, nrreg)), b)
b <- b[b$Item %in% c("Oil, palm fruit", "Palm kernels", "Palmkernel Oil", "Palm Oil", "Palmkernel Cake"),]

fwrite(as.data.frame(b), file=paste0("results/Biofuels/",year,"_Z.csv"), sep=";")


# check supply and use tables for Indonesia
load("C:/Users/mbruckne/Desktop/01_code & data/data/yearly/2011_use.RData")
a <- cbind(use[,1:6], use$IDN)
load("C:/Users/mbruckne/Desktop/01_code & data/data/yearly/2011_use.RData")
b <- cbind(sup[,1:5], sup$IDN, sup_usd$IDN)
b$price <- b$`sup_usd$IDN` / b$`sup$IDN`
fwrite(as.data.frame(a), file=paste0("results/Biofuels/",year,"_use_IDN.csv"), sep=";")
fwrite(as.data.frame(b), file=paste0("results/Biofuels/",year,"_sup_IDN.csv"), sep=";")

# check Z domestic for Indonesia
load("C:/Users/mbruckne/Desktop/01_code & data/data/yearly/2011_Z.RData")
a <- Z[(71*nrcom+1):(72*nrcom),(71*nrcom+1):(72*nrcom)]
rownames(a) <- colnames(a) <- items$Item
fwrite(as.data.frame(a), file=paste0("results/Biofuels/",year,"_Z_IDN.csv"), sep=";")

# check A domestic for Indonesia
load("C:/Users/mbruckne/Desktop/01_code & data/data/yearly/2011_A.RData")
a <- A[(71*nrcom+1):(72*nrcom),(71*nrcom+1):(72*nrcom)]
rownames(a) <- colnames(a) <- items$Item
fwrite(as.data.frame(a), file=paste0("results/Biofuels/",year,"_A_IDN.csv"), sep=";")

#b <- h5read(paste0("C:/Users/mbruckne/Desktop/01_code & data/data/yearly/",year,"_A.mat"), "A")


# check L domestic for Indonesia
L <- h5read(paste0("./FABIO matlab/",year,"_L.mat"), "L")
a <- L[(71*nrcom+1):(72*nrcom),(71*nrcom+1):(72*nrcom)]
rownames(a) <- colnames(a) <- items$Item
fwrite(as.data.frame(a), file=paste0("results/Biofuels/",year,"_L_IDN.csv"), sep=";")






