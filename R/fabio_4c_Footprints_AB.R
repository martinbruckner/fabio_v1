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
library(rhdf5)
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
# load production data with yields
load(file=paste0("./data/Prod.RData"))
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
year=2011
for(year in 1986:2013){
  print(year)
  #-------------------------------------------------------------------------
  # Read data
  #-------------------------------------------------------------------------
  # h5ls(paste0("FABIO matlab/",year,"_L.mat"))
  L <- h5read(paste0("./FABIO matlab/",year,"_L.mat"), "L")
  load(file=paste0("./data/yearly/",year,"_Z.RData"))
  load(file=paste0("./data/yearly/",year,"_Y.RData"))
  Landuse <- Prod[Prod$Unit=="ha" & Prod$Year==year,]
  nrreg <- nrow(regions)
  nrcom <- nrow(Y) / nrreg
  
  
  #-------------------------------------------------------------------------
  # Prepare Extension
  #-------------------------------------------------------------------------
  x <- rowSums(Z) + rowSums(Y)
  rm(Z); gc()
  
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
  ext$Value[!is.finite(ext$Value)] <- 0
  
  
  #-------------------------------------------------------------------------
  # Prepare Multipliers
  #-------------------------------------------------------------------------
  MP <- as.vector(ext$Value) / x
  MP[!is.finite(MP)] <- 0
  MP <- MP * L   # is identical with L * MP
  rm(L); gc()
  save(MP, file=paste0("./data/yearly/",year,"_MP.RData"))
  load(file=paste0("./data/yearly/",year,"_MP.RData"))
  
  # aggregate multipliers A) by country of origin
  MP <- as.data.frame(MP)
  MP$ID <- rep(regions$ISO, each = nrcom)
  MP_A <- setDT(MP)[, lapply(.SD, sum), by = .(ID)]
  MP_A$ID <- NULL
  MP_A <- as.matrix(MP_A)
  # rownames(MP_A) <- paste(1:192,regions$ISO, sep = "_")
  gc()
  save(MP_A, file=paste0("data/yearly/",year,"_MP_A.RData"))

  # aggregate multipliers B) by input product
  MP <- as.data.frame(MP)
  MP$ID <- rep(items$Com.Code, nrreg)
  MP_B <- setDT(MP)[, lapply(.SD, sum), by = .(ID)]
  MP_B$ID <- NULL
  MP_B <- as.matrix(MP_B)
  rownames(MP_B) <- items$Com.Code
  save(MP_B, file=paste0("data/yearly/",year,"_MP_B.RData"))

  rm(MP); gc()

  
  #-------------------------------------------------------------------------
  # Calculate Footprints
  #-------------------------------------------------------------------------
  load(file=paste0("data/yearly/",year,"_MP_A.RData"))
  load(file=paste0("data/yearly/",year,"_MP_B.RData"))
  load(file=paste0("data/yearly/",year,"_Y.RData"))
  
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
  
  FP_region <- list()
  
  # region=77
  for(region in regions_selected$Nr){
    print(paste0("region ",region))
    FP_fd <- list()
    # fd=1
    for(fd in 1:4){
      # define final demand
      Yreg <- Y[,(4*region-4+fd)]
      
      # Calculate Footprint (MP * FD)
      FP_AB <- list()
      
      FP <- as.data.table((t(MP_A) * Yreg))
      FP$ID <- items$Com.Code
      FP <- as.data.frame(FP[, lapply(.SD, sum), by = .(ID)])
      FP$ID <- NULL
      FP <- t(FP)
      FP_AB[["A"]] <- FP
      
      FP <- as.data.table((t(MP_B) * Yreg))
      FP$ID <- items$Com.Code
      FP <- as.data.frame(FP[, lapply(.SD, sum), by = .(ID)])
      FP$ID <- NULL
      FP <- t(FP)
      FP_AB[["B"]] <- FP
      
      FP_fd[[fd]] <- FP_AB
      
      rm(FP_AB); gc()
    }
    FP_region[[region]] <- FP_fd
    rm(FP_fd); gc()
  }
  
  save(FP_region, file=paste0("results/",year,"_FP_results_selected.RData"))
  
  # load(file=paste0("results/",year,"_FP_results_all.RData"))
  # load(file=paste0("results/",year,"_FP_results_selected.RData"))
  
  # Rearrange results A as list
  fd_categories <- c("1_Food","2_OtherUses","3_StockVariation","4_Balancing")
  FP <- data.table()
  # region=9
  for(region in regions_selected$Nr){
    print(paste("region",region))
    # fd = 1
    for(fd in 1:4){
      f <- as.data.table(FP_region[[region]][[fd]][["A"]])
      colnames(f) <- as.character(items$Com.Code)
      f$From.Country.Code <- regions$Country.Code
      f$From.Country <- regions$Country
      f$From.ISO <- regions$ISO
      f <- melt(f, id.vars = c("From.Country.Code","From.Country","From.ISO"), variable.name = "Com.Code")
      # f <- melt(f, id.vars = "From.Country.Code")
      f$fd <- fd_categories[fd]
      f$Country.Nr <- region
      
      FP <- rbind(FP,f)
    }
  }
  
  FP <- dcast(FP, ... ~ fd, value.var = "value", fun.aggregate = sum)
  FP <- FP[rowSums(FP[,6:9])>0]
  FP$Country <- regions_selected$Country[match(FP$Country.Nr,regions_selected$Nr)]
  FP$ISO <- regions_selected$ISO[match(FP$Country.Nr,regions_selected$Nr)]
  FP$Country.Nr <- NULL
  fwrite(FP, file=paste0("results/Biofuels/",year,"_FP_selected_A.csv"), sep=";")
  # write.xlsx(FP, file=paste0("results/Biofuels/",year,"_FP_selected_A.xlsx"), sheetName = "Biomass (tonnes)", row.names = F)
  
  # Rearrange results B as list
  fd_categories <- c("1_Food","2_OtherUses","3_StockVariation","4_Balancing")
  FP <- data.table()
  # region=9
  for(region in regions_selected$Nr){
    print(paste("region",region))
    # fd = 1
    for(fd in 1:4){
      f <- as.data.table(FP_region[[region]][[fd]][["B"]])
      colnames(f) <- as.character(items$Com.Code)
      f$From.Com.Code <- items$Com.Code
      f$From.Item <- items$Item
      f <- melt(f, id.vars = c("From.Com.Code","From.Item"), variable.name = "Com.Code")
      f$fd <- fd_categories[fd]
      f$Country.Nr <- region
      
      FP <- rbind(FP,f)
    }
  }
  
  FP <- dcast(FP, ... ~ fd, value.var = "value", fun.aggregate = sum)
  FP <- FP[rowSums(FP[,4:7])>0]
  FP$Country <- regions_selected$Country[match(FP$Country.Nr,regions_selected$Nr)]
  FP$ISO <- regions_selected$ISO[match(FP$Country.Nr,regions_selected$Nr)]
  FP$Country.Nr <- NULL
  fwrite(FP, file=paste0("results/Biofuels/",year,"_FP_selected_B.csv"), sep=";")
  # write.xlsx(FP, file=paste0("results/Biofuels/",year,"_FP_selected_B.xlsx"), sheetName = "Biomass (tonnes)", row.names = F)
  
}

