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
  load(file=paste0("./data/yearly/",year,"_Y.RData"))
  load(file=paste0("./data/yearly/",year,"_MP.RData"))
  # aggregate multipliers A) by country of origin and B) by input product
  # MP <- colSums(MP)
  # gc()
  # max(MP)
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
  FP_region <- list()
  regions_selected <- regions[regions$ISO %in% c("CHN"),]
  regions_selected$Nr <- (1:nrow(regions))[regions$ISO %in% c("CHN")]
  # region=31
  for(region in regions_selected$Nr){
    print(paste("region",region))
    FP_fd <- list()
    # fd=1
    for(fd in 1:4){
      # define final demand
      Yreg <- Y[,(4*region-4+fd)]
      
      # Calculate Footprint (MP * FD)
      FP <- as.data.table((t(MP) * Yreg))
      FP$ID <- items$Com.Code
      FP <- as.data.frame(FP[, lapply(.SD, sum), by = .(ID)])
      FP$ID <- NULL
      FP <- t(FP)
      FP_fd[[fd]] <- FP
    }
    FP_region[[region]] <- FP_fd
    rm(FP_fd); gc()
  }
  
  save(FP_region, file=paste0("results/",year,"_FP_results_selected.RData"))
  
  
  # Rearrange results as list
  fd_categories <- c("1_Food","2_OtherUses","3_StockVariation","4_Balancing")
  FP <- data.table()
  # region=9
  for(region in regions_selected$Nr){
    print(paste("region",region))
    # fd = 1
    for(fd in 1:4){
      f <- as.data.table(FP_region[[region]][[fd]])
      colnames(f) <- as.character(items$Com.Code)
      f$From.Country.Code <- regions$Country.Code
      f$From.Country <- regions$Country
      f$From.ISO <- regions$ISO
      f <- melt(f, id.vars = c("From.Country.Code","From.Country","From.ISO"), variable.name = "Com.Code")
      # f <- melt(f, id.vars = "From.Country.Code")
      f$fd <- fd_categories[fd]
      f$Country.Code <- region
      
      FP <- rbind(FP,f)
    }
  }
  
  FP <- dcast(FP, ... ~ fd, value.var = "value", fun.aggregate = sum)
  FP$Country <- regions$Country[match(FP$Country.Code,regions$Country.Code)]
  FP$ISO <- regions$ISO[match(FP$Country.Code,regions$Country.Code)]
  
  write.xlsx(FP, file=paste0("results/",year,"_FP_selected_A.xlsx"), sheetName = "Biomass (tonnes)", row.names = F)
  
  # Rearrange results as list
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
  FP <- FP[rowSums(FP[,6:9])>0]
  FP$Country <- regions_selected$Country[match(FP$Country.Nr,regions_selected$Nr)]
  FP$ISO <- regions_selected$ISO[match(FP$Country.Nr,regions_selected$Nr)]
  FP$Continent <- regions_selected$Continent[match(FP$Country.Nr,regions_selected$Nr)]
  FP$Country.Nr <- NULL
  fwrite(FP, file=paste0("results/",year,"_FP_selected_A.csv"), sep=";")
  # write.xlsx(FP, file=paste0("results/",year,"_FP_selected_A.xlsx"), sheetName = "Biomass (tonnes)", row.names = F)
  
}

