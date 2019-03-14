##############################################################################################
##  
##  FABIO: BUILD MRIO TABLE BASED ON FAOSTAT COMMODITY BALANCE SHEETS AND TRADE DATA
##    1e Define start values for BTD
##  
##############################################################################################

library(reshape2)
library(data.table)

rm(list=ls()); gc()
##########################################################################
# Start loop for a series of years
##########################################################################
# year=1988
year=2013
for(year in 1986:2013){
  print(year)
  ##########################################################################
  # Read data
  #-------------------------------------------------------------------------
  load(file=paste0("data/yearly/",year,"_CBS_cons.RData"))
  load(file=paste0("data/yearly/",year,"_BTD_cons.RData"))
  
  BTD$Value <- BTD$tonnes + BTD$tHead
  BTD <- BTD[,c(1:7,12)]
  CBS <- CBS[,c(1:5,7,10)]
  
  # do only once
  if(!exists("BTD_start")){
    reg <- unique(BTD[,1:2])
    names(reg) <- names(BTD)[3:4]
    reg <- rbind(reg, unique(BTD[,3:4]))
    names(reg) <- names(CBS)[1:2]
    reg <- rbind(reg, unique(CBS[,1:2]))
    reg <- unique(reg)
    
    nrreg <- nrow(reg)
    nritem <- length(unique(CBS$Item))
    
    BTD_start <- BTD[0,]
    BTD_start[1:(nrreg^2*nritem),] <- NA
    
    BTD_start$From.Country.Code <- rep(reg$Country.Code, nrreg*nritem)
    BTD_start$From.Country <- rep(reg$Country, nrreg*nritem)
    BTD_start$To.Country.Code <- rep(rep(reg$Country.Code, each = nrreg), nritem)
    BTD_start$To.Country <- rep(rep(reg$Country, each = nrreg), nritem)
    BTD_start$Item.Code <- rep(unique(CBS$Item.Code), each = nrreg*nrreg)
    BTD_start$Item <- rep(unique(CBS$Item), each = nrreg*nrreg)
    BTD_start$Year <- year
    BTD_start$Value <- 0
    BTD_start$ID <- paste(BTD_start$From.Country.Code,BTD_start$To.Country.Code,BTD_start$Item.Code, sep = ".")
  }
  BTD_start$Year <- year
  
  # fill BTD_original with values from BTD
  BTD_original <- BTD_start
  BTD$ID <- paste(BTD$From.Country.Code,BTD$To.Country.Code,BTD$Item.Code, sep = ".")
  BTD_original$Value <- BTD$Value[match(BTD_original$ID,BTD$ID)]
  BTD_original$Original <- 0
  BTD_original$Original[!is.na(BTD_original$Value)] <- 1
  # BTD_original$Value[is.na(BTD_original$Value)] <- 0
  
  BTD_est <- BTD_start
  BTD_est_ex <- BTD_start
  BTD_est_im <- BTD_start
  
  imp <- dcast(CBS[,-7], Item.Code + Item + Year ~ Country.Code, value.var = "Imports")
  exp <- dcast(CBS[,-6], Item.Code + Item + Year ~ Country.Code, value.var = "Exports")
  
  # estimate BTD
  region=231
  for(region in unique(CBS$Country.Code)){
    # estimate BTD by spreading exports according to import shares of all countries world wide
    temp <- imp
    temp[,as.character(region)] <- 0
    temp[,-(1:3)] <- temp[,-(1:3)] / rowSums(temp[,-(1:3)]) * exp[,as.character(region)]
    temp <- melt(as.data.table(temp), id=1:3, measure=4:ncol(temp))
    temp$ID <- paste(region,temp$variable,temp$Item.Code, sep = ".")
    BTD_est_ex$Value[BTD_est_im$From.Country.Code==region] <- 
      temp$value[match(BTD_est_ex$ID[BTD_est_im$From.Country.Code==region], temp$ID)]
    
    # estimate BTD by spreading imports according to export shares of all countries world wide
    temp <- exp
    temp[,as.character(region)] <- 0
    temp[,-(1:3)] <- temp[,-(1:3)] / rowSums(temp[,-(1:3)]) * imp[,as.character(region)]
    temp <- melt(as.data.table(temp), id=1:3, measure=4:ncol(temp))
    temp$ID <- paste(temp$variable,region,temp$Item.Code, sep = ".")
    BTD_est_im$Value[BTD_est_im$To.Country.Code==region] <- 
      temp$value[match(BTD_est_im$ID[BTD_est_im$To.Country.Code==region], temp$ID)]
    
  }
  
  # taking the average of import and export based estimates
  BTD_est$Est_ex <- BTD_est_ex$Value
  BTD_est$Est_im <- BTD_est_im$Value
  BTD_est$Value <- (BTD_est_ex$Value + BTD_est_im$Value) / 2
  
  # write files
  save(BTD_original, file = paste0("data/yearly/",year,"_BTD_original.RData"))
  save(BTD_est, file = paste0("data/yearly/",year,"_BTD_est.RData"))
  
  # # write csv files for balancing in GAMS
  # # fwrite is much faster than write.table
  # fwrite(BTD_original[,-9], file = paste0("C:/Users/mbruckne/Dropbox/FAOMRIO data/",year,"_BTD_original.csv"), sep = ";", row.names = FALSE)
  # fwrite(BTD_est[,1:8], file = paste0("C:/Users/mbruckne/Dropbox/FAOMRIO data/",year,"_BTD_est.csv"), sep = ";", row.names = FALSE)
  
  
  # write.table(BTD_original[,-9], file = paste0("C:/Users/mbruckne/Dropbox/FAOMRIO data/",year,"_BTD_original.csv"), sep = ";", row.names = FALSE)
  # write.table(BTD_est[,1:8], file = paste0("C:/Users/mbruckne/Dropbox/FAOMRIO data/",year,"_BTD_est.csv"), sep = ";", row.names = FALSE)
  # write.table(BTD_original[,c(1:7,10)], file = paste0("C:/Users/mbruckne/Dropbox/FAOMRIO data/",year,"_BTD_indicator.csv"), sep = ";", row.names = FALSE)
  
}
