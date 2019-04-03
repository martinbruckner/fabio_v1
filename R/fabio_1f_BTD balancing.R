##############################################################################################
##  
##  FABIO: BUILD MRIO TABLE BASED ON FAOSTAT COMMODITY BALANCE SHEETS AND TRADE DATA
##    1f Balance BTD_original + BTD_est using RAS
##  
##############################################################################################

library(reshape2)
library(data.table)

rm(list=ls()); gc()

##########################################################################
# Start loop for a series of years
##########################################################################
# year=1988
# year=2013
# for(year in 1986:2013){
fabio_BTD_balancing <- function(year){
  print(year)
  is.finite.data.frame <- function(x) do.call(cbind, lapply(x, is.finite))
  ##########################################################################
  # Read data
  #-------------------------------------------------------------------------
  load(file = paste0("/mnt/nfs_fineprint/tmp/fabio/data/yearly/",year,"_BTD_original.RData"))
  load(file = paste0("/mnt/nfs_fineprint/tmp/fabio/data/yearly/",year,"_BTD_est.RData"))
  load(file=paste0("/mnt/nfs_fineprint/tmp/fabio/data/yearly/",year,"_CBS_cons.RData"))
  
  BTD_est$Year <- year
  BTD_original$Year <- year
  
  BTD_original$Value[BTD_original$Original==0] <- 0
  
  
  #-------------------------------------------------------------------------------------------
  # 1. Integrate original and estimated BTD values
  #-------------------------------------------------------------------------------------------
  # compile data set with total imports and exports per region according to BTD (Actual) and CBS (Target)
  exp <- data.frame(data.table::as.data.table(BTD_original[,c(1,2,5,6,7,8)])[,list(val = sum(Value)), by = 'From.Country.Code,From.Country,Item.Code,Item,Year'])
  imp <- data.frame(data.table::as.data.table(BTD_original[,c(3,4,5,6,7,8)])[,list(val = sum(Value)), by = 'To.Country.Code,To.Country,Item.Code,Item,Year'])
  names(exp) <- c("Country.Code", "Country","Item.Code","Item","Year","Actual")
  names(imp) <- c("Country.Code", "Country","Item.Code","Item","Year","Actual")
  exp$ID <- paste(exp$Country.Code,exp$Item.Code,exp$Year, sep = ".")
  imp$ID <- paste(imp$Country.Code,imp$Item.Code,imp$Year, sep = ".")
  CBS$ID <- paste(CBS$Country.Code,CBS$Item.Code,CBS$Year, sep = ".")
  exp$Target <- CBS$Exports[match(exp$ID, CBS$ID)]
  imp$Target <- CBS$Imports[match(imp$ID, CBS$ID)]
  exp$Target[is.na(exp$Target)] <- 0
  imp$Target[is.na(imp$Target)] <- 0
  
  # balance global imports and exports for each commodity
  # (difference between total imports and exports reported in CBS is absorbed by RoW (999))
  # and write the difference into trade targets and CBS
  item=2617
  for(item in unique(CBS$Item.Code)){
    delta <- sum(exp$Target[exp$Item.Code==item]) - sum(imp$Target[imp$Item.Code==item])
    if(delta < 0){
      exp$Target[exp$Item.Code==item & exp$Country.Code==999] <- exp$Target[exp$Item.Code==item & exp$Country.Code==999] - delta
      CBS$Exports[CBS$Item.Code==item & CBS$Country.Code==999] <- exp$Target[exp$Item.Code==item & exp$Country.Code==999] - delta
    } else{
      imp$Target[imp$Item.Code==item & imp$Country.Code==999] <- imp$Target[imp$Item.Code==item & imp$Country.Code==999] + delta
      CBS$Imports[CBS$Item.Code==item & CBS$Country.Code==999] <- imp$Target[imp$Item.Code==item & imp$Country.Code==999] + delta
    }
  }
  
  # Balance Supply and Use for ROW
  CBS$TotalSupply <- CBS$Production + CBS$Imports
  CBS$Balancing[CBS$Country.Code==999] <- CBS$TotalSupply[CBS$Country.Code==999] - CBS$StockVariation[CBS$Country.Code==999] - 
    CBS$Exports[CBS$Country.Code==999] - CBS$Food[CBS$Country.Code==999] - CBS$Feed[CBS$Country.Code==999] - 
    CBS$Processing[CBS$Country.Code==999] - CBS$Seed[CBS$Country.Code==999] - CBS$Waste[CBS$Country.Code==999] - 
    CBS$OtherUses[CBS$Country.Code==999]
  
  # Integrate target imports and BTD_original into BTD_bal
  temp <- BTD_original[1:nrow(imp),1:9]
  temp$From.Country.Code <- 0
  temp$From.Country <- "Target"
  temp[,3:9] <- imp[,c(1:5,8,7)]
  BTD_bal <- rbind(BTD_original[,1:9], temp)
  # start value for RoW is 1 where BTD for RoW==0
  BTD_bal$Value[BTD_bal$From.Country.Code==999 & BTD_bal$Value==0] <- 1
  BTD_bal$Value[BTD_bal$To.Country.Code==999 & BTD_bal$Value==0] <- 1
  # cast list into matrix
  BTD_bal <- data.table::dcast(data.table::as.data.table(BTD_bal), From.Country.Code + From.Country + Item.Code + Item + Year ~ To.Country.Code + To.Country, fun=sum, value.var = "Value")
  BTD_bal <- as.data.frame(BTD_bal)
  BTD_bal[,-(1:5)][! is.finite(BTD_bal[,-(1:5)])] <- 0
  BTD_bal$ID <- paste(BTD_bal$From.Country.Code,BTD_bal$Item.Code,BTD_bal$Year, sep = ".")
  # add target exports to BTD_bal
  BTD_bal$Target <- exp$Target[match(BTD_bal$ID,exp$ID)]
  
  # Integrate target imports into BTD_est
  BTD_est <- rbind(BTD_est[,1:9], temp)
  # cast list into matrix
  BTD_est <- data.table::dcast(data.table::as.data.table(BTD_est), From.Country.Code + From.Country + Item.Code + Item + Year ~ To.Country.Code + To.Country, fun=sum, value.var = "Value")
  BTD_est <- as.data.frame(BTD_est)
  BTD_est[,-(1:5)][! is.finite(BTD_est[,-(1:5)])] <- 0
  BTD_est$ID <- paste(BTD_est$From.Country.Code,BTD_est$Item.Code,BTD_est$Year, sep = ".")
  
  # Integrate values from BTD_est into BTD_bal, where BTD_bal==0
  range <- 6:(ncol(BTD_bal)-2)
  data <- BTD_bal[,range]
  temp <- BTD_est[,range]
  temp[!data==0] <- 0
  compare <- BTD_est[,1:5]
  compare$ID <- paste(compare$From.Country.Code,compare$Item.Code,compare$Year, sep = ".")
  compare$Target <- exp$Target[match(compare$ID,exp$ID)]
  compare$Target[!is.finite(compare$Target)] <- 0
  compare$Actual <- exp$Actual[match(compare$ID,exp$ID)]
  compare$Actual[!is.finite(compare$Actual)] <- 0
  compare$Target <- compare$Target - compare$Actual
  # Scale BTD_est to trade target minus what is already reported in BTD_bal
  temp <- temp * ((compare$Target) / rowSums(temp))
  temp[!is.finite(temp)] <- 0
  temp[temp<0] <- 0
  # replace values smaller or equal to zero with estimates
  data[data==0] <- temp[data==0]
  data[data<0] <- temp[data<0]
  BTD_bal[,range] <- data
  
  # BTD_bal_original <- BTD_bal
  # BTD_bal <- BTD_bal_original
  
  #-------------------------------------------------------------------------------------------
  # 2. RAS
  #-------------------------------------------------------------------------------------------
  print("start RAS")
  item=2960
  check <- data.frame(item = unique(BTD_bal$Item.Code), start = 0, end = 0, residue = 0, iterations = 0)
  for(item in unique(BTD_bal$Item.Code)){
    #print(item)
    data <- BTD_bal[BTD_bal$Item.Code==item & BTD_bal$From.Country.Code>0,]
    targetimp <- as.numeric(BTD_bal[BTD_bal$Item.Code==item & BTD_bal$From.Country.Code==0,6:197])
    targetexp <- data$Target
    actualimp <- colSums(data[,range])
    actualexp <- rowSums(data[,range])
    deltaimp <- sum(actualimp) - sum(targetimp)
    deltaexp <- sum(actualexp) - sum(targetexp)
    delta <- deltaimp + deltaexp
    check$start[check$item==item] <- delta
    i <- 0
    while((suppressWarnings(max(abs(((actualimp - targetimp)/targetimp)[is.finite((actualimp - targetimp)/targetimp)]))>0.01) | 
           suppressWarnings(max(abs(((actualexp - targetexp)/targetexp)[is.finite((actualexp - targetexp)/targetexp)]))>0.01)) & 
          i<20){
      # I get warning in case that all imports or exports of a commodity are zero. There are actually commodities that are not traded at all. 
      # So these warnings cannot be avoided.
      mult.exp <- targetexp / actualexp
      mult.exp[!is.finite(mult.exp)] <- 0
      data[,range] <- data[,range] * mult.exp
      actualimp <- colSums(data[,range])
      mult.imp <- targetimp / actualimp
      mult.imp[!is.finite(mult.imp)] <- 0
      data[,range] <- as.matrix(data[,range]) %*% diag(mult.imp)
      actualimp <- colSums(data[,range])
      actualexp <- rowSums(data[,range])
      deltaimp <- sum(actualimp) - sum(targetimp)
      deltaexp <- sum(actualexp) - sum(targetexp)
      delta <- deltaimp + deltaexp
      i <- i+1
    }
    check$end[check$item==item] <- delta
    check$iterations[check$item==item] <- i
    BTD_bal[BTD_bal$Item.Code==item & BTD_bal$From.Country.Code>0,] <- data
  }
  check$residue <- check$end / check$start * 100
  print("end RAS")
  
  
  #-------------------------------------------------------------------------------------------
  # 3. Convert the balanced trade matrix into list
  #-------------------------------------------------------------------------------------------
  regions <- read.csv2("./inst/fabio_input/Regions_all.csv")
  BTD <- reshape2::melt(BTD_bal[,1:(ncol(BTD_bal)-2)], id=c("From.Country.Code","From.Country","Item.Code","Item","Year"), variable.name="To.Country", value.name = "Value")
  BTD$Value <- as.numeric(BTD$Value)
  BTD$Item <- as.character(BTD$Item)
  BTD$To.Country <- as.character(BTD$To.Country)
  BTD$To.Country.Code <- substr(BTD$To.Country,1,unlist(gregexpr("_",BTD$To.Country))-1)
  BTD$To.Country.Code <- as.numeric(BTD$To.Country.Code)
  BTD$To.Country <- regions$Country[match(BTD$To.Country.Code,regions$Country.Code)]
  BTD <- BTD[,c(1,2,8,6,3,4,5,7)]
  BTD$ID <- paste(BTD$From.Country.Code,BTD$To.Country.Code,BTD$Item.Code, sep = ".")
  BTD <- BTD[! BTD$From.Country=="Target",]
  
  # save results
  save(BTD, file = paste0("/mnt/nfs_fineprint/tmp/fabio/data/yearly/",year,"_BTD_balanced.RData"))
  save(CBS, file = paste0("/mnt/nfs_fineprint/tmp/fabio/data/yearly/",year,"_CBS_balanced.RData"))
  
  return(year)
}


library(parallel)
# Calculate the number of cores
no_cores <- detectCores() - 1
# Initiate cluster
cl <- makeCluster(no_cores)
# Years to run
years <- 1986:2013
# start parallel
parLapply(cl, years, fabio_BTD_balancing)
# stop cluster
stopCluster(cl)

