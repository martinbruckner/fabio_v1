##############################################################################################
##  
##  FABIO: BUILD MRIO TABLE BASED ON FAOSTAT COMMODITY BALANCE SHEETS AND TRADE DATA
##    2d Trade linking of Use tables
##  
##############################################################################################

library(reshape2)
library(data.table)
library(Matrix)

rm(list=ls()); gc()

##########################################################################
# Make intitial settings
##########################################################################
# read region classification
regions <- read.csv(file="./inst/fabio_input/Regions.csv", header=TRUE, sep=";")
# read commodity classification
items <- read.csv(file="./inst/fabio_input/Items.csv", header=TRUE, sep=";")


##########################################################################
# Start loop for a series of years
##########################################################################
# year=1986
# year=2013
# for(year in 1986:2013){
fabio_trade_use <- function(year, regions, items){
  print(year)
  is.finite.data.frame <- function(x) do.call(cbind, lapply(x, is.finite))
  #-------------------------------------------------------------------------
  # Read data
  #-------------------------------------------------------------------------
  load(file=paste0("/mnt/nfs_fineprint/tmp/fabio/data/yearly/",year,"_use.RData"))
  load(file=paste0("/mnt/nfs_fineprint/tmp/fabio/data/yearly/",year,"_BTD_balanced.RData"))
  load(file=paste0("/mnt/nfs_fineprint/tmp/fabio/data/yearly/",year,"_CBS_balanced_postuseallocation.RData"))
  nrproc <- length(unique(use$Proc.Code))
  nrcom <- length(unique(use$Com.Code))
  nrreg <- nrow(regions)
  
  #-------------------------------------------------------------------------
  # Multi-regional Use Tables
  #-------------------------------------------------------------------------
  # cast BTD
  BTD$Com.Code <- items$Com.Code[match(BTD$Item.Code,items$Item.Code)]
  BTD$From.ISO <- regions$ISO[match(BTD$From.Country.Code,regions$Country.Code)]
  BTD$To.ISO <- regions$ISO[match(BTD$To.Country.Code,regions$Country.Code)]
  BTDcast <- as.data.frame(data.table::dcast(data.table::as.data.table(BTD), From.Country.Code + From.ISO + Com.Code ~ To.Country.Code, value.var = "Value", fun.aggregate = sum))
  
  # cast Production
  CBS$Com.Code <- items$Com.Code[match(CBS$Item.Code,items$Item.Code)]
  Prod <- as.data.frame(data.table::dcast(data.table::as.data.table(CBS), Country.Code + Com.Code ~ Country.Code, value.var = "Production", fun.aggregate = sum))
  
  # Add cast Production and cast BTD to a total supply matrix
  Total <- Prod
  Total[,-(1:2)] <- Prod[,-(1:2)] + BTDcast[,-(1:3)]
  
  # cast Total Supply
  TScast <- stats::aggregate(. ~ Com.Code, Total[,-1], sum)
  TScast <- TScast[match(Total$Com.Code,TScast$Com.Code),]
  
  # calculate supply shares, i.e. the percentage of total supply of each commodity supplied by each country
  SupplyShares <- Total[,-(1:2)] / TScast[,-1]
  SupplyShares[!is.finite(SupplyShares)] <- 0
  SupplyShares <- as.matrix(SupplyShares)
  # negatives <- data.frame(Value = SupplyShares[SupplyShares < 0])
  
  rm(Prod,TScast,BTD,BTDcast,CBS); gc()
  
  # melt and cast use table
  use <- as.data.frame(data.table::melt(data.table::as.data.table(use), id.vars=1:6, variable.name = "ISO"))
  use$Country.Code <- regions$Country.Code[match(use$ISO,regions$ISO)]
  use <- as.data.frame(data.table::dcast(data.table::as.data.table(use), Com.Code ~ Country.Code + Proc.Code, value.var = "value", fun.aggregate = sum))
  
  # trade-linking of use table
  mr_use <- use[match(Total$Com.Code,use$Com.Code),]
  mr_use <- as.matrix(mr_use[,-1])
  #i=0
  for(i in (1:nrreg)-1){
    # Attention: data.frame can be multiplied with data.frame, or matrix with matrix, not matrix with data.frame!
    mr_use[,(1:nrproc)+i*nrproc] <- mr_use[,(1:nrproc)+i*nrproc] * SupplyShares[,i+1]
  }
  # mr_use[!is.finite(mr_use)] <- 0
  
  # trade-linking of final demand use table
  mr_use_fd <- use_FD[match(Total$Com.Code,use$Com.Code),]
  mr_use_fd <- as.matrix(mr_use_fd[,-(1:3)])
  #i=0
  for(i in (1:nrreg)-1){
    mr_use_fd[,(1:4)+i*4] <- mr_use_fd[,(1:4)+i*4] * SupplyShares[,i+1]
  }
  gc()
  # mr_use_fd[!is.finite(mr_use_fd)] <- 0
  
  # save results
  save(mr_use, mr_use_fd, file=paste0("/mnt/nfs_fineprint/tmp/fabio/data/yearly/",year,"_mr_use.RData"))
  
  # rm(mr_use,mr_use_fd,SupplyShares,Total,use,use_FD); gc()
  return(year)
}


library(parallel)
# Calculate the number of cores
no_cores <- detectCores() - 2

# run junks of 6 years (for memory reasons)
for(i in 0:4){
  # Initiate cluster
  cl <- makeCluster(no_cores)
  # Years to run
  years <- (1986+6*i):(1986+6*i+5)
  if(2013 %in% years) years <- 2010:2013
  print(years)
  # start parallel
  parLapply(cl, years, fabio_trade_use, items=items, regions=regions)
  # stop cluster
  stopCluster(cl)
}


# for(year in 1986:2013){
#   fabio_trade_use(year=year, items=items, regions=regions)
# }

