##############################################################################################
##  
##  FABIO: BUILD MRIO TABLE BASED ON FAOSTAT COMMODITY BALANCE SHEETS AND TRADE DATA
##    2c Trade linking of Supply tables
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
fabio_trade_sup <- function(year, regions, items){
  print(year)
  is.finite.data.frame <- function(x) do.call(cbind, lapply(x, is.finite))
  #-------------------------------------------------------------------------
  # Read data
  #-------------------------------------------------------------------------
  load(file=paste0("/mnt/nfs_fineprint/tmp/fabio/data/yearly/",year,"_sup.RData"))
  load(file=paste0("/mnt/nfs_fineprint/tmp/fabio/data/yearly/",year,"_sup_usd.RData"))
  nrproc <- length(unique(sup$Proc.Code))
  nrcom <- length(unique(sup$Com.Code))
  nrreg <- nrow(regions)
  
  
  #-------------------------------------------------------------------------
  # Multi-regional Supply Tables
  #-------------------------------------------------------------------------
  # cast supply tables and store in list
  supply <- list()
  supply_usd <- list()
  
  # iso="USA"
  for(iso in regions$ISO){
    data <- data.table::data.table(sup[,1:5], Value = sup[,iso])
    data <- data.table::dcast(data, Proc.Code + Process ~ Com.Code, value.var = "Value")
    data[!is.finite(data)] <- 0
    rownames(data) <- data$Proc.Code
    supply[[iso]] <- as.matrix(data[,-(1:2)])
    
    data <- data.table::data.table(sup_usd[,1:5], Value = sup_usd[,iso])
    data <- data.table::dcast(data, Proc.Code + Process ~ Com.Code, value.var = "Value")
    data[!is.finite(data)] <- 0
    rownames(data) <- data$Proc.Code
    supply_usd[[iso]] <- as.matrix(data[,-(1:2)])
  }
  
  # make block diagonal matrix with supply tables = multi-regional supply table
  mr_sup <- Matrix::bdiag(supply)
  mr_sup_usd <- Matrix::bdiag(supply_usd)
  
  
  # save results
  save(mr_sup, file=paste0("/mnt/nfs_fineprint/tmp/fabio/data/yearly/",year,"_mr_sup.RData"))
  save(mr_sup_usd, file=paste0("/mnt/nfs_fineprint/tmp/fabio/data/yearly/",year,"_mr_sup_usd.RData"))
  
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
parLapply(cl, years, fabio_trade_sup, items=items, regions=regions)
# stop cluster
stopCluster(cl)
