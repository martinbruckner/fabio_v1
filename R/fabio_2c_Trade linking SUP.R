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

is.finite.data.frame <- function(x) do.call(cbind, lapply(x, is.finite))
##########################################################################
# Make intitial settings
##########################################################################
# read region classification
regions <- read.csv(file="Regions.csv", header=TRUE, sep=";")
# read commodity classification
items <- read.csv(file="Items.csv", header=TRUE, sep=";")


##########################################################################
# Start loop for a series of years
##########################################################################
# year=1986
year=2013
for(year in 1986:2013){
  print(year)
  #-------------------------------------------------------------------------
  # Read data
  #-------------------------------------------------------------------------
  load(file=paste0("data/yearly/",year,"_sup.RData"))
  load(file=paste0("data/yearly/",year,"_sup_usd.RData"))
  load(file=paste0("data/yearly/",year,"_BTD_Balanced.RData"))
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
    data <- data.table(sup[,1:5], Value = sup[,iso])
    data <- dcast(data, Proc.Code + Process ~ Com.Code, value.var = "Value")
    data[!is.finite(data)] <- 0
    rownames(data) <- data$Proc.Code
    supply[[iso]] <- as.matrix(data[,-(1:2)])
    
    data <- data.table(sup_usd[,1:5], Value = sup_usd[,iso])
    data <- dcast(data, Proc.Code + Process ~ Com.Code, value.var = "Value")
    data[!is.finite(data)] <- 0
    rownames(data) <- data$Proc.Code
    supply_usd[[iso]] <- as.matrix(data[,-(1:2)])
  }
  
  # make block diagonal matrix with supply tables = multi-regional supply table
  mr_sup <- bdiag(supply)
  mr_sup_usd <- bdiag(supply_usd)
  
  
  # save results
  save(mr_sup, file=paste0("data/yearly/",year,"_mr_sup.RData"))
  save(mr_sup_usd, file=paste0("data/yearly/",year,"_mr_sup_usd.RData"))
  
}

