##############################################################################################
##  
##  FABIO: BUILD MRIO TABLE BASED ON FAOSTAT COMMODITY BALANCE SHEETS AND TRADE DATA
##    3b Derive A Matrix
##  
##############################################################################################

library(reshape2)
library(data.table)
library(Matrix)
library(rmatio)

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
  load(file=paste0("data/yearly/",year,"_Z.RData"))
  load(file=paste0("data/yearly/",year,"_X.RData"))
  
  #-------------------------------------------------------------------------
  # Prepare A-Matrix and L-Inverse
  #-------------------------------------------------------------------------
  A <- t(t(Z)/X)
  A[!is.finite(A)] <- 0
  A[A<0] <- 0
  # Don't do this! In a physical IOT a sector might have higher inputs than outputs, e.g. input: tonnes of feed -> output: thousand heads of animals
  # A[A>1] <- 0.999
  
  save(A, file=paste0("data/yearly/",year,"_A.RData"))
  # write.mat(list(A=A), paste0("./data/yearly/FABIO matlab/",year,"_A.mat"))
  
  rm(A); gc()
  
}


year=2013
for(year in 1986:2013){
  print(year)
  load(file=paste0("data/yearly/",year,"_A.RData"))
  write.mat(list(A=A), paste0("data/yearly/",year,"_A2.mat"), compression = F)
}