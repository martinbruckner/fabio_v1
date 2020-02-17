##############################################################################################
##  
##  FABIO: BUILD MRIO TABLE BASED ON FAOSTAT COMMODITY BALANCE SHEETS AND TRADE DATA
##    3b Clean Multi-regional Input-Output Tables
##  
##############################################################################################

library(Matrix)

rm(list=ls()); gc()


##########################################################################
# Start loop for a series of years
##########################################################################
# year=1986
year=2013
for(year in 1986:2013){
  print(year)
  gc()
  #-------------------------------------------------------------------------
  # Read data
  #-------------------------------------------------------------------------
  is.finite.data.frame <- function(x) do.call(cbind, lapply(x, is.finite))
  # read region classification
  regions <- read.csv(file="./inst/fabio_input/Regions.csv", header=TRUE, sep=";")
  # read commodity classification
  items <- read.csv(file="./inst/fabio_input/Items.csv", header=TRUE, sep=";")
  
  # load data
  Zp <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_Z_price.rds"))
  Zm <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_Z_mass.rds"))
  Y <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_Y.rds"))
  X <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_X.rds"))
  
  
  # save results
  saveRDS(Zm, file=paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_Z_mass.rds"))
  saveRDS(Zp, file=paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_Z_price.rds"))
  saveRDS(Y, file=paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_Y.rds"))
  saveRDS(X, file=paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_X.rds"))
  
  rm(list=ls()); gc()
}

