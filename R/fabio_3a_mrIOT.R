##############################################################################################
##  
##  FABIO: BUILD MRIO TABLE BASED ON FAOSTAT COMMODITY BALANCE SHEETS AND TRADE DATA
##    3a Construct Multi-regional Input-Output Tables
##  
##############################################################################################

library(reshape2)
library(data.table)
library(Matrix)
library(MASS)

rm(list=ls()); gc()


##########################################################################
# Start loop for a series of years
##########################################################################
# year=1986
# year=2013
for(year in 1986:2013){
  print(year)
  #-------------------------------------------------------------------------
  # Read data
  #-------------------------------------------------------------------------
  is.finite.data.frame <- function(x) do.call(cbind, lapply(x, is.finite))
  # read region classification
  regions <- read.csv(file="./inst/fabio_input/Regions.csv", header=TRUE, sep=";")
  # read commodity classification
  items <- read.csv(file="./inst/fabio_input/Items.csv", header=TRUE, sep=";")
  # load supply and use tables
  load(file=paste0("/mnt/nfs_fineprint/tmp/fabio/data/yearly/",year,"_mr_sup.RData"))
  load(file=paste0("/mnt/nfs_fineprint/tmp/fabio/data/yearly/",year,"_mr_sup_usd.RData"))
  load(file=paste0("/mnt/nfs_fineprint/tmp/fabio/data/yearly/",year,"_mr_use.RData"))
  nrreg <- nrow(regions)
  nrproc <- ncol(mr_use) / nrreg
  nrcom <- nrow(mr_use) / nrreg
  
  
  #----------------------------------
  # derive Z based on price allocation
  #----------------------------------
  # calculate product mix matrix or Transformation matrix (T)
  g <- rowSums(mr_sup_usd)
  Trans <- as.matrix(mr_sup_usd) / g
  rm(mr_sup_usd, g); gc()
  Trans[!is.finite(Trans)] <- 0
  # gc()
  
  # calculate Z matrix (U * T)
  Z <- mr_use %*% Trans
  
  # save results
  saveRDS(Z, file=paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_Z_price.rds"))
  
  
  #----------------------------------
  # derive Z based on mass allocation
  #----------------------------------
  g <- rowSums(mr_sup)
  Trans <- as.matrix(mr_sup) / g
  rm(mr_sup, g); gc()
  Trans[!is.finite(Trans)] <- 0
  # gc()
  
  # calculate Z matrix (U * T)
  Z <- mr_use %*% Trans
  Y <- mr_use_fd
  X <- rowSums(Z) + rowSums(Y)
  
  # save results
  saveRDS(Z, file=paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_Z_mass.rds"))
  saveRDS(Y, file=paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_Y.rds"))
  saveRDS(X, file=paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_X.rds"))
  
  rm(list=ls()); gc()
  
  
  
  #----------------------------------
  # alternative approaches
  #----------------------------------
  # 
  # # calculate input coefficient matrix
  # q <- rowSums(mr_use) + rowSums(mr_use_fd)
  # B <- mr_use / q
  # B[!is.finite(B)] <- 0
  # rm(mr_use,mr_sup_usd, q); gc()
  #
  # # calculate Z matrix (B * V)
  # Z <- B %*% mr_sup  # or multiply with mr_sup_usd
  #
  # # calculate Industry-output-proportions matrix
  # C_usd <- t(mr_sup_usd / rowSums(mr_sup_usd))
  # C <- t(mr_sup / rowSums(mr_sup))
  # C_usd_inv <- ginv(C_usd)
  # 
  # T_usd <- mr_sup_usd / rowSums(mr_sup_usd)
  # T_ton <- mr_sup / rowSums(mr_sup)
  # 
  # V_usd_inv <- ginv(t(as.matrix(mr_sup_usd)))
  # 
  # T2_usd <- V_usd_inv %*% diag(q)
}



