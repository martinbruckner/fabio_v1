################################################
# aggregate products & invert FABIO
################################################
rm(list=ls()); gc()

# Years to calculate hybridised FABIO for
years <- 1986:2013
# year = 2012
require(Matrix) # Necessary for forked processes

fabio_inverse <- function(year){
  print(year)
  # invert Z_mass
  Z_m <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_Z_mass.rds"))
  X <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_X.rds"))
  
  A <- t(t(Z_m)/X)
  A[!is.finite(A)] <- 0
  A[A<0] <- 0
  diag(A)[diag(A)==1] <- 1 - 1e-10
  
  L <- diag(nrow(A))-A
  L <- solve(L, tol = 1.0e-60)
  
  saveRDS(L, paste0("/mnt/nfs_fineprint/tmp/fabio/", year, "_L_mass.rds"))
  # saveRDS(L, paste0("../wu_share/WU/Projekte/GRU/04_Daten/", year, "_L_mass.rds"))
  
  # invert Z_price
  Z_p <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_Z_price.rds"))
  
  A <- t(t(Z_p)/X)
  A[!is.finite(A)] <- 0
  A[A<0] <- 0
  diag(A)[diag(A)==1] <- 1 - 1e-10
  
  L <- diag(nrow(A))-A
  L <- solve(L, tol = 1.0e-60)
  
  saveRDS(L, paste0("/mnt/nfs_fineprint/tmp/fabio/", year, "_L_price.rds"))
  # saveRDS(L, paste0("../wu_share/WU/Projekte/GRU/04_Daten/", year, "_L_price.rds"))
}

for(year in years){
  fabio_inverse(year=year)
}

# library(parallel)
# # Calculate the number of cores
# no_cores <- detectCores() - 2
# # run junks of 4 years (for memory reasons)
# for(i in 0:6){
#   # Initiate cluster
#   cl <- makeCluster(no_cores)
#   # Years to run
#   years <- (1986+4*i):(1986+4*i+3)
#   # if(2013 %in% years) years <- 2010:2013
#   print(years)
#   # start parallel
#   parLapply(cl, years, fabio_inverse)
#   # stop cluster
#   stopCluster(cl)
# }
