################################################
# aggregate products & invert FABIO
################################################

# Years to calculate hybridised FABIO for
years <- 1986:2013

require(Matrix) # Necessary for forked processes

fabio_inverse <- function(year){
  print(year)
  Z_m <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/120/",year,"_Z_mass.rds"))
  Z_p <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/120/",year,"_Z_price.rds"))
  X <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/120/",year,"_X.rds"))
  
  A <- t(t(Z_m)/X)
  A[!is.finite(A)] <- 0
  A[A<0] <- 0
  diag(A)[diag(A)==1] <- 1 - 1e-10
  
  L <- diag(nrow(A))-A
  L <- solve(L, tol = 1.0e-22)
  
  saveRDS(L, paste0("/mnt/nfs_fineprint/tmp/fabio/120/", year, "_L_mass.rds"))
  
  A <- t(t(Z_p)/X)
  A[!is.finite(A)] <- 0
  A[A<0] <- 0
  diag(A)[diag(A)==1] <- 1 - 1e-10
  
  L <- diag(nrow(A))-A
  L <- solve(L, tol = 1.0e-22)
  
  saveRDS(L, paste0("/mnt/nfs_fineprint/tmp/fabio/120/", year, "_L_price.rds"))
  
}

for(year in years){
  fabio_inverse(year=year)
}


