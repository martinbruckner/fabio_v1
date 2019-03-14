##############################################################################################
##  
##  FABIO: BUILD MRIO TABLE BASED ON FAOSTAT COMMODITY BALANCE SHEETS AND TRADE DATA
##    3c Derive Leontief Inverse
##  
##############################################################################################

library(reshape2)
library(data.table)
library(Matrix)
library(MASS)
library(pracma)
library(corpcor)

rm(list=ls()); gc()

is.finite.data.frame <- function(x) do.call(cbind, lapply(x, is.finite))
##########################################################################
# Make intitial settings
##########################################################################
# read region classification
reg <- read.csv(file="Regions.csv", header=TRUE, sep=";")
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
  load(file=paste0("data/yearly/",year,"_A.RData"))
  load(file=paste0("data/yearly/",year,"_x.RData"))
  
  # A <- as.matrix(A)
  
  
  #-------------------------------------------------------------------------
  # Prepare L-Inverse
  #-------------------------------------------------------------------------
  
  #A1 <- A[1:500,1:500]
  #X1 <- X[1:500]
  
  reduce_matrix <- function(x,y){
    if(length(y)==nrow(x)) x <- x[y!=0,y!=0]
    return(x)
  }
  
  A <- reduce_matrix(A, X)
  gc()
  
  #L <- chol2inv(base::chol(diag(nrow(A2)) - A2, pivot = T))
  
  a <- Sys.time()
  L <- solve(diag(nrow(A)) - A)
  print(Sys.time() - a)
  save(L, file=paste0("data/yearly/",year,"_L_solve.RData"))
  #X[X!=0][8952]
  
  a <- Sys.time()
  L <- qr.solve(diag(nrow(A)) - A)
  print(Sys.time() - a)
  save(L, file=paste0("data/yearly/",year,"_L_qrsolve.RData"))
  
  a <- Sys.time()
  L <- MASS::ginv(diag(nrow(A)) - A)  # 1.05 hours
  print(Sys.time() - a)
  save(L, file=paste0("data/yearly/",year,"_L_ginv.RData"))
  
  a <- Sys.time()
  L <- pracma::pinv(diag(nrow(A)) - A)  # 1.69 hours
  print(Sys.time() - a)
  
  a <- Sys.time()
  L <- corpcor::pseudoinverse(diag(nrow(A)) - A)  # 1.38 hours
  print(Sys.time() - a)
  
  rm(L); gc()
  
}





##########################################################################
# 
# 
# 
# install.packages("R.matlab")
# library(R.matlab)
# writeMat(con=paste0(getwd(),"/data/",year,"/A1.mat"), A1=A[,1:10000])
# writeMat(con=paste0(getwd(),"/data/",year,"/A2.mat"), A2=A[,10001:17500])
# 
# 
# 
# Sys.time()
# L <- solve(IminusA)
# Sys.time()
# 
# library(MASS)
# L <- ginv(IminusA)
# # install.packages("pracma")
# library(pracma)
# L <- pinv(IminusA)
# 
# install.packages("corpcor")
# library(corpcor)
# Sys.time()
# L <- pseudoinverse(I-A)
# Sys.time()
# 
# #######################################
# ia1 <- IminusA[1:1000,1:1000]
# Sys.time()
# l1 <- pseudoinverse(ia1)
# Sys.time()
# ia2 <- IminusA[1:2000,1:2000]
# Sys.time()
# l2 <- pseudoinverse(ia2)
# Sys.time()
# ia3 <- IminusA[1:3000,1:3000]
# Sys.time()
# l3 <- pseudoinverse(ia3)
# Sys.time()
# ia4 <- IminusA[1:4000,1:4000]
# Sys.time()
# l4 <- pseudoinverse(ia4)
# Sys.time()
# ia5 <- IminusA[1:5000,1:5000]
# Sys.time()
# l5 <- pseudoinverse(ia5)
# Sys.time()
# rm(ia1,ia2,ia3,ia4,ia5,l1,l2,l3,l4,l5)
# gc()
# #######################################
# library(MASS)
# ia1 <- IminusA[1:1000,1:1000]
# Sys.time()
# l1 <- ginv(ia1)
# Sys.time()
# ia2 <- IminusA[1:2000,1:2000]
# Sys.time()
# l2 <- ginv(ia2)
# Sys.time()
# ia3 <- IminusA[1:3000,1:3000]
# Sys.time()
# l3 <- ginv(ia3)
# Sys.time()
# rm(ia1,ia2,ia3,l1,l2,l3)
# gc()
# #######################################
# library(pracma)
# ia1 <- IminusA[1:1000,1:1000]
# Sys.time()
# l1 <- pinv(ia1)
# Sys.time()
# ia2 <- IminusA[1:2000,1:2000]
# Sys.time()
# l2 <- pinv(ia2)
# Sys.time()
# ia3 <- IminusA[1:3000,1:3000]
# Sys.time()
# l3 <- pinv(ia3)
# Sys.time()
# rm(ia1,ia2,ia3,l1,l2,l3)
# gc()
# #######################################