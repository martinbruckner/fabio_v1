##############################################################################################
##  
##  FABIO: BUILD MRIO TABLE BASED ON FAOSTAT COMMODITY BALANCE SHEETS AND TRADE DATA
##    3a-c Construct Z, A & L
##  
##############################################################################################

library(reshape2)
library(data.table)
library(Matrix)
library(MASS)

rm(list=ls()); gc()


# year=1986
year=2013

#-------------------------------------------------------------------------
# Read data
#-------------------------------------------------------------------------
is.finite.data.frame <- function(x) do.call(cbind, lapply(x, is.finite))
# read region classification
regions <- read.csv(file="Regions.csv", header=TRUE, sep=";")
# read commodity classification
items <- read.csv(file="Items.csv", header=TRUE, sep=";")
# load supply and use tables
load(file=paste0("data/yearly/",year,"_mr_sup.RData"))
load(file=paste0("data/yearly/",year,"_mr_sup_usd.RData"))
load(file=paste0("data/yearly/",year,"_mr_use.RData"))
nrreg <- nrow(regions)
nrproc <- ncol(mr_use) / nrreg
nrcom <- nrow(mr_use) / nrreg


#-------------------------------------------------------------------------
# Prepare Z-Matrix
#-------------------------------------------------------------------------
# calculate product mix matrix or Transformation matrix (T)
g <- rowSums(mr_sup_usd)
Trans <- as.matrix(mr_sup_usd) / g
rm(mr_sup, mr_sup_usd, g); gc()
Trans[!is.finite(Trans)] <- 0
gc()

# calculate Z matrix (U * T)
a <- Sys.time()
Z <- mr_use %*% Trans
print(Sys.time() - a)

Y <- mr_use_fd

X <- rowSums(Z) + rowSums(Y)

# save results
save(Z, file=paste0("data/yearly/",year,"_Z.RData"))
save(Y, file=paste0("data/yearly/",year,"_Y.RData"))
save(X, file=paste0("data/yearly/",year,"_X.RData"))

rm(list=ls()); gc()


#-------------------------------------------------------------------------
# Prepare A-Matrix
#-------------------------------------------------------------------------
load(file=paste0("data/yearly/",year,"_Z.RData"))
load(file=paste0("data/yearly/",year,"_X.RData"))
load(file=paste0("data/yearly/",year,"_Y.RData"))

# Add ROW to Balancing
Z[191*130+1,] <- colSums(Z[(191*130+1):(192*130),])
Y[,"ROW_Balancing"] <- Y[,"ROW_Balancing"] + rowSums(Z[,(191*130+1):(192*130)])
Z <- Z[1:(191*130),1:(191*130)]
X <- X[1:(191*130)]

A <- t(t(Z)/X)
rm(Z); gc()
A[!is.finite(A)] <- 0
A[A<0] <- 0
# Don't do this! In a physical IOT a sector might have higher inputs than outputs, e.g. input: tonnes of feed -> output: thousand heads of animals
# A[A>1] <- 0.999

save(A, file=paste0("data/yearly/",year,"_A.RData"))

rm(list=ls()); gc()


#-------------------------------------------------------------------------
# Prepare L-Inverse
#-------------------------------------------------------------------------
load(file=paste0("data/yearly/",year,"_A.RData"))
load(file=paste0("data/yearly/",year,"_X.RData"))

reduce_matrix <- function(x,y){
  x <- x[y!=0,y!=0]
  return(x)
}

refill_matrix <- function(x,y){
  id <- 1:length(y)
  id <- id[y!=0]
  filled <- matrix(0,length(y),length(y))
  filled[id,id] <- x
  rownames(filled) <- names(y)
  return(filled)
}

A <- reduce_matrix(A, X)

L <- chol2inv(base::chol(diag(nrow(A)) - A))
# L <- chol2inv(base::chol(diag(nrow(A)) - A, pivot = T))

a <- Sys.time()
L <- solve(diag(nrow(A)) - A)
print(Sys.time() - a)
save(L, file=paste0("data/yearly/",year,"_L.RData"))
#X[X!=0][8952]

a <- Sys.time()
L <- qr.solve(diag(nrow(A)) - A)
print(Sys.time() - a)
save(L, file=paste0("data/yearly/",year,"_L.RData"))

a <- Sys.time()
L <- ginv(diag(nrow(A)) - A)
print(Sys.time() - a)

save(L, file=paste0("data/yearly/",year,"_L.RData"))

