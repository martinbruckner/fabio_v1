##############################################################################################
##  
##  FABIO: BUILD MRIO TABLE BASED ON FAOSTAT COMMODITY BALANCE SHEETS AND TRADE DATA
##    3b Derive A Matrix
##  
##############################################################################################

library(reshape2)
library(data.table)
library(Matrix)

rm(list=ls()); gc()

reduce_matrix <- function(x,y){
  if(length(y)==nrow(x)) x <- x[y!=0,y!=0]
  return(x)
}

refill_matrix <- function(x,y){
  if(length(y)==nrow(x)) return(x)
  id <- 1:length(y)
  id <- id[y!=0]
  filled <- matrix(0,length(y),length(y))
  filled[id,id] <- x
  rownames(filled) <- names(y)
  return(filled)
}

is.finite.data.frame <- function(x) do.call(cbind, lapply(x, is.finite))

mmult <- function(A, name1, name2, year, X){
  A <- reduce_matrix(A, X)
  if(name1 != "A") load(file=paste0("./data/yearly/",year,"_",name1,".RData"))
  
  mat <- A %*% get(name1)
  mat[!is.finite(mat)] <- 0
  mat[mat<0] <- 0
  assign(name2, mat)
  save(list = name2, file=paste0("./data/yearly/",year,"_",name2,".RData"))
  return(NULL)
}

load_object <- function(file) {
  tmp <- new.env()
  load(file = file, envir = tmp)
  tmp[[ls(tmp)[1]]]
}

addmat <- function(year, end=99, i=1, X){
  if(i==end+1) {
    return(0)
  } else {
    # end=2;i=1
    A <- addmat(year,end,i=i+1, X)
    B <- load_object(paste0("./data/yearly/",year,"_A",c("",2:end)[i],".RData"))
    B <- reduce_matrix(B, X)
    L <- A + B
    return(L)
  }
}


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
year=1986
year=2013
for(year in 1986:2013){
  print(year)
  #-------------------------------------------------------------------------
  # Read data
  #-------------------------------------------------------------------------
  load(file=paste0("./data/yearly/",year,"_A.RData"))
  load(file=paste0("./data/yearly/",year,"_X.RData"))
  
  A <- reduce_matrix(A,X)
  gc()
  
  #-------------------------------------------------------------------------
  # Prepare L-Inverse by power series expansion
  #-------------------------------------------------------------------------
  # name1="A"
  # name2="A2"
  mmult(A, "A", "A2", year, X)
  mmult(A, "A2", "A3", year, X)
  mmult(A, "A3", "A4", year, X)
  mmult(A, "A4", "A5", year, X)
  mmult(A, "A5", "A6", year, X)
  mmult(A, "A6", "A7", year, X)
  mmult(A, "A7", "A8", year, X)
  
  rm(A); gc()
  
  L <- addmat(year = year, end = 8, i = 1, X)
  L <- diag(nrow(L)) + L
  L <- refill_matrix(L,X)
  save(L, file=paste0("./data/yearly/",year,"_L.RData"))
  rm(L,X); gc()
  
}

