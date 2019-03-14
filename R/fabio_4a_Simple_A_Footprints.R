##############################################################################################
##  
##  FABIO: BUILD MRIO TABLE BASED ON FAOSTAT COMMODITY BALANCE SHEETS AND TRADE DATA
##    3c Derive Leontief Inverse
##  
##############################################################################################

library(reshape2)
library(data.table)
library(Matrix)
library(xlsx)

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
  load(file=paste0("data/yearly/",year,"_A.RData"))
  load(file=paste0("data/yearly/",year,"_Z_usd.RData"))
  load(file=paste0("data/yearly/",year,"_Y.RData"))
  
  #-------------------------------------------------------------------------
  # Prepare Extension
  #-------------------------------------------------------------------------
  x <- rowSums(Z) + rowSums(Y)
  rm(Z); gc()
  
  ext <- data.frame(Item.Code = rep(items$Item.Code, 192),
                    Item = rep(items$Item, 192),
                    Com.Code = rep(items$Com.Code, 192),
                    Group = rep(items$Group, 192),
                    Value = x)
  ext$Value[!items$Group=="Primary crops"] <- 0
  
  MP <- as.numeric(ext$Value)
  for(i in 1:10){
    MP <- MP %*% as.matrix(A)
  }
  
  FP <- as.data.frame(as.matrix(Y) * as.vector(MP))
  FP$Com.Code <- ext$Com.Code
  FP$Item <- ext$Item
  FP <- as.data.frame(melt(as.data.table(FP), id.vars = c("Com.Code","Item"), variable.name = "ISO", value.name = "Value"))
  FP$FDcategory <- substr(FP$ISO,5,100)
  FP$ISO <- substr(FP$ISO,1,3)
  FP <- as.data.frame(dcast(as.data.table(FP), ISO + Com.Code + Item ~ FDcategory, fun.aggregate = sum, value.var = "Value"))
  
  write.xlsx(FP, file=paste0("results/",year,"_FP_simple.xlsx"), sheetName = "Biomass (tonnes)", row.names = F)
  
  
  rm(A,Z,Y); gc()
  
}

#------------------
# Checks
#------------------
Zat <- as.data.frame(as.matrix(Z[(8*130+1):(9*130),(8*130+1):(9*130)]))
Zat$FD <- rowSums(Y[(8*130+1):(9*130),])
Zat$Total <- rowSums(Zat)
Zat$Com.Code <- items$Com.Code
Zat$Item <- items$Item
colnames(Zat) <- items$Item
rownames(Zat) <- items$Item
write.xlsx(Zat, file=paste0("results/",year,"_Z_AT_usd_neu.xlsx"))

Vat <- as.data.frame(as.matrix(mr_sup[(8*116+1):(9*116),(8*130+1):(9*130)]))
Vat_usd <- as.data.frame(as.matrix(mr_sup_usd[(8*116+1):(9*116),(8*130+1):(9*130)]))
write.xlsx(Vat, file=paste0("results/",year,"_V_AT.xlsx"))
write.xlsx(Vat_usd, file=paste0("results/",year,"_V_AT_usd.xlsx"))

Uat <- as.data.frame(as.matrix(mr_use[(8*130+1):(9*130),(8*116+1):(9*116)]))
Uat$FD <- rowSums(Y[(8*130+1):(9*130),])
write.xlsx(Uat, file=paste0("results/",year,"_U_AT_neu.xlsx"))
