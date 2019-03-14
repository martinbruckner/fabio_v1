load("W:/WU/Projekte/GRU/01_Projekte/1305_Bonn BioMRIO/02_FABIO/01_code & data/data/yearly/2013_Y.RData")

agg <- function(x)
{
  x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x))
  return(x)
}

colnames(Y) <- rep(c("Food","OtherUses","StockVariation","Balancing"),192)
Y <- agg(Y)
Y <- t(Y)
colnames(Y) <- rep(1:130,192)
Y <- agg(Y)
Y <- t(Y)

write.csv(Y,"check_final_demand_2013.csv")
