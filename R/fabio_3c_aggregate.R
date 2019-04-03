################################
# aggregate FABIO
################################

require(Matrix) # Necessary for forked processes

fabio_aggregate <- function(year){
  agg <- function(x)
  {
    x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x))
    return(x)
  }
  print(year)
  # Products to aggregate
  products <- c("Rice (Milled Equivalent)","Wheat and products","Barley and products","Maize and products","Rye and products","Oats","Millet and products","Sorghum and products","Cereals, Other","Potatoes and products","Cassava and products","Sweet potatoes","Roots, Other","Yams","Sugar cane","Sugar beet","Beans","Peas","Pulses, Other and products","Nuts and products","Soyabeans","Groundnuts (Shelled Eq)","Sunflower seed","Rape and Mustardseed","Seed cotton","Coconuts - Incl Copra","Sesame seed","Oil, palm fruit","Olives (including preserved)","Oilcrops, Other","Tomatoes and products","Onions","Vegetables, Other","Oranges, Mandarines","Lemons, Limes and products","Grapefruit and products","Citrus, Other","Bananas","Plantains","Apples and products","Pineapples and products","Dates","Grapes and products (excl wine)","Fruits, Other","Coffee and products","Cocoa Beans and products","Tea (including mate)","Hops","Pepper","Pimento","Cloves","Spices, Other","Jute","Jute-Like Fibres","Soft-Fibres, Other","Sisal","Abaca","Hard Fibres, Other","Tobacco","Rubber","Fodder crops","Grazing","Cottonseed","Palm kernels","Sugar non-centrifugal","Molasses","Sugar, Refined Equiv","Sweeteners, Other","Soyabean Oil","Groundnut Oil","Sunflowerseed Oil","Rape and Mustard Oil","Cottonseed Oil","Palmkernel Oil","Palm Oil","Coconut Oil","Sesameseed Oil","Olive Oil","Ricebran Oil","Maize Germ Oil","Oilcrops Oil, Other","Soyabean Cake","Groundnut Cake","Sunflowerseed Cake","Rape and Mustard Cake","Cottonseed Cake","Palmkernel Cake","Copra Cake","Sesameseed Cake","Oilseed Cakes, Other","Wine","Beer","Beverages, Fermented","Beverages, Alcoholic","Alcohol, Non-Food","Cotton lint","Cattle, Buffaloes","Cattle, Buffaloes","Sheep, Goats","Sheep, Goats","Pigs","Poultry Birds","Live animals, other","Live animals, other","Live animals, other","Live animals, other","Live animals, other","Live animals, other","Live animals, other","Live animals, other","Milk","Milk","Eggs","Wool (Clean Eq.)","Bovine Meat","Mutton & Goat Meat","Pigmeat","Poultry Meat","Meat, Other","Offals, Edible","Fats, Animals, Raw","Hides and skins","Meat Meal","Pet food","Honey","Silk","Fish","Wood fuel","Industrial roundwood, coniferous","Industrial roundwood, non-coniferous")
  # load data
  Z_m <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_Z_mass.rds"))
  Z_p <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_Z_price.rds"))
  X <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_X.rds"))
  Y <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_Y.rds"))
  E <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_E.rds"))
  
  colnames(Z_m) <- paste0(rep(1:192, each = 130), "_", rep(products,192))
  Z_m <- t(agg(Z_m))
  colnames(Z_m) <- paste0(rep(1:192, each = 130), "_", rep(products,192))
  Z_m <- t(agg(Z_m))
  
  colnames(Z_p) <- paste0(rep(1:192, each = 130), "_", rep(products,192))
  Z_p <- t(agg(Z_p))
  colnames(Z_p) <- paste0(rep(1:192, each = 130), "_", rep(products,192))
  Z_p <- t(agg(Z_p))
  
  X <- t(as.matrix(X))
  colnames(X) <- paste0(rep(1:192, each = 130), "_", rep(products,192))
  X <- as.vector(agg(X))
  
  Y <- t(Y)
  colnames(Y) <- paste0(rep(1:192, each = 130), "_", rep(products,192))
  Y <- t(agg(Y))
  
  E <- E[!E$Com.Code %in% c("c098","c100","c104","c105","c106","c107","c108","c109","c110","c112"),]
  
  saveRDS(Z_m, paste0("/mnt/nfs_fineprint/tmp/fabio/120/", year, "_Z_mass.rds"))
  saveRDS(Z_p, paste0("/mnt/nfs_fineprint/tmp/fabio/120/", year, "_Z_price.rds"))
  saveRDS(X, paste0("/mnt/nfs_fineprint/tmp/fabio/120/", year, "_X.rds"))
  saveRDS(Y, paste0("/mnt/nfs_fineprint/tmp/fabio/120/", year, "_Y.rds"))
  saveRDS(E, paste0("/mnt/nfs_fineprint/tmp/fabio/120/", year, "_E.rds"))
  
  return(0)
}

# Years to run
years <- 1986:2013

for(year in years){
  fabio_aggregate(year)
}

# library(parallel)
# # Calculate the number of cores
# no_cores <- detectCores() - 1
# # Initiate cluster
# cl <- makeCluster(no_cores)
# # start parallel
# parLapply(cl, years, fabio_aggregate)
# # stop cluster
# stopCluster(cl)


