##############################################################################################
##  
##  FABIO: Calculate Footprints for FABIO paper
##  
##############################################################################################

library(Matrix)
library(tidyverse)

rm(list=ls()); gc()

is.finite.data.frame <- function(x) do.call(cbind, lapply(x, is.finite))
agg <- function(x) { x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x));  return(x) }

footprint <- function(region = character(), Y = matrix(), year=integer(), MP = matrix(), type=character()){
  FP <- Y[,1:2] * colSums(MP)
  FP[FP<0] <- 0     # filter negative values
  FP[rep(items$Item,nrreg)=="Cocoa Beans and products",][FP[rep(items$Item,nrreg)=="Cocoa Beans and products",] > 
                                                                mean(FP)*1000] <- 0   # filter outliers
  FP <- t(FP)
  colnames(FP) <- rep(c(rep("crop",96),rep("lvst",130-96)), nrreg)
  FP <- agg(FP)
  FP <- reshape2::melt(FP)
  FP$year <- year
  FP$region <- region
  FP$type <- type
  return(FP)
}


#-------------------------------------------------------------------------
# Make intitial settings
#-------------------------------------------------------------------------
# read region classification
regions <- read.csv(file="./inst/fabio_input/Regions.csv", header=TRUE, sep=";")
# read commodity classification
items <- read.csv(file="./inst/fabio_input/Items.csv", header=TRUE, sep=";")
nrreg <- nrow(regions)
nrcom <- nrow(items)

#-------------------------------------------------------------------------
# Start loop for a series of years
#-------------------------------------------------------------------------
# year=1986
year=2006
results <- data.frame()
imports <- data.frame()

for(year in 1986:2013){
  print(year)
  #-------------------------------------------------------------------------
  # Read data
  #-------------------------------------------------------------------------
  L_mass <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_L_mass.rds"))
  L_price <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_L_price.rds"))
  X <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_X.rds"))
  Y <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_Y.rds"))
  E <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_E.rds"))
  
  #-------------------------------------------------------------------------
  # Prepare Multipliers
  #-------------------------------------------------------------------------
  ext <- as.vector(E$Landuse) / X
  ext[!is.finite(ext)] <- 0
  ext[ext < 0] <- 0         # eliminate negative values
  L_mass[L_mass < 0] <- 0   # eliminate negative values
  L_price[L_price < 0] <- 0 # eliminate negative values
  MP_mass <- ext * L_mass   # is identical with L * ext
  MP_price <- ext * L_price
  
  #-------------------------------------------------------------------------
  # Calculate Footprints
  #-------------------------------------------------------------------------
  # Prepare final demand
  Y_codes <- data.frame(ISO = substr(colnames(Y),1,3))
  Y_codes$Continent = regions$Continent[match(Y_codes$ISO,regions$ISO)]
  Y_codes$FD <- substr(colnames(Y),5,100)
  
  Y_US <- Y[,Y_codes$ISO == "USA"]
  Y_CN <- Y[,Y_codes$ISO == "CHN"]
  
  Y_EU <- Y[,Y_codes$Continent == "EU"]
  colnames(Y_EU) <- Y_codes$FD[Y_codes$Continent == "EU"]
  Y_EU <- agg(Y_EU)
  colnames(Y_US) <- colnames(Y_CN) <- colnames(Y_EU)
  
  # Footprint
  results <- rbind(results, footprint(region = "EU", Y = Y_EU, year = year, MP = MP_mass, "mass"))
  results <- rbind(results, footprint(region = "EU", Y = Y_EU, year = year, MP = MP_price, "price"))
  
  results <- rbind(results, footprint(region = "US", Y = Y_US, year = year, MP = MP_mass, "mass"))
  results <- rbind(results, footprint(region = "US", Y = Y_US, year = year, MP = MP_price, "price"))
  
  results <- rbind(results, footprint(region = "CN", Y = Y_CN, year = year, MP = MP_mass, "mass"))
  results <- rbind(results, footprint(region = "CN", Y = Y_CN, year = year, MP = MP_price, "price"))
  
  
  imports <- rbind(imports, footprint(region = "EU", Y = Y_EU, year = year, MP = MP_mass[rep(regions$Continent, each = nrow(items)) != "EU", ], "mass"))
  imports <- rbind(imports, footprint(region = "EU", Y = Y_EU, year = year, MP = MP_price[rep(regions$Continent, each = nrow(items)) != "EU", ], "price"))
  
  imports <- rbind(imports, footprint(region = "US", Y = Y_US, year = year, MP = MP_mass[rep(regions$ISO, each = nrow(items)) != "USA", ], "mass"))
  imports <- rbind(imports, footprint(region = "US", Y = Y_US, year = year, MP = MP_price[rep(regions$ISO, each = nrow(items)) != "USA", ], "price"))
  
  imports <- rbind(imports, footprint(region = "CN", Y = Y_CN, year = year, MP = MP_mass[rep(regions$ISO, each = nrow(items)) != "CHN", ], "mass"))
  imports <- rbind(imports, footprint(region = "CN", Y = Y_CN, year = year, MP = MP_price[rep(regions$ISO, each = nrow(items)) != "CHN", ], "price"))
  
  
}

data.table::fwrite(results, file="./output/FABIO_paper_results.csv", sep=";")
data.table::fwrite(imports, file="./output/FABIO_paper_imports.csv", sep=";")


# #-------------------------------------------------------------------------
# # Check results
# #-------------------------------------------------------------------------
# year=2005
# for(year in c(1991:1993,2004:2006)){
#   
#   print(year)
#   #-------------------------------------------------------------------------
#   # Read data
#   #-------------------------------------------------------------------------
#   L_mass <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_L_mass.rds"))
#   L_price <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_L_price.rds"))
#   X <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_X.rds"))
#   Y <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_Y.rds"))
#   E <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_E.rds"))
#   nrreg <- nrow(regions)
#   nrcom <- nrow(Y) / nrreg
#   
#   #-------------------------------------------------------------------------
#   # Prepare Multipliers
#   #-------------------------------------------------------------------------
#   ext <- as.vector(E$Landuse) / X
#   ext[!is.finite(ext)] <- 0
#   MP_mass <- ext * L_mass   # is identical with L * ext
#   MP_price <- ext * L_price
#   
#   #-------------------------------------------------------------------------
#   # Calculate Footprints
#   #-------------------------------------------------------------------------
#   # Prepare final demand
#   Y_codes <- data.frame(ISO = substr(colnames(Y),1,3))
#   Y_codes$Continent = regions$Continent[match(Y_codes$ISO,regions$ISO)]
#   Y_codes$FD <- substr(colnames(Y),5,100)
#   
#   Y_US <- Y[,Y_codes$ISO == "USA"]
#   Y_CN <- Y[,Y_codes$ISO == "CHN"]
#   
#   Y_EU <- Y[,Y_codes$Continent == "EU"]
#   colnames(Y_EU) <- Y_codes$FD[Y_codes$Continent == "EU"]
#   Y_EU <- agg(Y_EU)
#   colnames(Y_US) <- colnames(Y_CN) <- colnames(Y_EU)
#   
#   FP <- Y_EU * colSums(MP_mass)
#   fwrite(FP, paste0("./output/results_EU_",year,".csv"), sep = ";")
#   FP <- Y_US * colSums(MP_mass)
#   fwrite(FP, paste0("./output/results_US_",year,".csv"), sep = ";")
#   FP <- Y_CN * colSums(MP_mass)
#   fwrite(FP, paste0("./output/results_CN_",year,".csv"), sep = ";")
# }


#-------------------------------------------------------------------------
# Plot results
#-------------------------------------------------------------------------
results <- data.table::fread(file="./output/FABIO_paper_results.csv", sep=";")
results <- as_tibble(results)
results$region[results$region=="CN"] <- "China"
results$region[results$region=="EU"] <- "EU-28"
results$region[results$region=="US"] <- "USA"

diff_results <- results %>% 
  dplyr::filter(Var1 %in% c("Food", "OtherUses")) %>%
  dplyr::mutate(var = paste(Var1, Var2, sep = "-")) %>% 
  dplyr::mutate(var = factor(var, c("OtherUses-lvst", "OtherUses-crop", "Food-lvst", "Food-crop"), 
                             c("Other uses - livestock", "Other uses - crops", "Food - livestock", "Food - crops"))) %>% 
  tidyr::spread(key = type, value = value) %>% 
  dplyr::mutate(diff = price - mass) %>% 
  dplyr::select(-Var1, -Var2, -price) %>% 
  tidyr::gather(key = type, value = value, -year, -region, -var)
# dplyr::mutate(region = paste(region, type, sep = "-")) 
# dplyr::mutate(region = factor(region, c("CN-mass","EU-mass","US-mass","CN-diff","EU-diff","US-diff"),
#                               c("CN-mass","EU-mass","US-mass","CN-diff","EU-diff","US-diff")))


p <- diff_results %>%
  dplyr::filter(type == "mass") %>% 
  ggplot() +
  facet_wrap(~region, ncol = 3) + 
  theme(panel.background = element_rect(fill = 'white', linetype = "solid", colour = 'grey'), 
        panel.grid.major = element_line(colour = "lightgrey", size = 0.1, linetype = "solid"), 
        panel.grid.minor = element_line(colour = "lightgrey", size = 0.1, linetype = "solid"), 
        legend.position = "none") +
  geom_area(mapping = aes(x = year, y = value, group = var, fill = var)) + 
  xlab("") + 
  ylab("Cropland area (hectares)") + 
  viridis::scale_fill_viridis(discrete = TRUE)

ggplot2::ggsave(paste0("results_a.png"), plot = p, device = "png", path = "./output/",
                scale = 1, width = 200, height = 80, units = "mm", dpi = 300)

p <- diff_results %>% 
  dplyr::filter(type == "diff") %>% 
  ggplot() +
  facet_wrap(~region, ncol = 3) +
  theme(panel.background = element_rect(fill = 'white', linetype = "solid", colour = 'grey'), 
        panel.grid.major = element_line(colour = "lightgrey", size = 0.1, linetype = "solid"), 
        panel.grid.minor = element_line(colour = "lightgrey", size = 0.1, linetype = "solid"), 
        legend.position = "bottom", legend.title = element_blank()) +
  geom_area(mapping = aes(x = year, y = value, group = var, fill = var)) + 
  xlab("") + 
  ylab("Cropland area (hectares)") + 
  viridis::scale_fill_viridis(discrete = TRUE)

ggplot2::ggsave(paste0("results_b.png"), plot = p, device = "png", path = "./output/",
                scale = 1, width = 200, height = 80, units = "mm", dpi = 300)

# diff_results %>% 
#   dplyr::filter(region == "CN") %>% 
#   ggplot() +
#   facet_wrap(~type, ncol = 2, scales = "free") +
#   geom_area(mapping = aes(x = year, y = value, group = var, fill = var)) + 
#   xlab("") + 
#   ylab("Cropland area in hectares") + 
#   viridis::scale_fill_viridis(discrete = TRUE) + 
#   theme(legend.position = "bottom", legend.title = element_blank())


imports <- data.table::fread(file="./output/FABIO_paper_imports.csv", sep=";")
imports <- as_tibble(imports)
imports$region[imports$region=="CN"] <- "China"
imports$region[imports$region=="EU"] <- "EU-28"
imports$region[imports$region=="US"] <- "USA"

results <- data.table::fread(file="./output/FABIO_paper_results.csv", sep=";")
results <- as_tibble(results) 
results$region[results$region=="CN"] <- "China"
results$region[results$region=="EU"] <- "EU-28"
results$region[results$region=="US"] <- "USA"

results <- results %>% 
  dplyr::filter(Var1 %in% c("Food", "OtherUses")) %>%
  dplyr::mutate(var = paste(Var1, Var2, sep = "-")) %>% 
  dplyr::mutate(var = factor(var, c("OtherUses-lvst", "OtherUses-crop", "Food-lvst", "Food-crop"), 
                             c("Other uses - livestock", "Other uses - crops", "Food - livestock", "Food - crops"))) %>% 
  tidyr::spread(key = type, value = value) %>% 
  dplyr::mutate(diff = price - mass) %>% 
  dplyr::select(-Var1, -Var2, -price) %>% 
  tidyr::gather(key = type, value = value, -year, -region, -var) 

imports <- imports %>% 
  dplyr::filter(Var1 %in% c("Food", "OtherUses")) %>%
  dplyr::mutate(var = paste(Var1, Var2, sep = "-")) %>% 
  dplyr::mutate(var = factor(var, c("OtherUses-lvst", "OtherUses-crop", "Food-lvst", "Food-crop"), 
                             c("Other uses - livestock", "Other uses - crops", "Food - livestock", "Food - crops"))) %>% 
  tidyr::spread(key = type, value = value) %>% 
  dplyr::mutate(diff = price - mass) %>% 
  dplyr::select(-Var1, -Var2, -price) %>% 
  tidyr::gather(key = type, value = value, -year, -region, -var) 

p <- imports %>% 
  dplyr::mutate(value = value / results$value * 100) %>% 
  dplyr::filter(type == "mass") %>% 
  ggplot(mapping = aes(x = year, y = value, group = var, color = var)) +
  facet_wrap(~region, ncol = 3) + 
  theme(panel.background = element_rect(fill = 'white', linetype = "solid", colour = 'grey'), 
        panel.grid.major = element_line(colour = "lightgrey", size = 0.1, linetype = "solid"), 
        panel.grid.minor = element_line(colour = "lightgrey", size = 0.1, linetype = "solid"), 
        legend.position = "bottom", legend.title = element_blank()) +
  geom_line() + 
  geom_point(size = 1) + 
  xlab("") + 
  ylab("Import share (percentages)") + 
  viridis::scale_color_viridis(discrete = TRUE)

ggplot2::ggsave(paste0("results_c.png"), plot = p, device = "png", path = "./output/",
                scale = 1, width = 200, height = 80, units = "mm", dpi = 300)


#########################################################################
# Calculate China net-trade in 2004
#########################################################################
rm(list=ls()); gc()

agg <- function(x) { x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x)); return(x) }

#-------------------------------------------------------------------------
# Make intitial settings
#-------------------------------------------------------------------------
year <- 2004
# read region classification
regions <- read.csv(file="./inst/fabio_input/Regions.csv", header=TRUE, sep=";")
# read commodity classification
items <- read.csv(file="./inst/fabio_input/Items.csv", header=TRUE, sep=";")
nrreg <- nrow(regions)
nrcom <- nrow(items)

# net trade of China
region = (1:nrreg)[regions$ISO=="CHN"]
dom <- (nrcom*(region-1)+1):(nrcom*region)

L_mass <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_L_mass.rds"))
L_price <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_L_price.rds"))
X <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_X.rds"))
Y <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_Y.rds"))
E <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_E.rds"))

#-------------------------------------------------------------------------
# Prepare Multipliers
#-------------------------------------------------------------------------
ext <- as.vector(E$Landuse) / X
ext[!is.finite(ext)] <- 0
ext[ext < 0] <- 0         # eliminate negative values
L_mass[L_mass < 0] <- 0   # eliminate negative values
L_price[L_price < 0] <- 0 # eliminate negative values
MP_mass <- ext * L_mass   # is identical with L * ext
MP_price <- ext * L_price

#-------------------------------------------------------------------------
# Calculate Footprints
#-------------------------------------------------------------------------
# prepare final demand data
Y[substr(colnames(Y),5,8)=="Food"][Y[substr(colnames(Y),5,8)=="Food"]<0] <- 0
Y[substr(colnames(Y),5,12)=="OtherUse"][Y[substr(colnames(Y),5,12)=="OtherUse"]<0] <- 0
colnames(Y) <- substr(colnames(Y),1,3)
Y <- agg(Y)

# Calculate import footprint
get_Xm <- function(){
  Ym <- Y[,region]
  Z <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_Z_price.rds"))
  Zm <- rowSums(Z[,dom])
  Ym[dom] <- Zm[dom] <- 0
  Xm <- Zm + Ym
  return(Xm)
}

# Calculate export footprint
get_Xe <- function(){
  Ye <- Y[dom,]
  Ye <- rowSums(Ye[,-region])
  Z <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_Z_price.rds"))
  Ze <- rowSums(Z[dom,-dom])
  Xe <- Ze + Ye
  return(Xe)
}

Xm <- get_Xm()
FP_imp <- sum(t(MP_price) * Xm)
Xe <- get_Xe()
FP_exp <- sum(t(MP_price[,dom]) * Xe)

# Calculate net trade
FP_nettrade_price <- FP_imp - FP_exp


# Calculate import footprint
get_Xm <- function(){
  Ym <- Y[,region]
  Z <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_Z_mass.rds"))
  Zm <- rowSums(Z[,dom])
  Ym[dom] <- Zm[dom] <- 0
  Xm <- Zm + Ym
  return(Xm)
}

# Calculate export footprint
get_Xe <- function(){
  Ye <- Y[dom,]
  Ye <- rowSums(Ye[,-region])
  Z <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_Z_mass.rds"))
  Ze <- rowSums(Z[dom,-dom])
  Xe <- Ze + Ye
  return(Xe)
}

Xm <- get_Xm()
FP_imp <- sum(t(MP_mass) * Xm)
Xe <- get_Xe()
FP_exp <- sum(t(MP_mass[,dom]) * Xe)

# Calculate net trade
FP_nettrade_mass <- FP_imp - FP_exp


#########################################################################
# Plot comparison for China net-trade in 2004
#########################################################################
rm(list=ls()); gc()
library(tidyverse)

references <- c("FABIO",
                "Qiang et al. \n(2013)", 
                "Kastner et al. \n(2014b)",
                "Meyfroidt \net al. (2010)",
                "Weinzettel \net al. (2013)",
                "Hubacek and \nFeng (2016)",
                "Yu et al. \n(2013)")

data <- tibble(reference = factor(references, references, references),
               value = c(20910431,20000000,19000000,16000000,-8000000,-9000000,-17000000)) %>%
  dplyr::mutate(colour = factor(c("physical","physical","physical","physical","mixed","monetary","monetary"))) %>% 
  dplyr::mutate(colour = factor(colour, c("physical", "mixed", "monetary"), c("physical", "mixed", "monetary")))

p <- data %>% 
  ggplot(aes(x=reference, y=value)) +
  geom_bar(stat="identity", aes(fill=colour))+
  viridis::scale_fill_viridis(discrete = TRUE) +
  # theme_minimal() +
  theme(panel.background = element_rect(fill = 'white', linetype = "solid", colour = 'grey'), 
        panel.grid.major = element_line(colour = "lightgrey", size = 0.1, linetype = "solid"), 
        panel.grid.minor = element_line(colour = "lightgrey", size = 0.1, linetype = "solid")) +
  xlab("") + 
  ylab("Cropland area (hectares)") + 
  labs(fill = "Accounting unit")

ggplot2::ggsave(paste0("china_nettrade.png"), plot = p, device = "png", path = "./output/",
                scale = 1, width = 200, height = 100, units = "mm", dpi = 300)


