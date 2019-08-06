##############################################################################################
##  
##  FABIO: BUILD MRIO TABLE BASED ON FAOSTAT COMMODITY BALANCE SHEETS AND TRADE DATA
##    1f_check estimated BTD
##  
##############################################################################################

library(reshape2)
library(data.table)

rm(list=ls()); gc()


year=2013

is.finite.data.frame <- function(x) do.call(cbind, lapply(x, is.finite))

# Read data
load(file = paste0("/mnt/nfs_fineprint/tmp/fabio/data/yearly/",year,"_BTD_original.RData"))
load(file = paste0("/mnt/nfs_fineprint/tmp/fabio/data/yearly/",year,"_BTD_balanced.RData"))

BTD$original <- BTD_original$Value[match(paste(BTD$From.Country,BTD$To.Country,BTD$Item),
                                         paste(BTD_original$From.Country,BTD_original$To.Country,BTD_original$Item))]
BTD$estimated <- BTD$Value
BTD$Value <- NULL
BTD$ID <- NULL

BTD[,8:9][!is.finite(BTD[,8:9])] <- 0

BTD$difference <- BTD$estimated - BTD$original
BTD$difference_abs <- abs(BTD$estimated - BTD$original)

BTD %>% 
  group_by(From.Country) %>% 
  summarise(difference = sum(difference),
            difference_abs = sum(difference_abs))
BTD %>% 
  group_by(To.Country) %>% 
  summarise(difference = sum(difference),
            difference_abs = sum(difference_abs))
BTD %>% 
  group_by(Item) %>% 
  summarise(difference = sum(difference),
            difference_abs = sum(difference_abs))



diff_from_country <- BTD %>% 
  group_by(From.Country) %>% 
  summarise(original = sum(original),
            estimated = sum(estimated)) %>% 
  mutate(difference = estimated - original) %>% 
  mutate(difference_percentage = difference / original * 100)
diff_to_country <- BTD %>% 
  group_by(To.Country) %>% 
  summarise(original = sum(original),
            estimated = sum(estimated)) %>% 
  mutate(difference = estimated - original) %>% 
  mutate(difference_percentage = difference / original * 100)
diff_item <- BTD %>% 
  group_by(Item) %>% 
  summarise(original = sum(original),
            estimated = sum(estimated)) %>% 
  mutate(difference = estimated - original) %>% 
  mutate(difference_percentage = difference / original * 100)

data.table::fwrite(diff_from_country, "./output/FABIO_paper_BTD_difference_exports.csv", sep = ";")
data.table::fwrite(diff_to_country, "./output/FABIO_paper_BTD_difference_imports.csv", sep = ";")
data.table::fwrite(diff_item, "./output/FABIO_paper_BTD_difference_items.csv", sep = ";")




##############################################################################################
rm(list=ls()); gc()

year=2013

is.finite.data.frame <- function(x) do.call(cbind, lapply(x, is.finite))

# Read data
load(file = paste0("/mnt/nfs_fineprint/tmp/fabio/data/yearly/",year,"_CBS.RData"))
load(file = paste0("/mnt/nfs_fineprint/tmp/fabio/data/yearly/",year,"_BTD.RData"))
regions <- read.csv(file="./inst/fabio_input/Regions.csv", header=TRUE, sep=";")
items <- read.csv(file="./inst/fabio_input/Items.csv", header=TRUE, sep=";")[,c(1,3,5)]

CBS <- CBS[CBS$Country %in% regions$Country & CBS$Item %in% items$Item,]
BTD <- BTD[BTD$Item %in% items$Item,]

# from_country <- BTD %>% 
#   group_by(From.Country) %>% 
#   summarise(tonnes = sum(tonnes),
#             tHead = sum(tHead))
# to_country <- BTD %>% 
#   group_by(To.Country) %>% 
#   summarise(tonnes = sum(tonnes),
#             tHead = sum(tHead))
item_sums <- BTD %>% 
  group_by(Item) %>% 
  summarise(trade = sum(tonnes + tHead) / 1000000)

items$trade <- item_sums$trade[match(items$Item,item_sums$Item)]

item_sums <- CBS %>% 
  group_by(Item) %>% 
  summarise(exports = sum(Exports) / 1000000,
            imports = sum(Imports) / 1000000)

items$exports <- item_sums$exports[match(items$Item,item_sums$Item)]
items$imports <- item_sums$imports[match(items$Item,item_sums$Item)]

data.table::fwrite(items, "./output/FABIO_paper_BTD_CBS_difference.csv", sep = ";", dec = ",")

