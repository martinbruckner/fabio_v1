##############################################################################################
##  
##  FABIO: BUILD MRIO TABLE BASED ON FAOSTAT COMMODITY BALANCE SHEETS AND TRADE DATA
##    1c_check Assessing the amount of estimated CBS
##  
##############################################################################################

# run code in fabio_1c_Estimate missing CBS.R until line 266

estimated <- c("Seed cotton", "Oil, palm fruit", "Hops", "Fodder crops", "Grazing", "Cattle", "Buffaloes",
  "Sheep", "Goats", "Pigs", "Poultry Birds", "Horses", "Asses", "Mules", "Camels", "Camelids, other", 
  "Rabbits and hares", "Rodents, other", "Live animals, other", "Fish, Seafood", "Pet food", "Alcohol, Non-Food",
  "Wood fuel", "Industrial roundwood, non-coniferous", "Industrial roundwood, coniferous")

regions <- read.csv2("./inst/fabio_input/Regions.csv")
CBS_new <- addCBS_all

per_country_new <- CBS_new %>%
  filter(Country %in% regions$Country) %>% 
  group_by(Country) %>%
  summarise(estimated=n())

per_country <- CBS %>% 
  filter(Country %in% regions$Country) %>% 
  filter(!(Item %in% estimated)) %>% 
  group_by(Country) %>% 
  summarise(reported=n())

per_country_new$reported <- per_country$reported[match(per_country_new$Country, per_country$Country)]
per_country_new$reported[is.na(per_country_new$reported)] <- 0
per_country_new$sum <- per_country_new$reported + per_country_new$estimated
per_country_new$Continent <- regions$Continent[match(per_country_new$Country,regions$Country)]

data.table::fwrite(per_country_new, "./output/FABIO_paper_CBS_estimated_country.csv", sep = ";")



# per_item_new <- CBS_new %>%
#   filter(Country %in% regions$Country) %>%
#   group_by(Item) %>%
#   summarise(estimated=n())

per_item <- CBS %>% 
  filter(Country %in% regions$Country) %>%
  group_by(Item) %>% 
  summarise(reported=n())

per_item$reported[per_item$Item %in% estimated] <- 0
per_item$estimated <- 191 - per_item$reported
per_item$Group <- items$Group[match(per_item$Item, items$Item)]

# per_item$estimated <- per_item_new$estimated[match(per_item$Item,per_item_new$Item)]
# per_item$sum <- per_item$reported + per_item$estimated

data.table::fwrite(per_item, "./output/FABIO_paper_CBS_estimated_item.csv", sep = ";")

