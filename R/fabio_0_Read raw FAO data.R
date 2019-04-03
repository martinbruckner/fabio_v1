##########################################################################
# Read old FAO DATA and save as RDATA (downloaded around May 2016)
##########################################################################
# CBScrop <- read.csv(file="/mnt/nfs_fineprint/tmp/fabio/raw/old/CommodityBalances_Crops_E_All_Data_(Norm).csv")
# CBSlvst <- read.csv(file="/mnt/nfs_fineprint/tmp/fabio/raw/old/CommodityBalances_LivestockFish_E_All_Data_(Norm).csv")
# CBS_raw <- rbind(CBScrop, CBSlvst)
# save(CBS_raw, file="/mnt/nfs_fineprint/tmp/fabio/raw/old/CBS_raw.RData")
# BTD_raw <- read.csv(file="/mnt/nfs_fineprint/tmp/fabio/raw/old/Trade_DetailedTradeMatrix_E_All_Data_(Norm).csv")
# save(BTD_raw, file="/mnt/nfs_fineprint/tmp/fabio/raw/old/BTD_raw.RData")
# Forestry_raw <- read.csv(file="/mnt/nfs_fineprint/tmp/fabio/raw/old/Forestry_E_All_Data_(Norm).csv")
# save(Forestry_raw, file="/mnt/nfs_fineprint/tmp/fabio/raw/old/Forestry_raw.RData")
# ForTrade_raw <- read.csv(file="/mnt/nfs_fineprint/tmp/fabio/raw/old/Forestry_Trade_Flows_E_All_Data_(Norm).csv")
# save(ForTrade_raw, file="/mnt/nfs_fineprint/tmp/fabio/raw/old/ForTrade_raw.RData")
# Primary_raw <- read.csv(file="/mnt/nfs_fineprint/tmp/fabio/raw/old/Production_Crops_Primary.csv")
# save(Primary_raw, file="/mnt/nfs_fineprint/tmp/fabio/raw/old/Primary_raw.RData")
# Prod_raw <- read.csv(file="/mnt/nfs_fineprint/tmp/fabio/raw/old/Production_Crops_E_All_Data_(Norm).csv")
# save(Prod_raw, file="/mnt/nfs_fineprint/tmp/fabio/raw/old/Prod_raw.RData")
# Lvst_raw <- read.csv(file="/mnt/nfs_fineprint/tmp/fabio/raw/old/Production_Livestock_E_All_Data_(Norm).csv")
# save(Lvst_raw, file="/mnt/nfs_fineprint/tmp/fabio/raw/old/Lvst_raw.RData")
##########################################################################

##########################################################################
# Read new FAO DATA (downloaded on 27.07.2017)
# Bulk Downloads from
# http://www.fao.org/faostat/en/#data/BC
# http://www.fao.org/faostat/en/#data/BL
# http://www.fao.org/faostat/en/#data/TM
# http://www.fao.org/faostat/en/#data/FO
# http://www.fao.org/faostat/en/#data/FT
# http://www.fao.org/faostat/en/#data/QC
# http://www.fao.org/faostat/en/#data/QA
# http://www.fao.org/faostat/en/#data/QD
# http://www.fao.org/faostat/en/#data/QP
# http://www.fao.org/faostat/en/#data/QL
# plus all data for the aggregated item "Crops Primary > (List)" from http://www.fao.org/faostat/en/#data/QC
# downloaded before fodder crops were excluded from FAOSTAT (only covering 1986-2013)
# plus fishery statistics from
# http://www.fao.org/fishery/statistics/global-production/en
##########################################################################
CBScrop <- read.csv(file="/mnt/nfs_fineprint/tmp/fabio/raw/CommodityBalances_Crops_E_All_Data_(Normalized).csv")
CBSlvst <- read.csv(file="/mnt/nfs_fineprint/tmp/fabio/raw/CommodityBalances_LivestockFish_E_All_Data_(Normalized).csv")
CBS_raw <- rbind(CBScrop, CBSlvst)
save(CBS_raw, file="/mnt/nfs_fineprint/tmp/fabio/raw/CBS_raw.RData")
BTD_raw <- read.csv(file="/mnt/nfs_fineprint/tmp/fabio/raw/Trade_DetailedTradeMatrix_E_All_Data_(Norm).csv")
save(BTD_raw, file="/mnt/nfs_fineprint/tmp/fabio/raw/BTD_raw.RData")
Forestry_raw <- read.csv(file="/mnt/nfs_fineprint/tmp/fabio/raw/Forestry_E_All_Data_(Normalized).csv")
save(Forestry_raw, file="/mnt/nfs_fineprint/tmp/fabio/raw/Forestry_raw.RData")
ForTrade_raw <- read.csv(file="/mnt/nfs_fineprint/tmp/fabio/raw/Forestry_Trade_Flows_E_All_Data_(Normalized).csv")
save(ForTrade_raw, file="/mnt/nfs_fineprint/tmp/fabio/raw/ForTrade_raw.RData")
Primary_raw <- read.csv(file="/mnt/nfs_fineprint/tmp/fabio/raw/Production_Crops_Primary.csv")
save(Primary_raw, file="/mnt/nfs_fineprint/tmp/fabio/raw/Primary_raw.RData")
Prod_raw <- read.csv(file="/mnt/nfs_fineprint/tmp/fabio/raw/Production_Crops_E_All_Data_(Normalized).csv")
save(Prod_raw, file="/mnt/nfs_fineprint/tmp/fabio/raw/Prod_raw.RData")
Proc_lvst_raw <- read.csv(file="/mnt/nfs_fineprint/tmp/fabio/raw/Production_LivestockProcessed_E_All_Data_(Normalized).csv")
save(Proc_lvst_raw, file="/mnt/nfs_fineprint/tmp/fabio/raw/Proc_lvst_raw.RData")
Proc_crop_raw <- read.csv(file="/mnt/nfs_fineprint/tmp/fabio/raw/Production_CropsProcessed_E_All_Data_(Normalized).csv")
save(Proc_crop_raw, file="/mnt/nfs_fineprint/tmp/fabio/raw/Proc_crop_raw.RData")
Lvst_raw <- read.csv(file="/mnt/nfs_fineprint/tmp/fabio/raw/Production_Livestock_E_All_Data_(Normalized).csv")
save(Lvst_raw, file="/mnt/nfs_fineprint/tmp/fabio/raw/Lvst_raw.RData")
LvstPrimary_raw <- read.csv(file="/mnt/nfs_fineprint/tmp/fabio/raw/Production_LivestockPrimary_E_All_Data_(Normalized).csv")
save(LvstPrimary_raw, file="/mnt/nfs_fineprint/tmp/fabio/raw/LvstPrimary_raw.RData")
FishProd_raw <- read.csv(file="/mnt/nfs_fineprint/tmp/fabio/raw/FishProduction/TS_FI_PRODUCTION.csv")
save(FishProd_raw, file="/mnt/nfs_fineprint/tmp/fabio/raw/FishProd_raw.RData")
##########################################################################

##########################################################################
# Read ethanol data
##########################################################################
library(openxlsx)
# source: https://www.eia.gov/beta/international/data/browser/#/?pa=000001g&c=ruvvvvvfvtvnvv1urvvvvfvvvvvvfvvvou20evvvvvvvvvnvvuvo&ct=0&tl_id=79-A&vs=INTL.80-1-AFG-TBPD.A&vo=0&v=H&start=1980&end=2014
ProdEthanol_EIA <- read.xlsx(xlsxFile="/mnt/nfs_fineprint/tmp/fabio/raw/EIA_Biofuels_production.xlsx", rows = 5:235)
# source: http://dx.doi.org/10.1787/data-00550-en
ProdEthanol_IEA <- read.xlsx(xlsxFile = "/mnt/nfs_fineprint/tmp/fabio/raw/IEA_Biogasoline_production.xlsx", startRow = 6)
save(ProdEthanol_EIA, ProdEthanol_IEA, file="/mnt/nfs_fineprint/tmp/fabio/raw/ProdEthanol_raw.RData")

##########################################################################
# Read BACI92 trade data (available from 1995)
# source: http://www.cepii.fr/cepii/en/bdd_modele/download.asp?id=1
# Attention: restricted access
##########################################################################
BACI92 <- read.csv(file=paste0("/mnt/nfs_fineprint/tmp/fabio/raw/Baci92/baci92_",1995,".csv"))
for(year in 1996:2015){
  print(year)
  BACI92 <- rbind(BACI92, read.csv(file=paste0("/mnt/nfs_fineprint/tmp/fabio/raw/Baci92/baci92_",year,".csv")))
  gc()
}
save(BACI92, file="/mnt/nfs_fineprint/tmp/fabio/raw/BACI92_raw.RData")
# select Fish (30x) and Ethanol (2207)
BACI <- BACI92[substr(BACI92$hs6, 1, 3) %in% c("301","302","303","304") | substr(BACI92$hs6, 1, 4) == "2207", ]
save(BACI, file="/mnt/nfs_fineprint/tmp/fabio/raw/BACI_selected.RData")
##########################################################################

##########################################################################
# Read Comtrade trade data for years prior to 1995 (available from 1988)
# Read via API directly from web source
# Attention: restricted access
# set token to get unrestricted access:
token <- "NWXIFLPHElN6pA5nZhwLrpSbph7+cDZ9EsVP9tu62urda5zCBVRH6FU+0/BI/Rq4B6yuz3A71eB2GA3ep5d6VjynVCMotkdYO8wjCiDh47Fls/QflRXygZeb4fonhJ9OP2LZUMAJYo1zwnpiRz3FTbafW8JAIArhG3c0vb/8yxk="
# get token: login at https://comtrade.un.org/data/bulk -> reveice token at https://comtrade.un.org/db/u/uAccountInfo.aspx
##########################################################################
# install.packages("devtools")
# library(devtools)
# devtools::install_github("ChrisMuir/comtradr", force = TRUE)
library(comtradr)
ct_register_token(token)

# load data for HS codes "0301","0302","0303","0304" (Fish) and "2207" (Ethanol)
comtrade <- ct_search(reporters = "All", 
                     partners = "All", 
                     trade_direction = "All", 
                     start_date = "1986-01-01", 
                     end_date = "1990-12-31", 
                     commod_codes = c("0301","0302","0303","0304","2207"))
comtrade <- rbind(comtrade, ct_search(reporters = "All", 
                                    partners = "All", 
                                    trade_direction = "All", 
                                    start_date = "1991-01-01", 
                                    end_date = "1994-12-31", 
                                    commod_codes = c("0301","0302","0303","0304","2207")))
save(comtrade, file="/mnt/nfs_fineprint/tmp/fabio/raw/comtrade_raw.RData")

