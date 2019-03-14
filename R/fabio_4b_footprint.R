#' @title FABIO footprint
#' @author Martin Bruckner, \email{martin.bruckner@@wu.ac.at}
#' 
#' @description Footprint analysis based on FABIO model. 
#' 
#' @param ... some argument 
#' 
#' @return returns some data
#' 
#' @export
fabio_footprint <- function(wd, years, regions_select = "all"){
  
  
  #------------------------------------------------
  rm(list=ls()); gc()
  
  library(dplyr)
  library(data.table)
  library(reshape2)
  
  # wd <- "W:/WU/Projekte/SRU-Projekte/01_Projekte/1305_Bonn BioMRIO/02_FABIO/01_code & data"
  year <- years <- 2013
  regions_select <- "all"
  #------------------------------------------------
  
  # current_wd <- getwd()
  # setwd(wd)
  
  footprint <- function(region, fd, select_items = "all", file_name){
    if(length(select_items) == 1) {
      if(select_items == "all") select_items <- rep(TRUE,nrow(Y))
    }
    
    Yreg <- Y[,(4*region-4+fd)]
    
    if(sum(Yreg)==0) return(0)
    
    # Calculate Footprint (MP * FD)
    FP <- as.data.table((t(MP) * Yreg[select_items])) %>%
      mutate(ID = rep(items$Com.Code, nrreg)[select_items])
    FP <- as.data.frame(as.data.table(FP)[, lapply(.SD, sum), by = .(ID)])
    FP$ID <- NULL
    FP <- t(FP)
    
    # Rearrange results as a list
    f <- matrix(0,length(X),ncol(FP))
    f[X!=0,] <- as.matrix(FP)
    FP <- as.data.frame(f)
    
    colnames(FP) <- as.character(items$Com.Code)
    FP$From.Country.Code <- rep(regions$Country.Code, each = nrow(items))
    FP$From.Country <- rep(regions$Country, each = nrow(items))
    FP$From.ISO <- rep(regions$ISO, each = nrow(items))
    FP <- reshape2::melt(FP, id.vars = c("From.Country.Code","From.Country","From.ISO"), variable.name = "Com.Code")
    FP <- FP[FP$value!=0,]
    FP$Item <- items$Item[match(FP$Com.Code,items$Com.Code)]
    FP$fd <- fd_categories[fd]
    FP$Country_Nr <- region
    regions$Nr <- 1:nrow(regions)
    FP$Country <- as.character(regions$Country)[match(FP$Country_Nr,regions$Nr)]
    FP$ISO <- as.character(regions$ISO)[match(FP$Country_Nr,regions$Nr)]
    FP$Country_Nr <- NULL
    regions$Nr <- NULL
    
    data.table::fwrite(FP, file=paste0("./output/results/",year,"_",file_name,".csv"), append = TRUE)
    
    return(0)
  }
  
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
  
  ##########################################################################
  # Make intitial settings
  ##########################################################################
  # read region classification
  regions <- utils::read.csv(file="./input/Regions.csv", header=TRUE, sep=";")
  # read commodity classification
  items <- utils::read.csv(file="./input/Items.csv", header=TRUE, sep=";")
  
  ##########################################################################
  # Start loop for a series of years
  ##########################################################################
  # year <- 2013
  for(year in years){
    print(year)
    
    #-------------------------------------------------------------------------
    # Read data
    #-------------------------------------------------------------------------
    load(file=paste0("./output/yearly/",year,"_L.RData"))
    load(file=paste0("./output/yearly/",year,"_X.RData"))
    load(file=paste0("./output/yearly/",year,"_Y.RData"))
    load(file=paste0("./output/yearly/",year,"_E.RData"))
    nrreg <- nrow(regions)
    nrcom <- nrow(Y) / nrreg
    
    
    #-------------------------------------------------------------------------
    # Prepare Multipliers
    #-------------------------------------------------------------------------
    MP <- as.vector(E$Landuse) / X
    MP[!is.finite(MP)] <- 0
    MP <- MP * L   # is identical with L * MP
    MP <- reduce_matrix(MP,X)
    rm(L); gc()
    
    
    #-------------------------------------------------------------------------
    # Calculate Footprints
    #-------------------------------------------------------------------------
    # regions_selected <- utils::read.csv(file="./input/Regions_Footprint.csv", header=TRUE, sep=";")
    regions_selected <- regions
    
    if (regions_select != "all"){
      regions_selected <- regions_selected[regions_selected$Continent %in% regions_select,]
    }
    
    file_name <- "FP_selected"
    headings <- data.table(From.Country.Code = integer(),
                           From.Country = character(),
                           From.ISO = character(),
                           Com.Code = character(),
                           value = numeric(),
                           Commodity = character(),
                           fd = character(),
                           Country = character(),
                           ISO = character())
    data.table::fwrite(headings, file=paste0("./output/results/",year,"_",file_name,".csv"))
    fd_categories <- c("1_Food","2_OtherUses","3_StockVariation","4_Balancing")
    select_items <- X!=0
    
    region=56; fd=2
    for(region in 1:nrow(regions_selected)){
      print(paste0("region ",region))
      
      for(fd in 1:4){
        footprint(region = region, fd = fd, select_items = select_items, file_name = file_name)
      }
    }
    
    FP <- data.table::fread(file = paste0("./output/results/",year,"_",file_name,".csv"))
    FP <- reshape2::dcast(FP, ... ~ fd, value.var = "value", fun.aggregate = sum)
    data.table::fwrite(FP, file=paste0("./output/results/",year,"_",file_name,".csv"))
  }
  
  # setwd(current_wd)
  
}

