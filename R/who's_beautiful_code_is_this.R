library(tidyverse)

rm(list = ls()); gc()

#----------------------------------------------------------------------------
#
# LOAD AND CONVERT DATA FILES (190 countries)
#
#----------------------------------------------------------------------------

mount_wu_share()

fabio <- "/mnt/nfs_fineprint/tmp/fabio/"

# load E files and subset landuse columns
E_list <-  list.files(path = fabio, pattern = "*_E.RData") %>% 
  map(~ mget(load(paste0(fabio,.x))))
E_landuse <- list()
for (i in seq_along(1:length(E_list))){
  E_landuse[[i]] <- E_list[[i]][["E"]][["Landuse"]]
}

# load X files
X_list <-  list.files(path = fabio, pattern = "*_X.RData") %>% 
  map(~ mget(load(paste0(fabio,.x))))
X_list2 <- list()
for (i in seq_along(1:length(X_list))){
  X_list2[[i]] <- X_list[[i]][["X"]]
}

# calculate U and replace NaN and Inf values with zero
U_list <- map2(E_landuse, X_list2, ~.x / .y) %>% 
  rapply(function(x) ifelse(!is.finite(x), 0, x), how = "list") %>% 
  map(~.x[-c(24701:24960)])

# save U 
save(U_list, file = "U_list.RData")

# load L files
L_list <- list.files(path = fabio, pattern = "*_L.RData")

# subset L for 3 years at a time
L_list <- L_list[5:7] %>% 
  map(~ mget(load(paste0(fabio,.x)))) %>%
  lapply(as.data.frame) %>% 
  map(~.x[1:24700, 1:24700]) %>% 
  lapply(as.matrix)

# save L
#save(L_list, file = "L_list.RData")

# load Y files and subset food columns
Y_list <-  list.files(path = fabio, pattern = "*_Y.RData") %>% 
  map(~ mget(load(paste0(fabio,.x)))) %>% 
  lapply(as.data.frame) %>% 
  lapply(dplyr::select, contains("Food")) %>% 
  lapply(as.matrix) %>% 
  map(~.x[-c(24701:24960), -c(191, 192)])

# save Y
save(Y_list, file = "Y_list.RData")
