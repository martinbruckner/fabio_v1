##########################################################################
##  Cattle check
##########################################################################

rm(list=ls())
library(tidyverse)
library(data.table)

mount_wu_share()

is.finite.data.frame <- function(x) do.call(cbind, lapply(x, is.finite))
# read region classification
regions <- read.csv(file="./inst/fabio_input/Regions.csv", header=TRUE, sep=";")
# read commodity classification
items <- read.csv(file="./inst/fabio_input/Items.csv", header=TRUE, sep=";")

