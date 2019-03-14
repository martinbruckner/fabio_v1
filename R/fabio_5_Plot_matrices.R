library(ggplot2)
library(data.table)

#################################################################
# Plot heatmap of Z (country x country)
#################################################################
rm(list = ls()); gc()
agg <- function(x)
{
  x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x))
  return(x)
}

load("W:/WU/Projekte/GRU/01_Projekte/1305_Bonn BioMRIO/02_FABIO/01_code & data/data/yearly/2013_Z.RData")

colnames(Z) <- sprintf("%03d", rep(1:192,each=130))
Z <- agg(Z)
Z <- t(Z)
colnames(Z) <- sprintf("%03d", rep(1:192,each=130))
Z <- agg(Z)
Z <- t(Z)
gc()

Z <- data.table(Z)

# Add rownames to the data frame as a column
Z <- cbind(data.table(rnames=sprintf("%03d", 1:192)),Z)

# melt data for ggplot to draw graphs
Z <- data.table::melt(Z)

Z$value[Z$value<1] <- 0

Z$rnames <- factor(Z$rnames, labels = 192:1, levels = Z$rnames[192:1])
levels(Z$variable) <- 1:192

p <- ggplot(Z, aes(variable, rnames)) + geom_tile(aes(fill = ifelse(is.infinite(log(value)), 0, log(value))), colour = "white") + 
  viridis::scale_fill_viridis(direction = -1) + xlab("") + ylab("") + labs(fill="log(Z)") +
  theme(text = element_text(size=8)) + scale_x_discrete(breaks = c(1,(1:19)*10), position = "top") + 
  scale_y_discrete(breaks = c(1,(1:19)*10))

ggsave(filename = "./results/heatmap_z.png", plot = p, dpi = 640)


#################################################################
# Plot heatmap of Z (product x product)
#################################################################
rm(list = ls()); gc()
agg <- function(x)
{
  x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x))
  return(x)
}

load("W:/WU/Projekte/GRU/01_Projekte/1305_Bonn BioMRIO/02_FABIO/01_code & data/data/yearly/2013_Z.RData")
regions <- read.csv2(file = "Regions.csv")
items <- read.csv2(file = "Items.csv")

colnames(Z) <- sprintf("%03d", rep(1:130,192))
Z <- agg(Z)
Z <- t(Z)
colnames(Z) <- sprintf("%03d", rep(1:130,192))
Z <- agg(Z)
Z <- t(Z)
gc()

Z <- data.table(Z)

# Add rownames to the data frame as a column
Z <- cbind(data.table(rnames=sprintf("%03d", 1:130)),Z)

# melt data for ggplot to draw graphs
Z <- data.table::melt(Z)

Z$value[Z$value<1] <- 0

Z$rnames <- factor(Z$rnames, labels = 130:1, levels = Z$rnames[130:1])
levels(Z$variable) <- 1:130

p <- ggplot(Z, aes(variable, rnames)) + geom_tile(aes(fill = ifelse(is.infinite(log(value)), 0, log(value))), colour = "white") + 
  viridis::scale_fill_viridis(direction = -1) + xlab("") + ylab("") + labs(fill="log(Z)") +
  theme(text = element_text(size=8)) + scale_x_discrete(breaks = c(1,(1:13)*10), position = "top") + 
  scale_y_discrete(breaks = c(1,(1:13)*10))

ggsave(filename = "./results/heatmap_z_pxp.png", plot = p, dpi = 640)


#################################################################
# Plot heatmap of Y (country x country)
#################################################################
rm(list = ls()); gc()
agg <- function(x)
{
  x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x))
  return(x)
}

load("W:/WU/Projekte/GRU/01_Projekte/1305_Bonn BioMRIO/02_FABIO/01_code & data/data/yearly/2013_Y.RData")

colnames(Y) <- rep(1:192,each=4)
Y <- agg(Y)
Y <- t(Y)
colnames(Y) <- sprintf("%03d", rep(1:192,each=130))
Y <- agg(Y)
Y <- t(Y)
gc()

Y <- data.table(Y)

# Add rownames to the data frame as a column
Y <- cbind(data.table(rnames=sprintf("%03d", 1:192)),Y)

# melt data for ggplot to draw graphs
Y <- data.table::melt(Y)

Y$value[Y$value<1] <- 0

Y$rnames <- factor(Y$rnames, labels = 192:1, levels = Y$rnames[192:1])
levels(Y$variable) <- 1:192

p <- ggplot(Y, aes(variable, rnames)) + geom_tile(aes(fill = ifelse(is.infinite(log(value)), 0, log(value))), colour = "white") + 
  viridis::scale_fill_viridis(direction = -1) + xlab("") + ylab("") + labs(fill="log(Y)") +
  theme(text = element_text(size=8)) + scale_x_discrete(breaks = c(1,(1:19)*10), position = "top") + 
  scale_y_discrete(breaks = c(1,(1:19)*10))

ggsave(filename = "./results/heatmap_y.png", plot = p, dpi = 640)

#################################################################
# Plot heatmap of Z for Austria (product x product)
#################################################################
rm(list = ls()); gc()
agg <- function(x)
{
  x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x))
  return(x)
}

load("W:/WU/Projekte/GRU/01_Projekte/1305_Bonn BioMRIO/02_FABIO/01_code & data/data/yearly/2013_Z.RData")
load("W:/WU/Projekte/GRU/01_Projekte/1305_Bonn BioMRIO/02_FABIO/01_code & data/data/yearly/2013_Y.RData")
regions <- read.csv2(file = "Regions.csv")
items <- read.csv2(file = "Items.csv")

# Austria is No 9 in the regions list
reg <- 9
Zdom <- cbind(Z[(130*(reg-1)+1):(130*reg),(130*(reg-1)+1):(130*reg)], Y[(130*(reg-1)+1):(130*reg),(4*(reg-1)+1):(4*reg)])
Zimp <- cbind(Z[-((130*(reg-1)+1):(130*reg)),(130*(reg-1)+1):(130*reg)], Y[-((130*(reg-1)+1):(130*reg)),(4*(reg-1)+1):(4*reg)])
Zexp <- cbind(Z[(130*(reg-1)+1):(130*reg),-((130*(reg-1)+1):(130*reg))], Y[(130*(reg-1)+1):(130*reg),-((4*(reg-1)+1):(4*reg))])

colnames(Zexp) <- c(sprintf("%03d", rep(1:130,191)),paste0("f_", rep(1:4,191)))
Zexp <- agg(Zexp)

Zimp <- t(Zimp)
colnames(Zimp) <- sprintf("%03d", rep(1:130,191))
Zimp <- agg(Zimp)
Zimp <- t(Zimp)
gc()

Zdom <- data.table(Zdom)
Zimp <- data.table(Zimp)
Zexp <- data.table(Zexp)

# Add rownames to the data frame as a column
Zdom <- cbind(data.table(rnames=sprintf("%03d", 1:130)),Zdom)
Zimp <- cbind(data.table(rnames=sprintf("%03d", 1:130)),Zimp)
Zexp <- cbind(data.table(rnames=sprintf("%03d", 1:130)),Zexp)

# melt data for ggplot to draw graphs
Zdom <- data.table::melt(Zdom)
Zimp <- data.table::melt(Zimp)
Zexp <- data.table::melt(Zexp)

Zdom$value[Zdom$value<1] <- 0
Zimp$value[Zimp$value<1] <- 0
Zexp$value[Zexp$value<1] <- 0

Zdom$rnames <- factor(Zdom$rnames, labels = 130:1, levels = Zdom$rnames[130:1])
levels(Zdom$variable) <- 1:134
Zdom$log <- log(Zdom$value)
Zdom$log[!is.finite(Zdom$log)] <- 0
Zimp$rnames <- factor(Zimp$rnames, labels = 130:1, levels = Zimp$rnames[130:1])
levels(Zimp$variable) <- 1:134
Zimp$log <- log(Zimp$value)
Zimp$log[!is.finite(Zimp$log)] <- 0
Zexp$rnames <- factor(Zexp$rnames, labels = 130:1, levels = Zexp$rnames[130:1])
levels(Zexp$variable) <- 1:134
Zexp$log <- log(Zexp$value)
Zexp$log[!is.finite(Zexp$log)] <- 0


p <- ggplot(Zdom, aes(variable, rnames)) + geom_tile(aes(fill = ifelse(is.infinite(log(value)), 0, log(value))), colour = "white") + 
  viridis::scale_fill_viridis(direction = -1) + xlab("") + ylab("") + labs(fill="log(Z_dom)") +
  theme(text = element_text(size=8)) + scale_x_discrete(breaks = c(1,(1:13)*10), position = "top") + 
  scale_y_discrete(breaks = c(1,(1:13)*10))

ggsave(filename = "./results/heatmap_z_dom.png", plot = p, dpi = 640)

p <- ggplot(Zimp, aes(variable, rnames)) + geom_tile(aes(fill = ifelse(is.infinite(log(value)), 0, log(value))), colour = "white") + 
  viridis::scale_fill_viridis(direction = -1) + xlab("") + ylab("") + labs(fill="log(Z_imp)") +
  theme(text = element_text(size=8)) + scale_x_discrete(breaks = c(1,(1:13)*10), position = "top") + 
  scale_y_discrete(breaks = c(1,(1:13)*10))

ggsave(filename = "./results/heatmap_z_imp.png", plot = p, dpi = 640)

p <- ggplot(Zexp, aes(variable, rnames)) + geom_tile(aes(fill = ifelse(is.infinite(log(value)), 0, log(value))), colour = "white") + 
  viridis::scale_fill_viridis(direction = -1) + xlab("") + ylab("") + labs(fill="log(Z_exp)") +
  theme(text = element_text(size=8)) + scale_x_discrete(breaks = c(1,(1:13)*10), position = "top") + 
  scale_y_discrete(breaks = c(1,(1:13)*10))

ggsave(filename = "./results/heatmap_z_exp.png", plot = p, dpi = 640)

