required.packages <- c("reshape2","ggplot2","data.table","foreign")
lapply(required.packages, require, character.only=T)

setwd("G:/My Drive/Work/GitHub/consumption-floor/")
temp_unz <- unzip("project-data/UGIR7BSV.ZIP", "UGIR7BFL.SAV")
uga.data <- read.spss(temp_unz, to.data.frame=T)
names(uga.data) <- attributes(uga.data)[4]$variable.labels

uga.wealth <- as.data.frame(uga.data$`Wealth index factor score combined (5 decimals)`/100000)
names(uga.wealth) <- "wealth.index"

min.wealth <- min(uga.wealth$wealth.index)

uga.wealth$wealth.index <- uga.wealth$wealth.index - min.wealth
uga.wealth$ind.weights <- 1/length(uga.wealth$wealth.index)
uga.wealth$wom.weights <- (uga.data$`Women's individual sample weight (6 decimals)`/sum(uga.data$`Women's individual sample weight (6 decimals)`))
uga.wealth$urban.weights <- 0
uga.wealth$urban.weights[which(uga.data$`Type of place of residence`=="Urban")] <- 1/length(uga.data$`Type of place of residence`[which(uga.data$`Type of place of residence`=="Urban")])
uga.wealth$rural.weights <- 0
uga.wealth$rural.weights[which(uga.data$`Type of place of residence`=="Rural")] <- 1/length(uga.data$`Type of place of residence`[which(uga.data$`Type of place of residence`=="Rural")])


fgt.index <- function(distribution,weights,z,a){
  fgt <- (1/sum(weights))*sum(weights[which(distribution<z)]*((z-distribution[which(distribution<z)])/z)^a)
  return(fgt)
}

floor.calc <- function(distribution,weights,z,c=1){
  floor <- z*(1-fgt.index(distribution,weights,z,c+1)/fgt.index(distribution,weights,z,c))
  return(floor)
}

mode.kernel <- function(distribution, kernel="g", adjust=1, weights){
  dens <- density(distribution, kernel=kernel, adjust=adjust, weights=weights)
  mode <- dens$x[which(dens$y==max(dens$y))]
  return(mode)
}

wealth.mode <- mode.kernel(uga.wealth$wealth.index, weights=uga.wealth$ind.weights)
wealth.wom.mode <- mode.kernel(uga.wealth$wealth.index, weights=uga.wealth$wom.weights)
wealth.urb.mode <- mode.kernel(uga.wealth$wealth.index, weights=uga.wealth$urban.weights)
wealth.rur.mode <- mode.kernel(uga.wealth$wealth.index, weights=uga.wealth$rural.weights)


uga.floor <- floor.calc(uga.wealth$wealth.index,uga.wealth$ind.weights,wealth.mode) + min.wealth
uga.wom.floor <- floor.calc(uga.wealth$wealth.index,uga.wealth$ind.weights,wealth.wom.mode) + min.wealth
uga.urb.floor <- floor.calc(uga.wealth$wealth.index, weights=uga.wealth$urban.weights, wealth.urb.mode) + min.wealth
uga.rur.floor <- floor.calc(uga.wealth$wealth.index, weights=uga.wealth$rural.weights, wealth.rur.mode) + min.wealth
