required.packages <- c("reshape2","ggplot2","data.table","foreign")
lapply(required.packages, require, character.only=T)

setwd("G:/My Drive/Work/GitHub/consumption-floor/")
uga.data <- read.spss("project-data/UGIR7BFL.SAV", to.data.frame=T)
names(uga.data) <- attributes(uga.data)[4]$variable.labels

uga.wealth <- as.data.frame(uga.data$`Wealth index factor score combined (5 decimals)`/100000)
names(uga.wealth) <- "wealth.index"
uga.wealth$wealth.index <- uga.wealth$wealth.index - min(uga.wealth$wealth.index)
uga.wealth$weights <- 1/length(uga.wealth$wealth.index)

mode.kernel <- function(distribution, kernel="g", adjust=1, weights){
  dens <- density(distribution, kernel=kernel, adjust=adjust, weights=weights)
  mode <- dens$x[which(dens$y==max(dens$y))]
  return(mode)
}

fgt.index <- function(distribution,weights,z,a){
  fgt <- (1/sum(weights))*sum(weights[which(distribution<z)]*((z-distribution[which(distribution<z)])/z)^a)
  return(fgt)
}

floor.calc <- function(distribution,weights,z,c=1){
  floor <- z*(1-fgt.index(distribution,weights,z,c+1)/fgt.index(distribution,weights,z,c))
  return(floor)
}

wealth.mode <- mode.kernel(uga.wealth$wealth.index, weights=uga.wealth$weights)

floor.calc(uga.wealth$wealth.index,uga.wealth$weights,wealth.mode)
