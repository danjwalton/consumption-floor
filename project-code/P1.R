required.packages <- c("reshape2","ggplot2","data.table")
lapply(required.packages, require, character.only=T)

setwd("G:/My Drive/Work/GitHub/consumption-floor/")

povcal.threshold.global <- function(targetHC,year="all",lowerguess=0, upperguess=10,precision=0.005){
  data.list <- list()
  PLs <- seq(lowerguess,upperguess,by=precision)
  pb <- txtProgressBar(min=0, max=length(PLs),style=3)
  for (i in 1:length(PLs)){
    param <- paste0("Countries=all&RefYears=",year,"&PovertyLine=",PLs[i])
    url <- paste0("http://iresearch.worldbank.org/PovcalNet/PovcalNetAPI.ashx?",param,"&display=a")
    temp <- read.csv(url,header=T)
    data.list[[i]] <- temp
    setTxtProgressBar(pb,i)
  }
  close(pb)
  data <- rbindlist(data.list)
  data$diff <- abs(targetHC-data$hc)
  datatemp <- list()
  for(year in unique(data$requestYear)){
    datatemp[[year-1980]] <- data[which(requestYear==year&data$diff==min(data[which(requestYear==year)]$diff))]
  }
  thresholds <- rbindlist(datatemp)
  return(thresholds)
}

povcal.threshold.national <- function(targetHC,year="all",lowerguess=0, upperguess=10,precision=0.005){
  data.list <- list()
  PLs <- seq(lowerguess,upperguess,by=precision)
  pb <- txtProgressBar(min=0, max=length(PLs),style=3)
  for (i in 1:length(PLs)){
    param <- paste0("Countries=all&RefYears=",year,"&PovertyLine=",PLs[i])
    url <- paste0("http://iresearch.worldbank.org/PovcalNet/PovcalNetAPI.ashx?",param,"&display=c")
    temp <- read.csv(url,header=T)
    data.list[[i]] <- temp
    setTxtProgressBar(pb,i)
  }
  close(pb)
  data <- rbindlist(data.list)
  data$diff <- abs(targetHC-data$HeadCount)
  #datatemp <- list()
  #i <- 1
  #for(country in unique(data$CountryName)){
    #for(year in unique(data$RequestYear)){
      #datatemp[[i]] <- data[which(CountryName==country&RequestYear==year&data$diff==min(data[which(RequestYear==year&CountryName==country)]$diff))]
      #i = i + 1
      #}
  #}
  #thresholds <- rbindlist(datatemp)
  datadt <- as.data.table(data)
  datadt$CountryYear <- paste0(datadt$CountryName,datadt$CoverageType,datadt$RequestYear)
  datadt <- datadt[datadt[, .I[diff == min(diff)], by=CountryYear]$V1]
  datadt <- datadt[datadt[, .I[PovertyLine==min(PovertyLine)], by=CountryYear]$V1]
  return(datadt)
}

P1.thresholds.global <- povcal.threshold.all(targetHC=0.01,year="all",lowerguess=0.3,upperguess=0.7,precision=0.005)

P20.thresholds.global <- povcal.threshold.all(targetHC=0.2,year="all",lowerguess=1,upperguess=3,precision=0.005)

P1.thresholds.national <- povcal.threshold.national(targetHC = 0.01, year="all", lowerguess=0, upperguess=10, precision=0.005)

P20.thresholds.national <- povcal.threshold.national(targetHC = 0.2, year="all", lowerguess=0, upperguess=50, precision=0.005)
