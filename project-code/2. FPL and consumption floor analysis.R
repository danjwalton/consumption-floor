required.packages <- c("reshape2","ggplot2","WDI","data.table")
lapply(required.packages, require, character.only=T)

setwd("G:/My Drive/Work/GitHub/consumption-floor/")

data <- read.csv("project-data/PL data.csv")
ext <- read.csv("project-data/povcal_out.csv")
Povcal.CPI <- read.csv("project-data/doc_cpi_list.csv")

#Download FAO food CPI and total CPI
temp_download <- tempfile(pattern = "", fileext = ".zip")
download.file("http://fenixservices.fao.org/faostat/static/bulkdownloads/ConsumerPriceIndices_E_All_Data.zip",temp_download)
FAO.CPI <- read.csv(unz(temp_download,"ConsumerPriceIndices_E_All_Data_NOFLAG.csv"))
unlink(temp_download)

#Establish year range
data$Current.year <- as.character(data$Current.year)
data$Current.year.1 <- as.numeric(substr(data$Current.year,0,4))
data$Current.year.2 <- as.numeric(substr(data$Current.year,6,9))
start.year <- min(data$Current.year.1,na.rm=T)
end.year <- max(rbind(data$Current.year.1,data$Current.year.2),na.rm=T)

WDI.CPI <- WDI(indicator="FP.CPI.TOTL",start=start.year,end=end.year,extra=T)
WDI.ISO <- unique(WDI.CPI[,c("country","iso3c")])
WDI.CPI <- WDI.CPI[,c("country","year","FP.CPI.TOTL")]
WDI.PPP <- WDI(indicator="PA.NUS.PRVT.PP",start=2011,end=2011)[,c("country","PA.NUS.PRVT.PP")]

remap_cov = function(x){
  cov_dict = c(
    "R"=1,
    "U"=2,
    "N"=3,
    "A"=NA
  )
  return(cov_dict[as.character(x)])
}
remap_cov = Vectorize(remap_cov)

ext$svy_code = remap_cov(ext$CoverageType)
ext = subset(ext,!is.na(svy_code))
ext$C0 = paste(ext$CountryCode,ext$svy_code,sep="_")

Povcal.PPP <- unique(ext[,c("C0","PPP")])

WDI.ISO$iso3c[which(WDI.ISO$country=="North Macedonia")] <- "MKD"

#Replace old Povcal country codes with new and melt
Povcal.CPI$ISO[which(Povcal.CPI$ISO=="ZAR")] <- "COD"
Povcal.CPI$ISO[which(Povcal.CPI$ISO=="WBG")] <- "PSE"
Povcal.CPI$ISO[which(Povcal.CPI$ISO=="KSV")] <- "XKX"
Povcal.CPI$ISO[which(Povcal.CPI$ISO=="TMP")] <- "TLS"
Povcal.CPI <- merge(Povcal.CPI,WDI.ISO, by.x="ISO",by.y="iso3c")[,c("country","ISO","Year","Pop","R.Pop","U.Pop","CPI","R.CPI","U.CPI")]
Povcal.CPI.melt <- melt(Povcal.CPI[,c("country","ISO","Year","CPI","R.CPI","U.CPI")], id.vars = c("country","ISO","Year"))
Povcal.CPI.melt <- Povcal.CPI.melt[complete.cases(Povcal.CPI.melt),]

#Use national CPI for rural and urban Indonesia
Povcal.CPI.melt.IDN <- subset(Povcal.CPI.melt, country == "Indonesia")
Povcal.CPI.melt.IDN.R <- Povcal.CPI.melt.IDN
Povcal.CPI.melt.IDN.R$variable <- "R.CPI"
Povcal.CPI.melt.IDN.U <- Povcal.CPI.melt.IDN
Povcal.CPI.melt.IDN.U$variable <- "U.CPI"
Povcal.CPI.melt <- rbind(Povcal.CPI.melt,Povcal.CPI.melt.IDN.R,Povcal.CPI.melt.IDN.U)

#Align names to data
Povcal.CPI.melt$country.append <- ""
Povcal.CPI.melt$ISO.append <- "_3"
Povcal.CPI.melt$country.append[which(Povcal.CPI.melt$variable=="R.CPI")] <- "--Rural"
Povcal.CPI.melt$ISO.append[which(Povcal.CPI.melt$variable=="R.CPI")] <- "_1"
Povcal.CPI.melt$country.append[which(Povcal.CPI.melt$variable=="U.CPI")] <- "--Urban"
Povcal.CPI.melt$ISO.append[which(Povcal.CPI.melt$variable=="U.CPI")] <- "_2"
Povcal.CPI.melt$country <- paste0(Povcal.CPI.melt$country,Povcal.CPI.melt$country.append)
Povcal.CPI.melt$ISO <- paste0(Povcal.CPI.melt$ISO,Povcal.CPI.melt$ISO.append)
Povcal.CPI.melt <- Povcal.CPI.melt[,c("country","ISO","Year","value")]

#Replace FAO country names with WB version where different
FAO.CPI$WB.country <- as.character(FAO.CPI$Area)
{
  FAO.CPI$WB.country[which(FAO.CPI$Area=="Bahamas")] <- "Bahamas, The"
  FAO.CPI$WB.country[which(FAO.CPI$Area=="Bolivia (Plurinational State of)")] <- "Bolivia"
  FAO.CPI$WB.country[which(FAO.CPI$Area=="China, mainland")] <- "China"
  FAO.CPI$WB.country[which(FAO.CPI$Area=="China, Hong Kong SAR")] <- "Hong Kong SAR, China"
  FAO.CPI$WB.country[which(FAO.CPI$Area=="China, Macao SAR")] <- "Macao SAR, China"
  FAO.CPI$WB.country[which(FAO.CPI$Area=="Congo")] <- "Congo, Rep."
  FAO.CPI$WB.country[which(FAO.CPI$Area=="Democratic Republic of the Congo")] <- "Congo, Dem. Rep."
  FAO.CPI$WB.country[which(FAO.CPI$Area=="Côte d'Ivoire")] <- "Cote d'Ivoire"
  FAO.CPI$WB.country[which(FAO.CPI$Area=="Czechia")] <- "Czech Republic"
  FAO.CPI$WB.country[which(FAO.CPI$Area=="Egypt")] <- "Egypt, Arab Rep."
  FAO.CPI$WB.country[which(FAO.CPI$Area=="Gambia")] <- "Gambia, The"
  FAO.CPI$WB.country[which(FAO.CPI$Area=="Iran (Islamic Republic of)")] <- "Iran, Islamic Rep."
  FAO.CPI$WB.country[which(FAO.CPI$Area=="Kyrgyzstan")] <- "Kyrgyz Republic"
  FAO.CPI$WB.country[which(FAO.CPI$Area=="Lao People's Democratic Republic")] <- "Lao PDR"
  FAO.CPI$WB.country[which(FAO.CPI$Area=="The former Yugoslav Republic of Macedonia")] <- "North Macedonia"
  FAO.CPI$WB.country[which(FAO.CPI$Area=="Occupied Palestinian Territory")] <- "West Bank and Gaza"
  FAO.CPI$WB.country[which(FAO.CPI$Area=="Republic of Korea")] <- "Korea, Rep."
  FAO.CPI$WB.country[which(FAO.CPI$Area=="Republic of Moldova")] <- "Moldova"
  FAO.CPI$WB.country[which(FAO.CPI$Area=="Saint Kitts and Nevis")] <- "St. Kitts and Nevis"
  FAO.CPI$WB.country[which(FAO.CPI$Area=="Saint Lucia")] <- "St. Lucia"
  FAO.CPI$WB.country[which(FAO.CPI$Area=="Saint Vincent and the Grenadines")] <- "St. Vincent and the Grenadines"
  FAO.CPI$WB.country[which(FAO.CPI$Area=="Slovakia")] <- "Slovak Republic"
  FAO.CPI$WB.country[which(FAO.CPI$Area=="United Republic of Tanzania")] <- "Tanzania"
  FAO.CPI$WB.country[which(FAO.CPI$Area=="United States of America")] <- "United States"
  FAO.CPI$WB.country[which(FAO.CPI$Area=="Venezuela (Bolivarian Republic of)")] <- "Venezuela, RB"
  FAO.CPI$WB.country[which(FAO.CPI$Area=="Viet Nam")] <- "Vietnam"
  FAO.CPI$WB.country[which(FAO.CPI$Area=="Yemen")] <- "Yemen, Rep."
}

#Melt FAO by country, year, indicator
FAO.CPI.melt <- melt(FAO.CPI,id.vars = c("Area.Code","Area","Item.Code","Item","Months.Code","Months","Unit","WB.country"))
FAO.CPI.melt$variable <- as.numeric(substr(FAO.CPI.melt$variable,2,5))
FAO.CPI.melt <- FAO.CPI.melt[,c("WB.country","Item","Months","variable","value")]

#Calculate FAO mean indices by year
FAO.CPI.mean <- aggregate(FAO.CPI.melt, by = list(FAO.CPI.melt$WB.country,FAO.CPI.melt$Item,FAO.CPI.melt$variable), drop= T, FUN=mean, na.rm=T)
FAO.CPI.mean <- FAO.CPI.mean[,c("Group.1","Group.2","Group.3","value")]
names(FAO.CPI.mean) <- c("country","indicator","year","value")

#Separate FCPI and CPI, merge WDI and rebase to 2011
FCPI <- subset(FAO.CPI.mean, indicator == "Consumer Prices, Food Indices (2010 = 100)")[,c("country","year","value")]
FCPI.2011 <- subset(FCPI,year==2011)
FCPI.2011 <- merge(FCPI,FCPI.2011,by=("country"),all.x=T)
FCPI.2011$value.2011 <- FCPI.2011$value.x/FCPI.2011$value.y
FCPI.2011 <- FCPI.2011[,c("country","year.x","value.2011")]
CPI <- subset(FAO.CPI.mean, indicator == "Consumer Prices, General Indices (2010 = 100)")[,c("country","year","value")]
CPI <- merge(CPI, WDI.CPI,by=c("country","year"),all.y=T)
CPI.2011 <- subset(CPI,year==2011)
CPI.2011 <- merge(CPI,CPI.2011,by=("country"),all.x=T)
CPI.2011$FAO.value.2011 <- CPI.2011$value.x/CPI.2011$value.y
CPI.2011$WDI.value.2011 <- CPI.2011$FP.CPI.TOTL.x/CPI.2011$FP.CPI.TOTL.y
CPI.2011 <- CPI.2011[,c("country","year.x","FAO.value.2011","WDI.value.2011")]

#Merge indices with data
data <- merge(data,FCPI.2011, by.x=c("Country","Current.year.1"),by.y=c("country","year.x"),all.x=T)
names(data)[which(names(data)=="value.2011")] <- "FCPI.1"
data <- merge(data,FCPI.2011, by.x=c("Country","Current.year.2"),by.y=c("country","year.x"),all.x=T)
names(data)[which(names(data)=="value.2011")] <- "FCPI.2"
data <- merge(data,CPI.2011, by.x=c("Country","Current.year.1"),by.y=c("country","year.x"),all.x=T)
names(data)[which(names(data)=="FAO.value.2011")] <- "FAO.CPI.1"
names(data)[which(names(data)=="WDI.value.2011")] <- "WDI.CPI.1"
data <- merge(data,CPI.2011, by.x=c("Country","Current.year.2"),by.y=c("country","year.x"),all.x=T)
names(data)[which(names(data)=="FAO.value.2011")] <- "FAO.CPI.2"
names(data)[which(names(data)=="WDI.value.2011")] <- "WDI.CPI.2"
data <- merge(data, Povcal.CPI.melt, by.x=c("Country", "Povcal.code","Current.year.1"),by.y=c("country","ISO","Year"),all.x=T)
data <- merge(data, Povcal.CPI.melt, by.x=c("Country", "Povcal.code","Current.year.2"),by.y=c("country", "ISO","Year"),all.x=T)
names(data)[which(names(data)=="value.x")] <- "Povcal.CPI.1"
names(data)[which(names(data)=="value.y")] <- "Povcal.CPI.2"
data$Povcal.CPI.1 <- as.numeric(data$Povcal.CPI.1)
data$Povcal.CPI.2 <- as.numeric(data$Povcal.CPI.2)

#Average indicies across current years. Note that years missing data are not included in the average
data$FCPI.2011 <- rowMeans(cbind(data$FCPI.1,data$FCPI.2),na.rm=T)
data$FAO.CPI.2011 <- rowMeans(cbind(data$FAO.CPI.1,data$FAO.CPI.2),na.rm=T)
data$WDI.CPI.2011 <- rowMeans(cbind(data$WDI.CPI.1,data$WDI.CPI.2),na.rm=T)
data$Povcal.CPI.2011 <- rowMeans(cbind(data$Povcal.CPI.1,data$Povcal.CPI.2),na.rm=T)
data[is.na(data)] <- NA

data <- data[,c("Country","Povcal.code","Country.code","Current.year","Income.group","FPL.current.","NPL.current.","Calories","FCPI.2011","FAO.CPI.2011","WDI.CPI.2011","Povcal.CPI.2011")]

#Merge PPP conversions
data <- merge(data,WDI.PPP, by.x="Country",by.y="country",all.x=T)
data <- merge(data,Povcal.PPP, by.x="Povcal.code",by.y="C0",all.x=T)
names(data)[which(names(data)=="PA.NUS.PRVT.PP")] <- "WDI.PPP.conversion"
names(data)[which(names(data)=="PPP")] <- "Povcal.PPP.conversion"

PL <- data[,c("Country","Povcal.code","Current.year","Income.group")]

normalise.FPL.calories <- function(FPL,calories){
  if(!is.na(calories)){
    FPL.out <- FPL/calories*2100
    } else {
      FPL.out <- FPL
    }
  return(FPL.out)
}

normalise.NPL.calories <- function(NPL,FPL,calories){
  if(!is.na(calories)){
    NPL.excess <- NPL - FPL
    FPL.cal <- FPL/calories*2100
    NPL.out <- FPL.cal + NPL.excess
  } else {
    NPL.out <- NPL
  }
  return(NPL.out)
}

fcpi.deflate <- function(FPL,fcpi,povcal.cpi,WDI.cpi,FAO.cpi){
  if(!is.na(fcpi)){
    FPL.out <- FPL/fcpi
  } else {
    if(!is.na(povcal.cpi)){
      FPL.out <- FPL/povcal.cpi
    } else { 
      if(!is.na(WDI.cpi)){
        FPL.out <- FPL/WDI.cpi
      } else {
        FPL.out <- FPL/FAO.cpi
      }
    }
  }
  return(FPL.out)
}

cpi.deflate <- function(PL,povcal.cpi,WDI.cpi,FAO.cpi){
  if(!is.na(povcal.cpi)){
      PL.out <- PL/povcal.cpi
    } else { 
      if(!is.na(WDI.cpi)){
        PL.out <- PL/WDI.cpi
      } else {
        PL.out <- PL/FAO.cpi
      }
    }
  return(PL.out)
}

PPP.convert <- function(PL,povcal.PPP,WDI.PPP){
  if(!is.na(povcal.PPP)){
    PL.out <- PL/povcal.PPP
    } else {
      PL.out <- PL/WDI.PPP
      }
  return(PL.out)
}

i <- 1
for (i in 1:nrow(data)){
  PL[i,"FPL.2011LCU"] <- normalise.FPL.calories(data[i,"FPL.current."],data[i,"Calories"])
  PL[i,"FPL.2011LCU"] <- fcpi.deflate(PL[i,"FPL.2011LCU"],data[i,"FCPI.2011"],data[i,"Povcal.CPI.2011"],data[i,"WDI.CPI.2011"],data[i,"FAO.CPI.2011"])
  PL[i,"FPL.2011PPP"] <- PPP.convert(PL[i,"FPL.2011LCU"],data[i,"Povcal.PPP.conversion"],data[i,"WDI.PPP.conversion"])
  PL[i,"NPL.2011LCU"] <- normalise.NPL.calories(data[i,"NPL.current."],data[i,"FPL.current."],data[i,"Calories"])
  PL[i,"NPL.2011LCU"] <- cpi.deflate(PL[i,"NPL.2011LCU"],data[i,"Povcal.CPI.2011"],data[i,"WDI.CPI.2011"],data[i,"FAO.CPI.2011"])
  PL[i,"NPL.2011PPP"] <- PPP.convert(PL[i,"NPL.2011LCU"],data[i,"Povcal.PPP.conversion"],data[i,"WDI.PPP.conversion"])
}

FPL <- PL[,c("Povcal.code","FPL.2011PPP")]
FPL <- FPL[complete.cases(FPL),]

write.csv(PL,"output/PLs.csv",row.names=F)

PL.data <- as.data.table(PL)[!(Current.year=="")]

#Query Povcal with FPL(PPP) set as PL. There is a more efficient way of doing this (multiple countries and poverty lines per query), but the API doesn't seem to be able to handle it
povcal.out.PPP <- function(country,year="all",PL){
  param <- paste0("RefYears=",year,"&PovertyLine=",PL,"&C0=",country)
  url <- paste0("http://iresearch.worldbank.org/PovcalNet/PovcalNetAPI.ashx?",param,"&display=c")
  return(read.csv(url,header=T))
}

i <- 1
FP.list <- list()
for (i in 1:nrow(FPL)){
  FP.list[[i]] <- povcal.out.PPP(FPL[i,"Povcal.code"],year="all",PL=FPL[i,"FPL.2011PPP"])
}

FP.povcal.PPP <- rbindlist(FP.list)

#Query Povcal with FPL(LCU) set as PPP. This gives equivalent results to above.
povcal.out.FPL <- function(country,year="all",PL=1,PPP){
  param <- paste0("RefYears=",year,"&PovertyLine=",PL,"&C0=",country,"&PPP0=",PPP)
  url <- paste0("http://iresearch.worldbank.org/PovcalNet/PovcalNetAPI.ashx?",param,"&display=c")
  return(read.csv(url,header=T))
}

i <- 1
FP.list <- list()
for (i in 1:nrow(PL)){
  FP.list[[i]] <- povcal.out.FPL(PL.data[i,"Povcal.code"],year="all",PL=1,PL.data[i,"FPL.2011PPP"])
}

FP.povcal.FPL <- rbindlist(FP.list)

#Query Povcal as normal at extreme poverty
povcal.out <- function(country,year="all",PL=1.9){
  param <- paste0("RefYears=",year,"&PovertyLine=",PL,"&C0=",country)
  url <- paste0("http://iresearch.worldbank.org/PovcalNet/PovcalNetAPI.ashx?",param,"&display=c")
  return(read.csv(url,header=T))
}

i <- 1
povcal.list <- list()
for (i in 1:nrow(PL)){
  povcal.list[[i]] <- povcal.out(PL.data[i,"Povcal.code"],year="all",PL=1.9)
}

povcal <- rbindlist(povcal.list)
names(povcal) <- paste0(names(povcal),".extreme")

FP.povcal.PPP <- cbind(FP.povcal.PPP,povcal[,c("PovertyLine.extreme","HeadCount.extreme","PovGap.extreme","PovGapSqr.extreme")])
fwrite(FP.povcal.PPP,"output/Povcal food poverty.csv")

#Calculate national consumption floors based on food poverty lines, both in PPP dollar terms and FPL terms
FP.floors.PPP <- FP.povcal.PPP[,c(4,3,6,7,9,11,13,14,15)]
FP.floors.PPP[which(FP.floors.PPP$CoverageType=="R")]$CountryName <- paste0(FP.floors.PPP[which(FP.floors.PPP$CoverageType=="R")]$CountryName,"--Rural")
FP.floors.PPP[which(FP.floors.PPP$CoverageType=="U")]$CountryName <- paste0(FP.floors.PPP[which(FP.floors.PPP$CoverageType=="U")]$CountryName,"--Urban")
FP.floors.PPP$floor <- FP.floors.PPP$PovertyLine*(1-FP.floors.PPP$PovGapSqr/FP.floors.PPP$PovGap)
FP.floors.PPP <- FP.floors.PPP[,c(1,4,5,10)]

FP.floors.FPL <- FP.povcal.FPL[,c(4,3,6,7,9,11,13,14,15)]
FP.floors.FPL[which(FP.floors.FPL$CoverageType=="R")]$CountryName <- paste0(FP.floors.FPL[which(FP.floors.FPL$CoverageType=="R")]$CountryName,"--Rural")
FP.floors.FPL[which(FP.floors.FPL$CoverageType=="U")]$CountryName <- paste0(FP.floors.FPL[which(FP.floors.FPL$CoverageType=="U")]$CountryName,"--Urban")
FP.floors.FPL$floor <- FP.floors.FPL$PovertyLine*(1-FP.floors.FPL$PovGapSqr/FP.floors.FPL$PovGap)
FP.floors.FPL <- FP.floors.FPL[,c(1,4,5,10)]
FP.floors.FPL$floor.cal <- FP.floors.FPL$floor*2100

povcal.floors <- povcal[,c(4,3,6,7,9,11,13,14,15)]
povcal.floors[which(povcal.floors$CoverageType=="R")]$CountryName <- paste0(povcal.floors[which(povcal.floors$CoverageType=="R")]$CountryName,"--Rural")
povcal.floors[which(povcal.floors$CoverageType=="U")]$CountryName <- paste0(povcal.floors[which(povcal.floors$CoverageType=="U")]$CountryName,"--Urban")
povcal.floors$floor <- povcal.floors$PovertyLine*(1-povcal.floors$PovGapSqr/povcal.floors$PovGap)
povcal.floors <- povcal.floors[,c(1,4,5,10)]

FP.floors.FPL.agg <- FP.povcal.FPL[,c(6,7,11,14,15,21)]
#FP.floors.FPL.agg <- FP.floors.FPL.agg[which(FP.floors.FPL.agg$CoverageType != "A")]
FP.floors.FPL.agg$PovGap.Pop <- FP.floors.FPL.agg$PovGap*FP.floors.FPL.agg$ReqYearPopulation
FP.floors.FPL.agg$PovGapSqr.Pop <- FP.floors.FPL.agg$PovGapSqr*FP.floors.FPL.agg$ReqYearPopulation
FP.floors.FPL.agg <- FP.floors.FPL.agg[,c(2,3,6,7,8)]
FP.floors.FPL.agg[which(FP.floors.FPL.agg$PovGap.Pop==0)]$ReqYearPopulation <- 0
FP.floors.FPL.agg <- aggregate(FP.floors.FPL.agg, by=list(FP.floors.FPL.agg$RequestYear,FP.floors.FPL.agg$PovertyLine),FUN=sum)
FP.floors.FPL.agg$PovGap <- FP.floors.FPL.agg$PovGap.Pop/FP.floors.FPL.agg$ReqYearPopulation
FP.floors.FPL.agg$PovGapSqr <- FP.floors.FPL.agg$PovGapSqr.Pop/FP.floors.FPL.agg$ReqYearPopulation
FP.floors.FPL.agg <- FP.floors.FPL.agg[,c(1,2,8,9)]
FP.floors.FPL.agg$floor <- FP.floors.FPL.agg$Group.2*(1-FP.floors.FPL.agg$PovGapSqr/FP.floors.FPL.agg$PovGap)
FP.floors.FPL.agg$floor.cal <- FP.floors.FPL.agg$floor*2100

FP.floors.FPL.agg.ex.chn <- FP.povcal.FPL[,c(4,6,7,11,14,15,21)]
FP.floors.FPL.agg.ex.chn <- FP.floors.FPL.agg.ex.chn[which(FP.floors.FPL.agg.ex.chn$CountryName != "China")]
FP.floors.FPL.agg.ex.chn$PovGap.Pop <- FP.floors.FPL.agg.ex.chn$PovGap*FP.floors.FPL.agg.ex.chn$ReqYearPopulation
FP.floors.FPL.agg.ex.chn$PovGapSqr.Pop <- FP.floors.FPL.agg.ex.chn$PovGapSqr*FP.floors.FPL.agg.ex.chn$ReqYearPopulation
FP.floors.FPL.agg.ex.chn <- FP.floors.FPL.agg.ex.chn[,c(3,4,7,8,9)]
FP.floors.FPL.agg.ex.chn[which(FP.floors.FPL.agg.ex.chn$PovGap.Pop==0)]$ReqYearPopulation <- 0
FP.floors.FPL.agg.ex.chn <- aggregate(FP.floors.FPL.agg.ex.chn, by=list(FP.floors.FPL.agg.ex.chn$RequestYear,FP.floors.FPL.agg.ex.chn$PovertyLine),FUN=sum)
FP.floors.FPL.agg.ex.chn$PovGap <- FP.floors.FPL.agg.ex.chn$PovGap.Pop/FP.floors.FPL.agg.ex.chn$ReqYearPopulation
FP.floors.FPL.agg.ex.chn$PovGapSqr <- FP.floors.FPL.agg.ex.chn$PovGapSqr.Pop/FP.floors.FPL.agg.ex.chn$ReqYearPopulation
FP.floors.FPL.agg.ex.chn <- FP.floors.FPL.agg.ex.chn[,c(1,2,8,9)]
FP.floors.FPL.agg.ex.chn$floor <- FP.floors.FPL.agg.ex.chn$Group.2*(1-FP.floors.FPL.agg.ex.chn$PovGapSqr/FP.floors.FPL.agg.ex.chn$PovGap)
FP.floors.FPL.agg.ex.chn$floor.cal <- FP.floors.FPL.agg.ex.chn$floor*2100

povcal.floors.agg <- povcal[,c(6,7,11,14,15,21)]
#povcal.floors.agg <- povcal.floors.agg[which(povcal.floors.agg$CoverageType != "A")]
povcal.floors.agg$PovGap.Pop <- povcal.floors.agg$PovGap*povcal.floors.agg$ReqYearPopulation
povcal.floors.agg$PovGapSqr.Pop <- povcal.floors.agg$PovGapSqr*povcal.floors.agg$ReqYearPopulation
povcal.floors.agg <- povcal.floors.agg[,c(2,3,6,7,8)]
povcal.floors.agg[which(povcal.floors.agg$PovGap.Pop==0)]$ReqYearPopulation <- 0
povcal.floors.agg <- aggregate(povcal.floors.agg, by=list(povcal.floors.agg$RequestYear,povcal.floors.agg$PovertyLine),FUN=sum)
povcal.floors.agg$PovGap <- povcal.floors.agg$PovGap.Pop/povcal.floors.agg$ReqYearPopulation
povcal.floors.agg$PovGapSqr <- povcal.floors.agg$PovGapSqr.Pop/povcal.floors.agg$ReqYearPopulation
povcal.floors.agg <- povcal.floors.agg[,c(1,2,8,9)]
povcal.floors.agg$floor <- povcal.floors.agg$Group.2*(1-povcal.floors.agg$PovGapSqr/povcal.floors.agg$PovGap)

povcal.floors.agg.ex.chn <- povcal[,c(4,6,7,11,14,15,21)]
povcal.floors.agg.ex.chn <- povcal.floors.agg.ex.chn[which(povcal.floors.agg.ex.chn$CountryName != "China")]
povcal.floors.agg.ex.chn$PovGap.Pop <- povcal.floors.agg.ex.chn$PovGap*povcal.floors.agg.ex.chn$ReqYearPopulation
povcal.floors.agg.ex.chn$PovGapSqr.Pop <- povcal.floors.agg.ex.chn$PovGapSqr*povcal.floors.agg.ex.chn$ReqYearPopulation
povcal.floors.agg.ex.chn <- povcal.floors.agg.ex.chn[,c(3,4,7,8,9)]
povcal.floors.agg.ex.chn[which(povcal.floors.agg.ex.chn$PovGap.Pop==0)]$ReqYearPopulation <- 0
povcal.floors.agg.ex.chn <- aggregate(povcal.floors.agg.ex.chn, by=list(povcal.floors.agg.ex.chn$RequestYear,povcal.floors.agg.ex.chn$PovertyLine),FUN=sum)
povcal.floors.agg.ex.chn$PovGap <- povcal.floors.agg.ex.chn$PovGap.Pop/povcal.floors.agg.ex.chn$ReqYearPopulation
povcal.floors.agg.ex.chn$PovGapSqr <- povcal.floors.agg.ex.chn$PovGapSqr.Pop/povcal.floors.agg.ex.chn$ReqYearPopulation
povcal.floors.agg.ex.chn <- povcal.floors.agg.ex.chn[,c(1,2,8,9)]
povcal.floors.agg.ex.chn$floor <- povcal.floors.agg.ex.chn$Group.2*(1-povcal.floors.agg.ex.chn$PovGapSqr/povcal.floors.agg.ex.chn$PovGap)

#Calculate kernel densities
FPL.kernel <- as.data.frame(cbind(density(PL$FPL.2011PPP, adjust=1, kernel="g", na.rm=T)$x, density(PL$FPL.2011PPP, adjust=1, kernel="g", na.rm=T)$y))
NPL.kernel <- as.data.frame(cbind(density(PL$NPL.2011PPP, adjust=1, kernel="g", na.rm=T)$x, density(PL$NPL.2011PPP, adjust=1, kernel="g", na.rm=T)$y))
Floor.FPL.kernel <- as.data.frame(cbind(density(FP.floors.FPL$floor.cal, adjust=1, kernel="g", na.rm=T)$x, density(FP.floors.FPL$floor.cal, adjust=1, kernel="g", na.rm=T)$y))
Floor.povcal.kernel <- as.data.frame(cbind(density(povcal.floors$floor, adjust=1, kernel="g", na.rm=T)$x, density(povcal.floors$floor, adjust=1, kernel="g", na.rm=T)$y))

i <- 1
kernel.list <- list()
for(year in unique(FP.floors.FPL$RequestYear)){
  kernel.list[[year-1980]] <- as.data.frame(cbind(density(subset(FP.floors.FPL, RequestYear==year)$floor.cal, adjust=1, kernel="g",na.rm=T,bw="SJ",from=0,to=2100)$x,density(subset(FP.floors.FPL, RequestYear==year)$floor.cal, adjust=1, kernel="g",na.rm=T,bw="SJ",from=0,to=2100)$y))
kernel.list[[year-1980]]$year <- year
  }
floor.kernels <- rbindlist(kernel.list)
names(floor.kernels) <- c("floor.cal","density","year")
floor.kernels <- melt(floor.kernels,id.vars=c(1,3))
floor.kernels <- dcast(floor.kernels, floor.cal ~ year)
write.csv(floor.kernels, "output/Floor kernels.csv")

FPL.kernel.plot <- ggplot(subset(PL,Income.group != "High income")) +
  geom_density(aes(FPL.2011PPP,col=Income.group,fill=Income.group,alpha=I(0.5)),adjust=1, kernel="g", na.rm=T)
ggsave("output/FPL.kernel.plot.png", FPL.kernel.plot)

NPL.kernel.plot <- ggplot(subset(PL,Income.group != "High income")) +
  geom_density(aes(NPL.2011PPP,col=Income.group,fill=Income.group,alpha=I(0.5)),adjust=1,kernel="g", na.rm=T)
ggsave("output/NPL.kernel.plot.png", NPL.kernel.plot)

Floor.FPL.kernel.plot <- ggplot(subset(FP.floors.FPL,RequestYear==2015|RequestYear==1981)) +
  geom_density(aes(floor.cal,col=as.character(RequestYear),fill=as.character(RequestYear),alpha=I(0.5)),adjust=1, kernel="g", na.rm=T)
ggsave("output/Floor.FPL.kernel.plot.png", Floor.FPL.kernel.plot)

Floor.povcal.kernel.plot <- ggplot(subset(povcal.floors,RequestYear==2015|RequestYear==1981)) +
  geom_density(aes(floor,col=as.character(RequestYear),fill=as.character(RequestYear),alpha=I(0.5)),adjust=1, kernel="g", na.rm=T)
ggsave("output/Floor.povcal.kernel.plot.png", Floor.povcal.kernel.plot)

Floors.comparison <- merge(FP.floors.FPL,povcal.floors, by=c("CountryName","RequestYear","DataType"))[,c(1,2,3,4,6)]
Floors.comparison$floor.y <- Floors.comparison$floor.y/1.9
names(Floors.comparison) <- c("CountryName","RequestYear","DataType","FPL.floor","IPL.floor")
Floors.comparison <- melt(Floors.comparison, id.vars=c(1,2,3))

Floors.comparison.kernel.plot <- ggplot(subset(Floors.comparison, RequestYear==2015))+
  geom_density(aes(value, col=variable, fill=variable, alpha=I(0.5)),adjust=1, kernel="g",na.rm=T)
ggsave("output/Floors.comparison.kernel.plot.png",Floors.comparison.kernel.plot)

rm(FP.list,povcal.list,CPI,CPI.2011,FAO.CPI,FAO.CPI.mean,FAO.CPI.melt,FCPI,FCPI.2011,Povcal.CPI,Povcal.CPI.melt,Povcal.PPP,WDI.CPI,WDI.ISO,WDI.PPP,Povcal.CPI.melt.IDN,Povcal.CPI.melt.IDN.R,Povcal.CPI.melt.IDN.U)
