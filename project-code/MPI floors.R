required.packages <- c("reshape2","ggplot2","WDI","data.table","foreign")
lapply(required.packages, require, character.only=T)

setwd("G:/My DriVe/Work/GitHub/consumption-floor/")
temp_unz <- unzip("project-data/UGPR7BSV.ZIP", "UGPR7BFL.SAV")
uga.data <- read.spss(temp_unz, to.data.frame=T)

#names(uga.data) <- attributes(uga.data)[4]$variable.labels

uga.schooling <- subset(uga.data, select=c("HV108","HV121"))
uga.adultnutrition <- subset(uga.data, select=c("HA40","HB40"))

uga.livingstandards <- subset(uga.data, select=c("HV206","HV205","HV225","HV201","HV204","HV202","HV213","HV214","HV215","HV226","HV208","HV207","HV221","HV243A","HV209","HV212","HV210","HV211","HV243E","HV243C"))

