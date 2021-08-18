library(tsModel) ; library(Epi) ; library(splines) ; 
library(lubridate) ; library(dplyr); library(dlnm); library(mgcv)

setwd("D:/data/TempHumidity/00")

humidity <- readRDS("D:/data/TempHumidity/00/DataHumidity_CK.rds")
  
mortality <- readRDS("D:/data/TempHumidity/00/DataMortality_CK.rds")

fulldataset <- readRDS("D:/data/TempHumidity/00/DataTemp_CK.rds")

names(fulldataset) <- c("Zuerich", "Bern","Luzern", "Uri", "Schwyz", "Obwalden", "Nidwalden","Glarus", "Zug","Fribourg","Solothurn","Basel-Stadt", "Basel-Landschaft","Schaffhausen","Appenzell Ausserrhoden", "Appenzell Innerrhoden","St. Gallen", "Graubuenden", "Aargau", "Thurgau", "Ticino", "Vaud", "Valais","Neuchatel", "Geneve", "Jura")


# Add the temperature mortality and the humidity data 
for(i in seq(length(fulldataset)) ) {
  data <- fulldataset[[i]]
  data2 <- humidity[[i]]
  data3 <- mortality[[i]]
  
  # calulate the saturation vapor pressure by formula Davis et al. 2015
  data2$sp <- 6.108*exp(17.27*as.numeric(data$tre200d0) / (as.numeric(data$tre200d0) + 237.3))
  # calculate relative humidity as fraction of vapor pressure and saturation pressure
  data2$rh <- (as.numeric(data2$pva200d0) / as.numeric(data2$sp))*100

  # calculate web-bulb temperature by Stull formula (https://www.omnicalculator.com/physics/wet-bulb#how-to-calculate-the-wet-bulb-temperature)
  data$wbt <- as.numeric(data$tre200d0) * atan(0.151977 * (as.numeric(data2$rh) + 8.313659)^(1/2)) + atan(as.numeric(data$tre200d0) + as.numeric(data2$rh))
                                     - atan(as.numeric(data2$rh) - 1.676331)
                                     + 0.00391838 * (as.numeric(data2$rh))^(3/2) * atan(0.023101*as.numeric(data2$rh)) - 4.686035

  #set no entries in death rates to zero:
  data3$deaths[is.na(data3$deaths)] <- 0
  
  
  # select the columns to bind from the two data sets  
  data <- cbind(data["station"], data["date"], data["year"], data["month"], data["dow"], data["tre200dx"], data["tre200dn"], data["tre200d0"], data2["pva200d0"], data2["sp"], data2["rh"], data["wbt"], data3["deaths"])
  #data <- cbind(data["station"], data["date"], data["year"], data["month"], data["dow"], data["tre200dx"], data["tre200dn"], data["tre200d0"], data2["pva200d0"], data3["deaths"])
  
  # rename the columns  
  colnames(data) <- c("canton", "date", "year", "month", "dow", "max temperature [deg]", "min temperature [deg]", "mean temperature [deg]", "partial pressure [hPa]", "saturation pressure [hPa]", "relative humidity [%]", "wet-bulb temperature [deg]", "deaths")
  #colnames(data) <- c("canton", "date", "year", "month", "dow", "max temperature [deg]", "min temperature [deg]", "mean temperature [deg]", "partial pressure [hPa]", "deaths")
  
  fulldataset[[i]] <- data
}
rm(data, data2, data3, humidity, mortality, i)
# the resulting aggregation of cantonal data is saved into an rds-file
#saveRDS(fulldataset, file = "swiss_cantonal_89-18.rds")  
saveRDS(fulldataset, file = "DataTempHumidity_CK.rds")  

# 
# # restrict the data to summer season
# for (i in seq(length(fulldataset))){
#   fulldataset[[i]] <- subset(fulldataset[[i]], month %in% c(5:9), c("date", "deaths", "year","month", "dow", "Tabsd", "monitor", "day", "yday", "station"))
# }

for(i in seq(length(fulldataset))) {
  data <- fulldataset[[i]]
  cat(sum(is.na(data$`max temperature [deg]`)), "")
  #cat(sum(is.na(data$`mean temperature [deg]`)), "")
  #cat(sum(is.na(data$`min temperature [deg]`)), "")
  #cat(sum(is.na(data$`partial pressure [hPa]`)))
  #cat(sum(is.na(data$`relative humidity [%]`)))
  
  
}

for(i in seq(length(fulldataset))) {
  data <- fulldataset[[i]]
  cat(sum(is.na(data$tre200dx)), "")
  cat(sum(is.na(data$tre200dn)), "")
  cat(sum(is.na(data$tre200d0)), "")
}

for(i in seq(length(humidity))) {
  data <- humidity[[i]]
  cat(sum(is.na(data$pva200d0)), "")
}

#### TASK: FIGURE OUT HOW I KEEP ALL VALUES SINCE I DONT HAVE MISSING VALUES IN TEMPERATURE OR HUMIDITY ANYMORE :-) :-)