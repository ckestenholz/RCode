#################################################################
# Humidity Data Preparation # 03.03.2021 - Christoph Kestenholz #
#################################################################

# The Code aims to aggregate Humidity data on the variable "partial pressure" of water vapor
# among monitor stations in Switzerland. The historical range is from 01.01.1989 - 31.12.2017
# There are at least one monitor per canton, in case of more than one monitor per canton,
# they are either weighted (per population density) or unweighted (equal importance)
# Supplementary information is stored in the files:
#   Documentation_Humidity_Data.docx: Documentation of the data preparation process
#   Arc_Gis_Export.xlsx: Output from weighting method in ArcMap
#   vapour_pressure_series_2_data.txt: Raw data download from IDAWEB Plattform (add URL)
#   vapour_pressure_series_2018_data.txt: Raw data for 2018 that is added.

# Define working directory
setwd("D:/data/TempHumidity/00")

# Add libraries
library("plyr")
library("data.table")
library("dplyr")
library("seas")
library("zoo") # for date manipulation
library("lubridate")


# Read Raw data download from IDAWEB
X <- read.delim("D:/data/TempHumidity/00/meteo data/vapour_pressure_series_2_data.txt", header = TRUE, sep = "\t", dec = ".")
Y <- read.delim("D:/data/TempHumidity/00/meteo data/vapour_pressure_series_2018_data.txt", header = TRUE, sep = "", dec = ".")

# Assign Missing Values as "NA": e.g.: "-"
X$pva200d0[(X$pva200d0 == "")] <- NA
X$pva200d0[(X$pva200d0 == "-")] <- NA
Y$pva200d0[(Y$pva200d0 == "")] <- NA
Y$pva200d0[(Y$pva200d0 == "-")] <- NA


sum(is.na(X$pva200d0)) # 367 NA in total
sum(is.na(Y$pva200d0)) # 19 NA in total

# write.table(X, file = "Xna_humi_.txt", sep = ",", quote = FALSE, row.names = T)
# write.table(Y, file = "Yna_humi_.txt", sep = ",", quote = FALSE, row.names = T)

# Define stations by column "stn"
stations <- as.list(levels(as.factor(X$stn)))
stations2018 <- stations
# Add the stations to a data frame
for (f in stations){
  print(f)
  stations[[f]] <- X[X$stn == f, ]
} 
for (f in stations2018){
  print(f)
  stations2018[[f]] <- Y[Y$stn == f, ]
} 
# remove "empty" stations, and stations not used such as "SIR"
stations <- stations[-c(1:38)]; stations <- stations[-c(30, 33)]
stations2018 <- stations2018[-c(1:38)]; stations2018 <- stations2018[-c(30, 33)]

# we end up with 34 monitor stations

# Define the column names, where as pva200d0 corresponds to the mean partial pressure
for(i in seq(length(stations))) {
  colnames(stations[[i]]) <- c("station", "date", "pva200d0")
}
for(i in seq(length(stations2018))) {
  colnames(stations2018[[i]]) <- c("station", "date", "pva200d0")
}

# Precise the date variable into year, month, da, yday, dow
for(i in seq(length(stations))) {
  
  data <- stations[[i]]
  # PRINT
  cat(i,"")
  data$date <- ymd(data$date)
  data$year <- year(data$date)
  data$month <- month(data$date)
  data$day <- day(data$date)
  data$yday <- yday(data$date)
  data$dow <- wday(data$date)
  stations[[i]] <- data
}
rm(data)
for(i in seq(length(stations2018))) {
  
  data <- stations2018[[i]]
  # PRINT
  cat(i,"")
  data$date <- ymd(data$date)
  data$year <- year(data$date)
  data$month <- month(data$date)
  data$day <- day(data$date)
  data$yday <- yday(data$date)
  data$dow <- wday(data$date)
  stations2018[[i]] <- data
}
rm(data); rm(X); rm(Y)

# Merge the data 1989-2017 with the data from 2018:
for(i in seq(length(stations))) {
  
  data <- stations[[i]]
  data2 <- stations2018[[i]]
  # PRINT
  cat(i,"")
  data <- rbind(data, data2)
  stations[[i]] <- data
}
rm(data); rm(data2); rm(stations2018)
#####################################################
# Population weighted monitors aggregated by canton #
#####################################################

# The Input for the significance variables is derived from the Arc_Map_Export.xlsx table

# Zuerich
Zuerich <- stations$REH
Zuerich$pva200d0 <- 0.39 * as.numeric(as.character(stations$REH$pva200d0)) + 0.50 * as.numeric(as.character(stations$SMA$pva200d0)) + 0.11 * as.numeric(as.character(stations$KLO$pva200d0))
Zuerich$station <- "Zuerich"
# Bern
Bern <- stations$BER
Bern$pva200d0 <- 0.85 * as.numeric(as.character(stations$BER$pva200d0)) + 0.15 * as.numeric(as.character(stations$INT$pva200d0))
Bern$station <- "Bern"
# Luzern
Luzern <- stations$LUZ
Luzern$station <- "Luzern"
# Uri
Uri <- stations$ANT
Uri$station <- "Uri"
#Schwyz
Schwyz <- stations$EIN
Schwyz$station <- "Schwyz"
Schwyz <- stations$EIN
# Obwalden
Obwalden <- stations$ENG
Obwalden$station <- "Obwalden"
# Nidwalden
Nidwalden <- stations$ALT
Nidwalden$station <- "Nidwalden"
# Glarus
Glarus <- stations$GLA
Glarus$station <- "Glarus"
# Zug
Zug <- stations$WAE
Zug$station <- "Zug"
# Fribourg
Fribourg <- stations$GRA
Fribourg$station <- "Fribourg"
# Solothurn
Solothurn <- stations$GOE
Solothurn$station <- "Solothurn"
# BaselStadt
BaselStadt <- stations$BAS
BaselStadt$station <- "BaselStadt"
# BaselLandschaft
BaselLandschaft <- stations$BAS
BaselLandschaft$station <- "BaselLandschaft"
# Schaffhausen
Schaffhausen <- stations$SHA
Schaffhausen$station <- "Schaffhausen"
# Schaffhausen$pva200d0 <- as.numeric(Schaffhausen$pva200d0)
# AppenzellAusserhoden
AppenzellAusserrhoden <- stations$STG
AppenzellAusserrhoden$station <- "AppenzellAusserrhoden"
# AppenzellInnerhoden
AppenzellInnerrhoden <- stations$STG
AppenzellInnerrhoden$station <- "AppenzellInnerrhoden"
# St.Gallen
St.Gallen <- stations$RAG
St.Gallen$pva200d0 <- 0.09 * as.numeric(as.character(stations$RAG$pva200d0)) + 0.91 * as.numeric(as.character(stations$STG$pva200d0))
St.Gallen$station <- "St.Gallen"
# Grauebunden
Graubuenden <- stations$CHU
stations$DAV$pva200d0[6925] <- stations$CHU$pva200d0[6925] # NA in DAV replace by CHU value
Graubuenden$pva200d0 <- 0.86 * as.numeric(as.character(stations$CHU$pva200d0)) + 0.07 * as.numeric(as.character(stations$DAV$pva200d0)) + 0.08 * as.numeric(as.character(stations$SAM$pva200d0))
Graubuenden$station <- "Grauebunden"
# Aargau
Aargau <- stations$BEZ
Aargau$station <- "Aargau"
# Thurgau
Thurgau <- stations$TAE
Thurgau$pva200d0 <- 0.43 * as.numeric(as.character(stations$TAE$pva200d0)) + 0.33 * as.numeric(as.character(stations$GUT$pva200d0)) + 0.24 * as.numeric(as.character(stations$HAI$pva200d0))
Thurgau$station <- "Thurgau"
# Ticino
Ticino <- stations$LUG
Ticino$station <- "Ticino"
# Vaud
Vaud <- stations$CGI
Vaud$pva200d0 <- 0.24 * as.numeric(as.character(stations$CGI$pva200d0)) + 0.76 * as.numeric(as.character(stations$PUY$pva200d0))
Vaud$station <- "Vaud"
# Valais
Valais <- stations$SIO
Valais$pva200d0 <- 0.79 * as.numeric(as.character(stations$SIO$pva200d0)) + 0.21 * as.numeric(as.character(stations$VIS$pva200d0))
Valais$station <- "Valais"
# Neuch?tel
Neuchatel <- stations$NEU
Neuchatel$station <- "Neuchatel"
# Gen?ve
Geneve <- stations$GVE
Geneve$station <- "Geneve"
# Jura
Jura <- stations$DEM
Jura$pva200d0 <- 0.71 * as.numeric(as.character(stations$DEM$pva200d0)) + 0.29 * as.numeric(as.character(stations$FAH$pva200d0))
Jura$station <- "Jura"

# There is a limited data set for the monitors Goesgen and Beznau. In order to have the same
# data dimension, we fill up the data set with NA's: 

# replace missing values in Solothurn by Zollikofen (BER) monitor for 1989-2008
SolothurnNA <- stations[[4]][stations[[4]]$date >= "1989-01-01" & stations[[4]]$date <= "2008-06-03", ]
SolothurnNA$station <- "Solothurn"
Solothurn <- rbind(SolothurnNA, Solothurn)

# replace missing values in Aargau by Zollikofen (BER) monitor for 1989-2008
AargauNA <- stations[[4]][stations[[4]]$date >= "1989-01-01" & stations[[4]]$date <= "2008-06-03", ]
AargauNA$station <- "Aargau"
Aargau <- rbind(AargauNA, Aargau)

rm(SolothurnNA, AargauNA)

# 06.08.2021: INSTEAD WE REPLACE
# THE MISSING VALUE WITH VALUES FROM A CLOSE MONITOR STATION (ZOLLIKOFEN): IS OUTDATED!!

# # Add NA's for missing years in Solothurn and Aargau data
# SolothurnNA <- Bern[Bern$date >= "1989-01-01" & Bern$date <= "2008-06-03", ]
# SolothurnNA$pva200d0 <- NA; SolothurnNA$station <- "Solothurn"
# Solothurn <- rbind(SolothurnNA, Solothurn)
# 
# # Add NA's for missing years in Solothurn and Aargau data
# AargauNA <- Bern[Bern$date >= "1989-01-01" & Bern$date <= "2008-06-03", ]
# AargauNA$pva200d0 <- NA; AargauNA$station <- "Aargau"
# Aargau <- rbind(AargauNA, Aargau)

# Add the stations into a large list: removed Solothurn and Aargau
stations <- list(Zuerich, Bern, Luzern, Uri, Schwyz, Obwalden, Nidwalden, Glarus, Zug,
                 Fribourg, Solothurn, BaselStadt, BaselLandschaft, Schaffhausen, 
                 AppenzellAusserrhoden, AppenzellInnerrhoden, St.Gallen, Graubuenden, 
                 Aargau, Thurgau, Ticino, Vaud, Valais, Neuchatel, Geneve, Jura)


# Precise the station name: removed Solothurn and Aargau
names(stations) <- c("Zuerich", "Bern","Luzern", "Uri", "Schwyz", "Obwalden", "Nidwalden","Glarus", "Zug",
                     "Fribourg", "Solothurn", "Basel-Stadt", "Basel-Landschaft","Schaffhausen",
                     "Appenzell Ausserrhoden", "Appenzell Innerrhoden","St. Gallen", "Graubuenden", 
                     "Aargau", "Thurgau", "Ticino", "Vaud", "Valais","Neuchatel", "Geneve", "Jura")

# Reset Index numbers // Set all humdity values to numeric
for(i in seq(length(stations))) {
  data <- stations[[i]]
  row.names(data) <- NULL
  data$pva200d0 <- as.numeric(data$pva200d0)
  stations[[i]] <- data
}

# CHECK NA
for(i in seq(length(stations))) {
  data <- stations[[i]]
  cat(sum(is.na(data$pva200d0)), "")
}

# CHECK NA
for(i in seq(length(stations))) {
  data <- stations[[i]]
  cat(sum(data$pva200d0=="-"), "")
}

# rm(data)



# the resulting aggregation of cantonal data is saved into an rds-file
saveRDS(stations, file = "DataHumidity_CK.rds")

#######
# END #
#######