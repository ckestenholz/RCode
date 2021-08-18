####################################################################
# Temperature Data Preparation # 10.03.2021 - Christoph Kestenholz #
####################################################################

# The Code aims to aggregate Temperature data on the variable "daily mean, daily min, and daily max temperature"
# among monitor stations in Switzerland. The historical range is from 01.01.1989 - 31.12.2017
# There are at least one monitor per canton, in case of more than one monitor per canton,
# they are either weighted (per population density) or unweighted (equal importance)
# Supplementary information is stored in the files:
#   Documentation_Humidity_Data.docx: Documentation of the data preparation process
#   Arc_Gis_Export.xlsx: Output from weighting method in ArcMap
#   temperature_series_data.txt: Raw data download from IDAWEB Plattform (add URL)
#   temperature_series_2018_data.txt: raw data for 2018 that is merged with the rest


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
X <- read.delim("D:/data/TempHumidity/00/meteo data/temperature_series_data.txt", header = TRUE, sep = "")
Y <- read.delim("D:/data/TempHumidity/00/meteo data/temperature_series_2018_data.txt", header = TRUE, sep = "")

# Assign Missing Values as "NA": e.g.: "-"
X$tre200dx[(X$tre200dx == "")] <- NA
X$tre200dx[(X$tre200dx == "-")] <- NA
Y$tre200dx[(Y$tre200dx == "")] <- NA
Y$tre200dx[(Y$tre200dx == "-")] <- NA

X$tre200dn[(X$tre200dn == "")] <- NA
X$tre200dn[(X$tre200dn == "-")] <- NA
Y$tre200dn[(Y$tre200dn == "")] <- NA
Y$tre200dn[(Y$tre200dn == "-")] <- NA

X$tre200d0[(X$tre200d0 == "")] <- NA
X$tre200d0[(X$tre200d0 == "-")] <- NA
Y$tre200d0[(Y$tre200d0 == "")] <- NA
Y$tre200d0[(Y$tre200d0 == "-")] <- NA

#sum(is.na(X$pva200d0)) # 367 NA in total

# write.table(X, file = "na_TEMP_X.txt", sep = ",", quote = FALSE, row.names = T)
# write.table(Y, file = "na_TEMP_Y.txt", sep = ",", quote = FALSE, row.names = T)

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
stations <- stations[-c(1:36)]; stations <- stations[-c(30, 33)]
stations2018 <- stations2018[-c(1:36)]; stations2018 <- stations2018[-c(30, 33)]

# we end up with 34 monitor stations

# Define the column names, where as tre200(dx,dn,d0) corresponds to max,min mean temperature
for(i in seq(length(stations))) {
  colnames(stations[[i]]) <- c("station", "date", "tre200dx", "tre200dn", "tre200d0")
}
for(i in seq(length(stations2018))) {
  colnames(stations2018[[i]]) <- c("station", "date", "tre200dx", "tre200dn", "tre200d0")
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
rm(data); rm(X)
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
rm(data); rm(Y)

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
Zuerich$tre200dx <- 0.39 * as.numeric(as.character(stations$REH$tre200dx)) + 0.50 * as.numeric(as.character(stations$SMA$tre200dx)) + 0.11 * as.numeric(as.character(stations$KLO$tre200dx))
Zuerich$tre200dn <- 0.39 * as.numeric(as.character(stations$REH$tre200dn)) + 0.50 * as.numeric(as.character(stations$SMA$tre200dn)) + 0.11 * as.numeric(as.character(stations$KLO$tre200dn))
Zuerich$tre200d0 <- 0.39 * as.numeric(as.character(stations$REH$tre200d0)) + 0.50 * as.numeric(as.character(stations$SMA$tre200d0)) + 0.11 * as.numeric(as.character(stations$KLO$tre200d0))
Zuerich$station <- "Zuerich"
# Bern
Bern <- stations$BER
Bern$tre200dx <- 0.85 * as.numeric(as.character(stations$BER$tre200dx)) + 0.15 * as.numeric(as.character(stations$INT$tre200dx))
Bern$tre200dn <- 0.85 * as.numeric(as.character(stations$BER$tre200dn)) + 0.15 * as.numeric(as.character(stations$INT$tre200dn))
Bern$tre200d0 <- 0.85 * as.numeric(as.character(stations$BER$tre200d0)) + 0.15 * as.numeric(as.character(stations$INT$tre200d0))
Bern$station <- "Bern"
# Luzern
Luzern <- stations$LUZ
Luzern$station <- "Luzern"
# Uri
Uri <- stations$ANT
Uri$station <- "Uri"
# # Uri missing values for Tmax:
# UriNA_ind <- which(is.na(Uri$tre200dx)) # index the missing values
# UriNA <- stations$ENG$tre200dx[UriNA_ind] # save the ENG values for the NA index
# Uri$tre200dx[UriNA_ind] <- UriNA
#Schwyz
Schwyz <- stations$EIN
Schwyz$station <- "Schwyz"
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
Solothurn$date <- Bern$date
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
# AppenzellAusserrhoden
AppenzellAusserrhoden <- stations$STG
AppenzellAusserrhoden$station <- "AppenzellAusserrhoden"
# AppenzellInnerhoden
AppenzellInnerrhoden <- stations$STG
AppenzellInnerrhoden$station <- "AppenzellInnerrhoden"
# St.Gallen
St.Gallen <- stations$RAG
St.Gallen$tre200dx <- 0.09 * as.numeric(as.character(stations$RAG$tre200dx)) + 0.91 * as.numeric(as.character(stations$STG$tre200dx))
St.Gallen$tre200dn <- 0.09 * as.numeric(as.character(stations$RAG$tre200dn)) + 0.91 * as.numeric(as.character(stations$STG$tre200dn))
St.Gallen$tre200d0 <- 0.09 * as.numeric(as.character(stations$RAG$tre200d0)) + 0.91 * as.numeric(as.character(stations$STG$tre200d0))
St.Gallen$station <- "St.Gallen"
# Graubuenden
Graubuenden <- stations$CHU
Graubuenden$tre200dx <- 0.86 * as.numeric(as.character(stations$CHU$tre200dx)) + 0.07 * as.numeric(as.character(stations$DAV$tre200dx)) + 0.08 * as.numeric(as.character(stations$SAM$tre200dx))
Graubuenden$tre200dn <- 0.86 * as.numeric(as.character(stations$CHU$tre200dn)) + 0.07 * as.numeric(as.character(stations$DAV$tre200dn)) + 0.08 * as.numeric(as.character(stations$SAM$tre200dn))
Graubuenden$tre200d0 <- 0.86 * as.numeric(as.character(stations$CHU$tre200d0)) + 0.07 * as.numeric(as.character(stations$DAV$tre200d0)) + 0.08 * as.numeric(as.character(stations$SAM$tre200d0))
Graubuenden$station <- "Graubuenden"
# Aargau
Aargau <- stations$BEZ
Aargau$station <- "Aargau"
# Thurgau
Thurgau <- stations$TAE
Thurgau$tre200dx <- 0.43 * as.numeric(as.character(stations$TAE$tre200dx)) + 0.33 * as.numeric(as.character(stations$GUT$tre200dx)) + 0.24 * as.numeric(as.character(stations$HAI$tre200dx))
Thurgau$tre200dn <- 0.43 * as.numeric(as.character(stations$TAE$tre200dn)) + 0.33 * as.numeric(as.character(stations$GUT$tre200dn)) + 0.24 * as.numeric(as.character(stations$HAI$tre200dn))
Thurgau$tre200d0 <- 0.43 * as.numeric(as.character(stations$TAE$tre200d0)) + 0.33 * as.numeric(as.character(stations$GUT$tre200d0)) + 0.24 * as.numeric(as.character(stations$HAI$tre200d0))
Thurgau$station <- "Thurgau"
# Ticino
Ticino <- stations$LUG
Ticino$station <- "Ticino"
# Vaud
Vaud <- stations$CGI
Vaud$tre200dx <- 0.24 * as.numeric(as.character(stations$CGI$tre200dx)) + 0.76 * as.numeric(as.character(stations$PUY$tre200dx))
Vaud$tre200dn <- 0.24 * as.numeric(as.character(stations$CGI$tre200dn)) + 0.76 * as.numeric(as.character(stations$PUY$tre200dn))
Vaud$tre200d0 <- 0.24 * as.numeric(as.character(stations$CGI$tre200d0)) + 0.76 * as.numeric(as.character(stations$PUY$tre200d0))
Vaud$station <- "Vaud"
# Valais
Valais <- stations$SIO
Valais$tre200dx <- 0.79 * as.numeric(as.character(stations$SIO$tre200dx)) + 0.21 * as.numeric(as.character(stations$VIS$tre200dx))
Valais$tre200dn <- 0.79 * as.numeric(as.character(stations$SIO$tre200dn)) + 0.21 * as.numeric(as.character(stations$VIS$tre200dn))
Valais$tre200d0 <- 0.79 * as.numeric(as.character(stations$SIO$tre200d0)) + 0.21 * as.numeric(as.character(stations$VIS$tre200d0))
Valais$station <- "Valais"
# Neuch?tel
Neuchatel <- stations$NEU
Neuchatel$station <- "Neuchatel"
# Gen?ve
Geneve <- stations$GVE
Geneve$station <- "Geneve"
# Jura
Jura <- stations$DEM
Jura$tre200dx <- 0.71 * as.numeric(as.character(stations$DEM$tre200dx)) + 0.29 * as.numeric(as.character(stations$FAH$tre200dx))
Jura$tre200dn <- 0.71 * as.numeric(as.character(stations$DEM$tre200dn)) + 0.29 * as.numeric(as.character(stations$FAH$tre200dn))
Jura$tre200d0 <- 0.71 * as.numeric(as.character(stations$DEM$tre200d0)) + 0.29 * as.numeric(as.character(stations$FAH$tre200d0))
Jura$station <- "Jura"

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

# Reset Index numbers // Set all temperature values to numeric
for(i in seq(length(stations))) {
  data <- stations[[i]]
  row.names(data) <- NULL
  data$tre200dx <- as.numeric(data$tre200dx)
  data$tre200dn <- as.numeric(data$tre200dn)
  data$tre200d0 <- as.numeric(data$tre200d0)
  stations[[i]] <- data
}

# CHECK NA
for(i in seq(length(stations))) {
  data <- stations[[i]]
  cat(sum(is.na(data$tre200dx)), "")
  cat(sum(is.na(data$tre200dn)), "")
  cat(sum(is.na(data$tre200d0)), "")
}

# CHECK NA
for(i in seq(length(stations))) {
  data <- stations[[i]]
  cat(sum(data$tre200dx =="-"), "")
  cat(sum(data$tre200dn =="-"), "")
  cat(sum(data$tre200d0 =="-"), "")
}

# rm(data)

# the resulting aggregation of cantonal data is saved into an rds-file
saveRDS(stations, file = "DataTemp_CK.rds")

#######
# END #
#######
