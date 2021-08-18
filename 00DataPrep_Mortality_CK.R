# mortality data 2018: data aggregation

# library(magrittr) # needs to be run every time you start R and want to use %>%
# don't load this otherwise the pipes do not work anymore


library(dplyr)    # alternatively, this also loads %>%
library(tidyverse)
library(ISOweek)
library(lubridate)
library(ggplot2)
library(ggthemes)

setwd("D:/data/TempHumidity/00")

# SET DIRECTORY OF DATA INPUT
dir <- "D:/data/temporal_trends_paper"

# LOAD DATA
munciptable <- readRDS(paste0(dir,"/municipalitylookuptable.rds"))
deathrecords <- readRDS(paste0(dir,"/deathrecordsmuncipality.rds"))

# SAVE ONLY DEATHS of 2018
#deathrecords <- deathrecords[(deathrecords$yy=="2018"),]
deathrecords <- deathrecords[!(deathrecords$yy<"1989"),]

# # CREATE AGE CATE3GORY
# deathrecords$age_cat <- NA
# deathrecords$age_cat <- deathrecords$age_death
# deathrecords$age_cat = ifelse(deathrecords$age_cat<=64,"1", ifelse(deathrecords$age_cat<=79,"2", ifelse(deathrecords$age_cat<=150, "3")))


# and use in the pipes, or the cplyr package, "group_by(date,kantonname)

# CREATE LOOKUP TABLE
# lookuptable <- deathrecords %>%
#   group_by(NAME,comm_resi, assigned_number,Kantonname) %>%
#   summarise(KANTONSNUM = mean(KANTONSNUM), deaths=sum(deaths))
# 
# # CREATE LOOKUP TABLE
# lookuptable <- deathrecords %>%
#   group_by(NAME,comm_resi, assigned_number,Kantonname) %>%
#   summarise(KANTONSNUM = mean(KANTONSNUM), deaths=sum(deaths))# 

# X <- deathrecords %>%
#   group_by(date, Kantonname) %>%
#   summarise(date=date, KANTONSNUM = mean(KANTONSNUM), deaths=sum(deaths))

Y <- deathrecords %>%
  group_by(KANTONSNUM, Kantonname, date) %>%
  summarise(KANTONSNUM = mean(KANTONSNUM), deaths=sum(deaths))# 

# Z <- deathrecords %>%
#   group_by(NAME,comm_resi, assigned_number,Kantonname) %>%
#   summarise(KANTONSNUM = mean(KANTONSNUM), deaths=sum(deaths))

# Define stations by column "stn"
stations <- as.list(levels(as.factor(Y$Kantonname)))

# create a large list
for (f in stations){
  print(f)
  stations[[f]] <- Y[Y$Kantonname == f, ]
} 
stations <- stations[-c(1:26)]

for (f in stations){
  print(length(f$date))
}

date1 <- as.Date("1989/1/1")
date2 <- as.Date("2018/12/31")

# w <- seq(date1, date2, by="days")
# 
# for (f in stations){
#   print(f)
#   stations[[f]] %>%
#     mutate(date = as.Date(date)) %>%
#     complete(date = seq.Date(date1, date2, by="day"))
# }


# for (f in stations){
#   print(f)
#   stations[f] %<>%
#     mutate(date = as.Date(date)) %>%
#     complete(date = seq.Date(date1, date2, by="day"))
# }




# FILL IN MISSING DATE VALUES AND DEAH RATES (na)
stations$Aargau %<>%
  mutate(date = as.Date(date)) %>%
  complete(date = seq.Date(date1, date2, by="day"))

stations$`Appenzell Ausserrhoden` %<>%
  mutate(date = as.Date(date)) %>%
  complete(date = seq.Date(date1, date2, by="day"))

stations$`Appenzell Innerrhoden` %<>%
  mutate(date = as.Date(date)) %>%
  complete(date = seq.Date(date1, date2, by="day"))

stations$`Basel-Landschaft` %<>%
  mutate(date = as.Date(date)) %>%
  complete(date = seq.Date(date1, date2, by="day"))

stations$`Basel-Stadt` %<>%
  mutate(date = as.Date(date)) %>%
  complete(date = seq.Date(date1, date2, by="day"))

stations$Bern %<>%
  mutate(date = as.Date(date)) %>%
  complete(date = seq.Date(date1, date2, by="day"))

stations$Fribourg %<>%
  mutate(date = as.Date(date)) %>%
  complete(date = seq.Date(date1, date2, by="day"))

stations$Geneva %<>%
  mutate(date = as.Date(date)) %>%
  complete(date = seq.Date(date1, date2, by="day"))

stations$Glarus %<>%
  mutate(date = as.Date(date)) %>%
  complete(date = seq.Date(date1, date2, by="day"))

stations$Graub?nden %<>%
  mutate(date = as.Date(date)) %>%
  complete(date = seq.Date(date1, date2, by="day"))

stations$Jura %<>%
  mutate(date = as.Date(date)) %>%
  complete(date = seq.Date(date1, date2, by="day"))

stations$Luzern %<>%
  mutate(date = as.Date(date)) %>%
  complete(date = seq.Date(date1, date2, by="day"))

stations$Neuch?tel %<>%
  mutate(date = as.Date(date)) %>%
  complete(date = seq.Date(date1, date2, by="day"))

stations$Nidwalden %<>%
  mutate(date = as.Date(date)) %>%
  complete(date = seq.Date(date1, date2, by="day"))

stations$Obwalden %<>%
  mutate(date = as.Date(date)) %>%
  complete(date = seq.Date(date1, date2, by="day"))

stations$Schaffhausen %<>%
  mutate(date = as.Date(date)) %>%
  complete(date = seq.Date(date1, date2, by="day"))

stations$Schwyz %<>%
  mutate(date = as.Date(date)) %>%
  complete(date = seq.Date(date1, date2, by="day"))

stations$Solothurn %<>%
  mutate(date = as.Date(date)) %>%
  complete(date = seq.Date(date1, date2, by="day"))

stations$`St. Gallen` %<>%
  mutate(date = as.Date(date)) %>%
  complete(date = seq.Date(date1, date2, by="day"))

stations$Thurgau %<>%
  mutate(date = as.Date(date)) %>%
  complete(date = seq.Date(date1, date2, by="day"))

stations$Ticino %<>%
  mutate(date = as.Date(date)) %>%
  complete(date = seq.Date(date1, date2, by="day"))

stations$Uri %<>%
  mutate(date = as.Date(date)) %>%
  complete(date = seq.Date(date1, date2, by="day"))

stations$Valais %<>%
  mutate(date = as.Date(date)) %>%
  complete(date = seq.Date(date1, date2, by="day"))

stations$Vaud %<>%
  mutate(date = as.Date(date)) %>%
  complete(date = seq.Date(date1, date2, by="day"))

stations$Zug %<>%
  mutate(date = as.Date(date)) %>%
  complete(date = seq.Date(date1, date2, by="day"))

stations$Z?rich %<>%
  mutate(date = as.Date(date)) %>%
  complete(date = seq.Date(date1, date2, by="day"))

for (f in stations){
  print(length(f$date))
}
# reorder the list
stationsnew <- stations[(c("Z?rich", "Bern","Luzern", "Uri", "Schwyz", "Obwalden", "Nidwalden","Glarus", "Zug",
                                        "Fribourg", "Solothurn", "Basel-Stadt", "Basel-Landschaft","Schaffhausen",
                                        "Appenzell Ausserrhoden", "Appenzell Innerrhoden","St. Gallen", "Graub?nden", 
                                        "Aargau", "Thurgau", "Ticino", "Vaud", "Valais","Neuch?tel", "Geneva", "Jura"))]


# Precise the station name: removed Solothurn and Aargau
names(stationsnew) <- c("Z?rich", "Bern","Luzern", "Uri", "Schwyz", "Obwalden", "Nidwalden","Glarus", "Zug",
                     "Fribourg", "Solothurn", "Basel-Stadt", "Basel-Landschaft","Schaffhausen",
                     "Appenzell Ausserhoden", "Appenzell Innerhoden","St. Gallen", "Gra?bunden", 
                     "Aargau", "Thurgau", "Ticino", "Vaud", "Valais","Neuch?tel", "Gen?ve", "Jura")



# for(i in seq(length(stations2018new))) {
#   
#   data <- stationsnew[[i]]
#   # PRINT
#   cat(i,"")
#   data$date <- ymd(data$date)
#   data$year <- year(data$date)
#   data$month <- month(data$date)
#   data$day <- day(data$date)
#   data$yday <- yday(data$date)
#   data$dow <- wday(data$date)
#   stationsnew[[i]] <- data
# }
# rm(data)

# the resulting aggregation of cantonal mortality data is saved into an rds-file
saveRDS(stationsnew, file = "datamortalitychristoph.rds")
