library(tsModel) ; library(Epi) ; library(splines) ; 
library(lubridate) ; library(dplyr); library(dlnm); library(mgcv)
library(imputeTS); library(Rcpp); library(ggplot2); library(Amelia)

# clear the workspace
rm(list = ls())

# dirpre <- "C:/Users/ppehavic/OD 1/LSHTM/MCC/MCCOzone/Analysis/"
setwd("D:/data/TempHumidity/00")

# load data
temperature <- readRDS("D:/data/TempHumidity/00/DataTemp_CK.rds")
humidity <- readRDS("D:/data/TempHumidity/00/DataHumidity_CK.rds")
# dlist <- readRDS(paste(dirpre,"output/swiss_cantonal_89-18.rds", sep=""))

# select temperature or humidity
dlist1 <- temperature
dlist2 <- humidity


# MULTIVARIATE IMPUTATION - AMELIA
# i <- 5: Schwyz canton
# i <- 5
# data <- dlist[[i]]
# missmap(data)
#write.table(data, file = "schwyz.txt", sep = ",", quote = FALSE, row.names = T)


# UNIVARIATE IMPUATION - LINEAR
for(i in 5:26) {
  data <- dlist1[[i]]
  
  # save copy
  data$tre200dx_old <- data$tre200dx
  data$tre200d0_old <- data$tre200d0
  data$tre200dn_old <- data$tre200dn
  
  #linear interpolation
  data$tre200dx <- na_interpolation(as.numeric(data$tre200dx), option = "linear", maxgap = Inf)
  data$tre200d0 <- na_interpolation(as.numeric(data$tre200d0), option = "linear", maxgap = Inf)
  data$tre200dn <- na_interpolation(as.numeric(data$tre200dn), option = "linear", maxgap = Inf)
  
  # store the imputation data
  dlist1[[i]] <- data
}

for(i in 5:26) {
  data <- dlist2[[i]]
  
  # save copy
  data$pva200d0_old <- data$pva200d0
  
  #linear interpolation
  data$pva200d0 <- na_interpolation(as.numeric(data$pva200d0), option = "linear", maxgap = Inf)
  
  # store the imputation data
  dlist2[[i]] <- data
}


# store temperature or humidity data
temperature <- dlist1
humidity <- dlist2

# the resulting aggregation of cantonal data is saved into an rds-file
saveRDS(temperature, file = "after-imp-DataTemp_CK.rds") # temperature
saveRDS(humidity, file = "after-imp-DataHumidity_CK.rds") # humidity


for(i in seq(length(dlist1))) {
  data <- dlist1[[i]]
  cat(sum(is.na(data$`max temperature [deg]`)), "")
  cat(sum(is.na(data$`mean temperature [deg]`)), "")
  cat(sum(is.na(data$`min temperature [deg]`)), "")
  #cat(sum(is.na(data$`partial pressure [hPa]`)))
  #cat(sum(is.na(data$`relative humidity [%]`)))
  
  
}

for(i in seq(length(dlist2))) {
  data <- dlist2[[i]]
  # cat(sum(is.na(data$`max temperature [deg]`)), "")
  # cat(sum(is.na(data$`mean temperature [deg]`)), "")
  #cat(sum(is.na(data$`min temperature [deg]`)), "")
  cat(sum(is.na(data$`partial pressure [hPa]`)))
  #cat(sum(is.na(data$`relative humidity [%]`)))
  
  
}

#for (i in seq(length(dlist))) {
# i <- 26 # jura
# data <- dlist[[i]]

#  # NA visualization
#  ggplot_na_distribution(as.numeric(data$`max temperature [deg]`))
#  ggplot_na_distribution(as.numeric(data$`partial pressure [hPa]`))
#  ggplot_na_distribution(as.numeric(data$`relative humidity [%]`))
#  # DOES NOT WORK: ggplot_na_intervals
#  ggplot_na_gapsize(as.numeric(data$`max temperature [deg]`))
#  ggplot_na_gapsize(as.numeric(data$`partial pressure [hPa]`))
#  ggplot_na_gapsize(as.numeric(data$`relative humidity [%]`))
#  
# data$Tint_lin <- na_interpolation(data$`max temperature [deg]`) 
#  na_interpolation(data$`partial pressure [hPa]`)
#  na_interpolation(data$`relative humidity [%]`)
#  
#  
#  # Uses Kalman Smoothing on structural time series models 
#  # (or on the state space representation  of an arima model) for imputation.
#  # DOES NOT WORK: data$Tint_1 <- na_kalman(data$`max temperature [deg]`)
#  
#  
#  # Removes the seasonal component from the time series, performs imputation on the 
#  # deseasonalized series and afterwards adds the seasonal component again.
#  data$Tint_2 <- na_seadec(data$`max temperature [deg]`, algorithm = "interpolation",
#            find_frequency = TRUE,
#            maxgap = Inf)
#  
#  # Splits the times series into seasons and afterwards performs imputation separately 
#  # for each of the resulting time series datasets (each containing the data for one 
#  #                                                 specific season).
#  data$Tint_3 <- na_seasplit(data$`max temperature [deg]`, algorithm = "interpolation",
#            find_frequency = TRUE,
#            maxgap = Inf)
#  




#linear interpolation:
# data$TmaxNEW1 <- na_interpolation(data$`max temperature [deg]`, option = "linear", maxgap = Inf)
# data$PPNEW1 <- na_interpolation(data$`partial pressure [hPa]`, option = "linear", maxgap = Inf)
# data$RHNEW1 <- na_interpolation(data$`relative humidity [%]`, option = "linear", maxgap = Inf)

# #stineman interpolation:
# data$TmaxNEW2 <- na_interpolation(data$`max temperature [deg]`, option = "stine")
# data$PPNEW2 <- na_interpolation(data$`partial pressure [hPa]`, option = "stine")
# data$RHNEW2 <- na_interpolation(data$`relative humidity [%]`, option = "stine")


### DOES NOT WORK: ####
# data$TmaxMA <- na_ma(data$`max temperature [deg]`, k = 4, weighting = "exponential", maxgap = Inf) 
# data$PPMA <- na_ma(data$`partial pressure [hPa]`, k = 4, weighting = "exponential", maxgap = Inf)
# data$RHMA <- na_ma(data$`relative humidity [%]`, k = 4, weighting = "exponential", maxgap = Inf)

#statsNA(data$`max temperature [deg]`)
#ggplot_na_distribution(data$`max temperature [deg]`)
#dlist[[i]] <- data
#}


