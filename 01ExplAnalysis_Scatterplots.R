# Scatterplots Temperature and Humidity, Poisson - Distribution Plots

# working directory
setwd("D:/data/TempHumidity/01")

# set language to english
Sys.setenv(LANG = "en")

## Define a function to run the study at once:
run <- function(x){
  
  
  data <- readRDS("D:/data/TempHumidity/00/after-imp-DataTempHumidity_CK.rds")
  
  # import libraries
  library(tidyverse)
  library(ISOweek)
  library(lubridate)
  library(ggplot2)
  library(ggthemes)
  library(RColorBrewer)
  library(ggpubr)
  library(cowplot)
  library(qpdf)
  library(xlsx)
  
  # import color palette 
  col_reg <- c(brewer.pal(12, "Paired"), "black")
  col_reg[11] <- "Khaki4" # instead of yellow
  
  # SELECT SPECIFIC CANTON TO RUN THE STUDY
  cant <- "Zuerich"
  # cant <- "Geneve"
  # cant <- "Valais"
  # cant <- "Ticino" 
  # cant <- "Basel-Stadt"
  
  for (cant in c("Zuerich", "Geneve", "Valais", "Ticino", "Basel-Stadt")) {
    
    print("-----------------------------------------------")
    print("The selected canton for the Excess Mortality is")
    print(paste0(cant))
    print("-----------------------------------------------")
    
    # SELECT SERIES AFTER 1999
    if (cant == "Zuerich") {
      dta <- subset(data$Zuerich, year>"1999")
    } else {
      if (cant == "Geneve") {
        dta <- subset(data$Geneve, year>"1999")
      } else {
        if (cant == "Valais") {
          dta <- subset(data$Valais, year>"1999")
        } else {
          if (cant == "Ticino") {
            dta <- subset(data$Ticino, year> "1999")
          } else {
            if (cant == "Basel-Stadt") {
              dta <- subset(data$`Basel-Stadt`, year> "1999")
            } else {
              print("No canton selected!")
            }
          }
        }
      }
    }
    
    # SET TO 0 THE NA
    dta[is.na(dta)] <- 0
    
    # time preparation
    dta$week <- isoweek(dta$date)
    dta$year <- year(dta$date)
    dta$month <- month(dta$date)
    
    dta$year_re <- rep(NA, nrow(dta))
    for (i in seq(2000,2018)){
      dta$year_re[dta$week==52 & dta$year==i  & dta$month==1] <- i-1
      dta$year_re[dta$week==1 & dta$year==i  & dta$month==12] <- i+1
    }
    dta$year_re[is.na(dta$year_re)] <- dta$year[is.na(dta$year_re)]
    
    # EXCLUSIONS
    dta <- subset(dta, week!=53)
    dta <- subset(dta, year_re!=2019)
    
    
    # create a data only containing extended summer months (may-sept):
    dta_summer <- dta
    dta_summer <- subset(dta_summer, month %in% c(5:9), c("date", "year", "month","dow", "max temperature [deg]", "min temperature [deg]", "mean temperature [deg]", "partial pressure [hPa]", "saturation pressure [hPa]", "relative humidity [%]", "deaths"))
    
    # PLOT histogram for exceedances for temperature and humidity for summers 2010-2018:
    scatter_plot_pp <- ggplot(dta_summer, aes(x=`partial pressure [hPa]`, y=`max temperature [deg]`)) + 
      geom_point() +
      geom_smooth(method='lm',formula= y~x, se=FALSE) +
      ggtitle(paste0("Scatterplot for temperature and humidity: Summers 2010-2018", "\n", cant, sep=" ")) +
      xlab("mean partial pressure humidity [hPa]") +
      ylab("maximum temperature [deg]") +
      theme_minimal() 
    scatter_plot_pp
    #ggsave(paste0(getwd(), "/img/", cant, "_scatter_pp",".pdf", sep=""), device = "pdf", width=8)
    
    
    scatter_plot_rh <- ggplot(dta_summer, aes(x=`relative humidity [%]`, y=`max temperature [deg]`)) + 
      geom_point() +
      geom_smooth(method='lm',formula= y~x, se=FALSE) +
      ggtitle(paste0("Scatterplot for temperature and humidity: Summers 2010-2018", "\n", cant, sep=" ")) +
      xlab("relative humidity [%]") +
      ylab("maximum temperature [deg]") +
      theme_minimal() 
    scatter_plot_rh
    #ggsave(paste0(getwd(), "/img/", cant, "_scatter_pp",".pdf", sep=""), device = "pdf", width=8)
    
    ml = lm(`max temperature [deg]`~ `partial pressure [hPa]`, data=dta_summer)
    summary(ml)$r.squared
    
    ml2 = lm(`max temperature [deg]`~ `relative humidity [%]`, data=dta_summer)
    summary(ml2)$r.squared
    # SELECT SPECIFIC SUMMER TO RUN THE STUDY
    yr <- "2003"
    # yr <- "2015"
    # yr <- "2018"
    
    dta_yr <- subset(dta, week!=53)
    dta_yr <- subset(dta_summer, year %in% c(2003,2015,2018))
    
    
    # for (yr in c("2003", "2015", "2018")) {
    #   
    #   print("-----------------------------------------------")
    #   print("The selected year for the Excess Mortality is")
    #   print(paste0(yr))
    #   print("-----------------------------------------------")
    #   
    #   # SELECT SERIES AFTER 1999
    #   if (yr == "2003") {
    #     dta_yr <- subset(dta, week!=53)
    #     dta_yr <- subset(dta, year_re==2003&month %in% c(5:9))
    #   } else {
    #     if (yr == "2015") {
    #       dta_yr <- subset(dta, week!=53)
    #       dta_yr <- subset(dta, year_re==2015&month %in% c(5:9))
    #     } else {
    #       if (yr == "2018") {
    #         dta_yr <- subset(dta, week!=53)
    #         dta_yr <- subset(dta, year_re==2018&month %in% c(5:9))
    #       } else {
    #         print("No year was selected!")
    #       }
    #     }
    #   }
      
      
      # PLOT histogram for exceedances for temperature and humidity for summers 2010-2018:
      scatter_plot_yr_pp <- ggplot(dta_yr, aes(x=`partial pressure [hPa]`, y=`max temperature [deg]`, color=as.factor(year))) + 
        geom_point() +
        scale_color_manual(values = c(col_reg[2], col_reg[8], col_reg[4])) +
        ggtitle(paste0("Scatterplot for temperature and humidity: Summers 2003, 2015 and 2018 ", "\n", cant, sep=" ")) +
        xlab("mean partial pressure humidity [hPa]") +
        ylab("maximum temperature [deg]") +
        labs(colour="Summer") +
        theme_minimal() 
      scatter_plot_yr_pp
      #ggsave(paste0(getwd(), "/img/", cant, "yr, "_scatter_pp",".pdf", sep=""), device = "pdf", width=8)
      # Correlation
      #cor(dta$`partial pressure [hPa]`, dta$`max temperature [deg]`,  method = "pearson", use = "complete.obs")
      #[1] 0.8737593
      
      scatter_plot_yr_rh <- ggplot(dta_yr, aes(x=`relative humidity [%]`, y=`max temperature [deg]`, color=as.factor(year))) + 
        geom_point() +
        scale_color_manual(values = c(col_reg[2], col_reg[8], col_reg[4])) +
        ggtitle(paste0("Scatterplot for temperature and humidity: Summers 2003, 2015 and 2018 ", "\n", cant, sep=" ")) +
        xlab("relative humidity [%]") +
        ylab("maximum temperature [deg]") +
        labs(colour="Summer") +
        theme_minimal() 
      scatter_plot_yr_rh
      #ggsave(paste0(getwd(), "/img/", cant, yr, "_scatter_pp",".pdf", sep=""), device = "pdf", width=8)
      # Correlation
      #cor(dta$`relative humidity [%]`, dta$`max temperature [deg]`,  method = "pearson", use = "complete.obs")
      #[1] -0.5524744
      
      
      scatter_plot_yr_pp2 <- ggplot(dta_yr, aes(x=`max temperature [deg]`, y=`deaths`, color=as.factor(year))) + 
        geom_point() +
        scale_color_manual(values = c(col_reg[2], col_reg[8], col_reg[4])) +
        ggtitle(paste0("Scatterplot for temperature and humidity: Summers 2003, 2015 and 2018 ", "\n", cant, sep=" ")) +
        xlab("maximum temperature [deg]") +
        ylab("deaths") +
        labs(colour="Summer") +
        theme_minimal() 
      scatter_plot_yr_pp2
      
      
      ######
      # fit normal distributions
      
      library(fitdistrplus)
      
      fit <- fitdistr(dta_yr$`max temperature [deg]`, densfun= "normal")
      fit
      
      hist(dta_yr$`max temperature [deg]`, pch=20, breaks=25, prob=TRUE, main="")
      curve(dnorm(x, fit$estimate[1], fit$estimate[2]), col="red", lwd=2, add=T)
      
      ###############
      
      # PLOT Distribution for temperature and humidity for summers 2010-2018:
      distr_plot_yr_tmax <- ggplot(dta_yr, aes(x=`max temperature [deg]`, group=as.factor(year), fill=as.factor(year))) + 
        geom_density(adjust=1.5, alpha=.4) +
        scale_color_manual(values = c(col_reg[2], col_reg[8], col_reg[4])) +
        ggtitle(paste0("Distribution for temperature: Summers 2003, 2015 and 2018 ", "\n", cant, sep=" ")) +
        xlab("maximum temperature [deg]") +
        ylab("density") +
        labs(colour="Summer") +
        theme_minimal() 
      distr_plot_yr_tmax
      #ggsave(paste0(getwd(), "/img/", cant, "yr, "_distr_tmax" ,".pdf", sep=""), device = "pdf", width=8)
      
      distr_plot_yr_pp <- ggplot(dta_yr, aes(x=`partial pressure [hPa]`, group=as.factor(year), fill=as.factor(year))) + 
        geom_density(adjust=1.5, alpha=.4) +
        scale_color_manual(values = c(col_reg[2], col_reg[8], col_reg[4])) +
        ggtitle(paste0("Distribution for humidity: Summers 2003, 2015 and 2018 ", "\n", cant, sep=" ")) +
        xlab("mean partial pressure humidity [hPa]") +
        ylab("density") +
        labs(colour="Summer") +
        theme_minimal() 
      distr_plot_yr_pp
      #ggsave(paste0(getwd(), "/img/", cant, "yr, "_distr_pp" ,".pdf", sep=""), device = "pdf", width=8)
      
      distr_plot_yr_rh <- ggplot(dta_yr, aes(x=`relative humidity [%]`, group=as.factor(year), fill=as.factor(year))) + 
        geom_density(adjust=1.5, alpha=.4) +
        scale_color_manual(values = c(col_reg[2], col_reg[8], col_reg[4])) +
        ggtitle(paste0("Distribution for humidity: Summers 2003, 2015 and 2018 ", "\n", cant, sep=" ")) +
        xlab("relative humidity [%]") +
        ylab("density") +
        labs(colour="Summer") +
        theme_minimal() 
      distr_plot_yr_rh
      #ggsave(paste0(getwd(), "/img/", cant, "yr, "_distr_pp" ,".pdf", sep=""), device = "pdf", width=8)
      
      
      ######
      
      
      # PLOT Distribution for temperature and humidity for summers 2010-2018:
      distr_plot_yr_mort <- ggplot(dta_yr, aes(x=`deaths`, group=as.factor(year), fill=as.factor(year))) + 
        geom_density(adjust=1.5, alpha=.4) +
        scale_color_manual(values = c(col_reg[2], col_reg[8], col_reg[4])) +
        ggtitle(paste0("Distribution of deaths: Summers 2003, 2015 and 2018 ", "\n", cant, sep=" ")) +
        xlab("deaths [counts]") +
        ylab("density") +
        labs(colour="Summer") +
        theme_minimal() 
      distr_plot_yr_mort
      #ggsave(paste0(getwd(), "/img/", cant, "yr, "_distr_tmax" ,".pdf", sep=""), device = "pdf", width=8)
      
      distr_plot_mort <- ggplot(dta_summer, aes(x=`deaths`, group=as.factor(year), fill=as.factor(year))) + 
        geom_density(adjust=1.5, alpha=.4) +
        scale_color_manual(values = c(col_reg[2], col_reg[8], col_reg[4])) +
        ggtitle(paste0("Distribution of deaths: Summers 2003, 2015 and 2018 ", "\n", cant, sep=" ")) +
        xlab("deaths [counts]") +
        ylab("density") +
        labs(colour="Summer") +
        theme_minimal() 
      distr_plot_mort
      #ggsave(paste0(getwd(), "/img/", cant, "yr, "_distr_tmax" ,".pdf", sep=""), device = "pdf", width=8)
      