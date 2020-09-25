library(stats)
library(data.table)
library(lubridate)
library(readxl)
library(magrittr)
library(stringr)
library(car)
library(sandwich)
library(timeSeries)
library(tseries)
library(urca)
library(tidyr)
library(zoo)
library(forecast)

#Set working directory
setwd("C:/R/Git Repo/projects_github/solar_forecast/")

#FUNCTION TO CONVERT DATA TYPE
convert_type <- function(x, type){
  
  switch(type,
         
         "character" = return(as.character(x)),
         "date" = return(parse_date_time(x, "mdy HMS p")),
         "numeric" = return(as.numeric(x)))
  
}


#BUILD data
solar.data.folder <- "Data/"
solar.data.files <- "solar_gen"

#Create list of all data file names
solar.data.list <- list.files(path = solar.data.folder, pattern = solar.data.files, full.names = TRUE)

#Aggregate Data
solar.data.master.list <- lapply(solar.data.list, function(x) {read.csv(x, header = FALSE, skip = 1)})
solar.data.dt <- do.call(rbind.data.frame,solar.data.master.list) %>% setDT()
solar.data.dt %>% setnames(.,names(solar.data.dt),names(read.csv(solar.data.list[1], header = TRUE)))


#Check structure of data and convert data types if necessary
solar.data.structure <- as.data.table(names(solar.data.dt)) %>% setnames(.,"V1","COL_NAME")
solar.data.structure[, TYPE.POSITIVE := c("date","date","character","numeric")]
solar.data.structure[,TYPE.NORMATIVE := sapply(solar.data.dt, class) %>% list %>% setDT()]


for (i in 1:nrow(solar.data.structure)){
  
  col_heading <-  solar.data.structure[,COL_NAME[i]]
  should.be.type <- solar.data.structure[,TYPE.POSITIVE[i]]
  current.type <- solar.data.structure[,TYPE.NORMATIVE[i]]
  
  if (current.type != should.be.type){
    
    solar.data.dt[, eval(col_heading) := convert_type(get(col_heading),should.be.type)]
    
  }
  
}

#SORT data by area and date
solar.data.dt %>% setorderv(.,cols = "datetime_beginning_ept", order = 1)
solar.data.dt[ ,":=" (Year = year(datetime_beginning_ept),
                      Month = month(datetime_beginning_ept),
                      Date = date(datetime_beginning_ept),
                      HE = hour(datetime_beginning_ept)+1)]
solar.data.dt[ , DMO := ymd(paste(Year,Month,1,sep="-"))]
                     
#Plot the solar output data by area
par(mfrow=c(5,1))
plot(x = solar.data.dt[area == "RTO",datetime_beginning_ept], y = solar.data.dt[area == "RTO", solar_generation_mw])
plot(x = solar.data.dt[area == "MIDATL", datetime_beginning_ept], y = solar.data.dt[area == "MIDATL", solar_generation_mw])
plot(x = solar.data.dt[area == "SOUTH", datetime_beginning_ept], y = solar.data.dt[area == "SOUTH", solar_generation_mw])
plot(x = solar.data.dt[area == "WEST",datetime_beginning_ept], y = solar.data.dt[area == "WEST", solar_generation_mw])
plot(x = solar.data.dt[area == "OTHER",datetime_beginning_ept], y = solar.data.dt[area == "OTHER", solar_generation_mw])

#Summarize data
solar.data.summary.dt <- solar.data.dt[,lapply(.SD, function(x) mean(x, na.rm = TRUE)), .SDcols = "solar_generation_mw", by = c("area","Month","HE")] %>% setnames(.,"solar_generation_mw","AVG.MW")
solar.data.summary.dt <- solar.data.summary.dt[solar.data.dt[, lapply(.SD, function(x) sd(x, na.rm = TRUE)), .SDcols = "solar_generation_mw", by = c("area","Month","HE")], on = c("area","Month","HE")] %>% setnames(.,"solar_generation_mw","STD.MW")
solar.data.summary.dt <- solar.data.summary.dt[solar.data.dt[, lapply(.SD, function(x) max(x, na.rm = TRUE)), .SDcols = "solar_generation_mw", by = c("area","Month","HE")], on = c("area","Month","HE")] %>% setnames(.,"solar_generation_mw","MAX.MW")   
solar.data.summary.dt <- solar.data.summary.dt[solar.data.dt[, lapply(.SD, function(x) min(x, na.rm = TRUE)), .SDcols = "solar_generation_mw", by = c("area","Month","HE")], on = c("area","Month","HE")] %>% setnames(.,"solar_generation_mw","MIN.MW")   
#solar.data.summary.dt[,Month := month(DMO)]

#PLOT output by month x hour
par(mfrow=c(1,1))

for (i in 1:12){
  
  data <- solar.data.summary.dt[Month == i & area == "RTO",]
  
  x <- data[,HE]
  y <- data[,AVG.MW]
  plot(x,y)
  
}

#ANALYZE data for seasonality and trend
#SUBSET data for a specific hour for RTO level data
test.data <- solar.data.dt[area == "RTO" & HE == 3,solar_generation_mw]
#DEFINE frequency
solar_ts <- ts(test.data, frequency = 24)
#CLEAN data for outliers
solar_ts_clean <- tsclean(solar_ts)
#DECOMPOSE observed data into trend, seasonal, and random components 
solar_ts_decompose <- decompose(solar_ts_clean, type ="add")
plot(solar_ts_decompose)

#ANALYZE Random component of solar generation 
solar_random <- solar_ts_decompose$random %>% as.data.table
solar_random <- solar_random[!is.na(x),]
auto.arima(solar_random)
  adf.test(solar_random)
  acf(solar_random)
  pacf(solar_random)

