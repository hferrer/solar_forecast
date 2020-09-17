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
solar.data.dt[ ,":=" (Date = date(datetime_beginning_ept),
                      HE = hour(datetime_beginning_ept)+1)]


