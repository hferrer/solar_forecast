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

#BUILD DATA

solar.data.folder <- "Data/"