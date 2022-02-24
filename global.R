
rm(list = ls(all.names = TRUE)); # activate the step before execution !!!!!!
cat("\f");

# setting system local encoding
Sys.setlocale("LC_ALL", "English_United States.932") # this works perfectly ---> für japanese characters

# setting some options
options(stringsAsFactors = FALSE)
options(Encoding = "latin1")
options(scipen = 999)

# libraries ----
library(data.table)
library(dplyr)

# Constructing path of relevant directories
root <- getwd()
root
path_data <- paste(root, "/", "data", sep="")
path_data

# defining parameters
lib_location <- .libPaths()[1]


file.data <- paste(path_data, "/transaction_data.csv", sep="")

system.time(transaction_data <- read.csv(file = file.data))

transaction_data$only_date <- as.Date(transaction_data$only_date, format = "%Y-%m-%d")
transaction_data$month_ <-as.numeric(transaction_data$month_)
transaction_data$year_ <- as.numeric(transaction_data$year_)
str(transaction_data)

