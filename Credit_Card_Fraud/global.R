# libraries used
#####
# library(aws.s3)
library(data.table) # fast read files
library(dplyr) # data manipulation
library(DT) # render data tables
library(RCurl) # used to download s3 data file
library(shiny) # required
library(shinydashboard) # dashboard look

#####

# data read
#####
# dataset = fread(file = "www/dataset/creditcard.csv", sep = ",", header = TRUE)
# dataset = fread(file = "https://cloudcomputinggroup10data.s3.us-east-2.amazonaws.com/creditcardraw.csv", sep = ",", header = TRUE)
download = getURL(url = "https://cloudcomputinggroup10data.s3.us-east-2.amazonaws.com/creditcardraw.csv?accessType=DOWNLOAD")
dataset = fread(text = download)
datasetFormatted = dataset[,c(1,31,30,2:29)] %>% round(digits = 2)
#####

# global variables
#####
APP_WIDTH = 1400
APP_HEIGHT = 800