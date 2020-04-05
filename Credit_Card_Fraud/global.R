# libraries used
#####
library(data.table) # fast read files
library(dplyr) # data manipulation
library(DT) # render data tables
library(shiny) # required
library(shinydashboard) # dashboard look
#####

# data read
#####
# dataset = read.csv(file = "www/dataset/creditcard.csv", header = TRUE)
dataset = fread(file = "www/dataset/creditcard.csv", sep = ",", header = TRUE)
datasetFormatted = dataset[,c(1,31,30,2:29)] %>% round(digits = 2)
#####

# global variables
#####
APP_WIDTH = 1400
APP_HEIGHT = 800
