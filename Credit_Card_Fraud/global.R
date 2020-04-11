# libraries used
#####
# library(aws.s3)
library(data.table) # fast read files
library(dplyr) # data manipulation
library(DT) # render data tables
library(ggplot2) # better graphs
library(RCurl) # used to download s3 data file
library(shiny) # required
library(shinycssloaders) # used for loading spinners
library(shinydashboard) # dashboard look
library(shinyWidgets) # fancier ui components

#####

# data reads
#####
download = getURL(url = "https://cloudcomputinggroup10data.s3.us-east-2.amazonaws.com/creditcardraw.csv?accessType=DOWNLOAD")
dataset = fread(text = download)
DATASET_TRANSACTIONS = dataset[,c(1,31,30,2:29)] %>% round(digits = 2)
VARIABLE_MEANS = sapply(X = DATASET_TRANSACTIONS[,-c(1,2)], FUN = mean)
VARIABLE_MEANS = VARIABLE_MEANS[c(2:29,1)]

download = getURL(url = "https://cloudcomputinggroup10data.s3.us-east-2.amazonaws.com/sagemaker/figures/coefficients.csv?accessType=DOWNLOAD")
DATASET_LOGISTIC_COEFFICIENTS = fread(text = download, skip = 1, drop = 1, col.names = c("Variable","Coefficient"))
#####

# global variables
#####
APP_WIDTH = 1400
APP_HEIGHT = 800

x = c(
  "V1" = 1,
  "V2" = 2
)

LOGISTIC_FIXED_VARIABLE_CHOICES = 1:29
names(LOGISTIC_FIXED_VARIABLE_CHOICES) = c(paste0("V", 1:28), "Amount")

LOGISTIC_BETA_INTERCEPT = 0 # update from Spencer
