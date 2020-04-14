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

LOGISTIC_FIXED_VARIABLE_CHOICES = 1:29
names(LOGISTIC_FIXED_VARIABLE_CHOICES) = c(paste0("V", 1:28), "Amount")

LOGISTIC_BETA_INTERCEPT = DATASET_LOGISTIC_COEFFICIENTS$Coefficient[30]

SAGEMAKER_SLIDE_IMAGE_NAMES = c(
  "https://cloudcomputinggroup10data.s3.us-east-2.amazonaws.com/sagemaker/presentation/tab1/Slide1.PNG",
  "https://cloudcomputinggroup10data.s3.us-east-2.amazonaws.com/sagemaker/presentation/tab1/Slide2.PNG",
  "https://cloudcomputinggroup10data.s3.us-east-2.amazonaws.com/sagemaker/presentation/tab1/Slide3.PNG",
  "https://cloudcomputinggroup10data.s3.us-east-2.amazonaws.com/sagemaker/presentation/tab2/Slide4.PNG",
  "https://cloudcomputinggroup10data.s3.us-east-2.amazonaws.com/sagemaker/presentation/tab2/Slide5.PNG",
  "https://cloudcomputinggroup10data.s3.us-east-2.amazonaws.com/sagemaker/presentation/tab2/Slide6.PNG",
  "https://cloudcomputinggroup10data.s3.us-east-2.amazonaws.com/sagemaker/presentation/tab2/Slide7.PNG",
  "https://cloudcomputinggroup10data.s3.us-east-2.amazonaws.com/sagemaker/presentation/tab2/Slide8.PNG",
  "https://cloudcomputinggroup10data.s3.us-east-2.amazonaws.com/sagemaker/presentation/tab2/Slide9.PNG",
  "https://cloudcomputinggroup10data.s3.us-east-2.amazonaws.com/sagemaker/presentation/tab3/Slide10.PNG",
  "https://cloudcomputinggroup10data.s3.us-east-2.amazonaws.com/sagemaker/presentation/tab3/Slide11.PNG",
  "https://cloudcomputinggroup10data.s3.us-east-2.amazonaws.com/sagemaker/presentation/tab3/Slide12.PNG",
  "https://cloudcomputinggroup10data.s3.us-east-2.amazonaws.com/sagemaker/presentation/tab3/Slide13.PNG",
  "https://cloudcomputinggroup10data.s3.us-east-2.amazonaws.com/sagemaker/presentation/tab3/Slide14.PNG",
  "https://cloudcomputinggroup10data.s3.us-east-2.amazonaws.com/sagemaker/presentation/tab3/Slide15.PNG",
  "https://cloudcomputinggroup10data.s3.us-east-2.amazonaws.com/sagemaker/presentation/tab4/Slide16.PNG",
  "https://cloudcomputinggroup10data.s3.us-east-2.amazonaws.com/sagemaker/presentation/tab4/Slide17.PNG",
  "https://cloudcomputinggroup10data.s3.us-east-2.amazonaws.com/sagemaker/presentation/tab4/Slide18.PNG",
  "skip",
  "skip",
  "https://cloudcomputinggroup10data.s3.us-east-2.amazonaws.com/sagemaker/presentation/tab4/Slide21.PNG",
  "https://cloudcomputinggroup10data.s3.us-east-2.amazonaws.com/sagemaker/presentation/tab4/Slide22.PNG"
)

SUMMARY_MODELS = data.frame(
  "Metric" = c("Best Accuracy","Best Precision","Best F1-Score"),
  "Model" = c("Random Forest","Logistic Regression","Logistic Regression"),
  "Value" = c(0.999438,0.966667,0.925532)
)
