#library to deploy app: rsconnect
#command to deploy app: deployApp(appDir = "C:\\Users\\derek.funk\\Documents\\MSDS\\6 - Spring 2020\\Cloud Computing\\Final Project\\Shiny App\\Credit_Card_Fraud", account = "derek-funk")

source("global.R")

ui = dashboardPage(skin = "black",
                   header = dashboardHeader(
                     title = "Fraud Detection in AWS Sagemaker"
                     
                   ),
                   sidebar = dashboardSidebar(
                     sidebarMenu(
                       menuItem(
                         text = "Introduction",
                         tabName = "introduction",
                         icon = icon("credit-card")
                       ),
                       menuItem(
                         text = "Problem Statement",
                         tabName = "problem",
                         icon = icon("question")
                       ),
                       menuItem(
                         text = "Dataset",
                         tabName = "dataset",
                         icon = icon("table")
                       ),
                       menuItem(
                         text = "EDA",
                         tabName = "eda",
                         icon = icon("chart-bar")
                       ),
                       menuItem(
                         text = "AWS Architecture",
                         tabName = "architecture",
                         icon = icon("aws")
                       ),
                       menuItem(
                         text = "Sagemaker",
                         tabName = "sagemaker",
                         icon = icon("brain")
                       ),
                       menuItem(
                         text = "Random Forest",
                         tabName = "decisionTree",
                         icon = icon("bezier-curve")
                       ),
                       menuItem(
                         text = "Logistic Regression",
                         tabName = "logisticRegression",
                         icon = icon("check-square")
                       ),
                       menuItem(
                         text = "Summary",
                         tabName = "summary",
                         icon = icon("flag")
                       )
                     )
                   ),
                   body = dashboardBody(
                     tags$style("
      .main-header .navbar {
          margin-left: 400px;
      }
      
      .main-header .logo {
          width: 400px;
      }
    "),
                     tabItems(
                       tabItem(
                         tabName = "introduction",
                         imageOutput(outputId = "image_introduction")
                       ),
                       tabItem(
                         tabName = "problem",
                         imageOutput(outputId = "image_problem")
                       ),
                       tabItem(
                         tabName = "dataset",
                         h1("Dataset"), br(), br(),
                         fluidRow(
                           box(
                             h2("Dataset source"),
                             h4(
                               "Defeat Fraud Credit Card Dataset from Unversite Libre de Bruxelles found at:", br(),
                               a("https://www.kaggle.com/mlg-ulb/creditcardfraud",
                                 href = "https://www.kaggle.com/mlg-ulb/creditcardfraud", target = "_blank")
                             )
                           ),
                           box(
                             h2("Dataset information"),
                             h4(tags$ul(
                               tags$li("Contains credit card transactions"),
                               tags$li("300,000 transactions"),
                               tags$li("500 ~ frauds (0.172%)"),
                               tags$li("September 2013 - Transaction history of two unique days"),
                               tags$li("Contains only numerical data due to PCA Transformation"),
                               tags$li("~30 Features are masked (i.e. v1,v2,v3..)"),
                               tags$li('Only features not transformed are "Time" and "Amount"'),
                               tags$li('Feature "Class" is binary and represents fraud')
                             ))
                           )
                         ), br(), br(),
                         fluidRow(
                           DT::dataTableOutput(outputId = "dataTable")
                         )
                       ),
                       tabItem(
                         tabName = "eda",
                         tabsetPanel(
                           tabPanel(
                             title = "Correlations",
                             uiOutput(outputId = "image_correlationMatrix")
                           ),
                           tabPanel(
                             title = "Distributions",
                             uiOutput(outputId = "image_overTime") %>% withSpinner(type = 6, color = "#76B7D2"),
                             uiOutput(outputId = "image_classDistribution") %>% withSpinner(type = 6, color = "#76B7D2")
                           )
                         )
                       ),
                       tabItem(
                         tabName = "architecture",
                         uiOutput(outputId = "image_architecture")
                       ),
                       tabItem(
                         tabName = "sagemaker",
                         tabsetPanel(
                           tabPanel(
                             title = "Precision & Recall",
                             uiOutput(outputId = "image_recall") %>% withSpinner(type = 6, color = "#76B7D2"),
                             uiOutput(outputId = "image_recallAndPrecision") %>% withSpinner(type = 6, color = "#76B7D2")
                           )
                         )
                       ),
                       tabItem(
                         tabName = "decisionTree",
                         tabsetPanel(
                           tabPanel(
                             title = "Model Validation",
                             column(
                               width = 8,
                               uiOutput(outputId = "image_randomForest_confusionMatrix")
                             ),
                             column(
                               width = 4,
                               HTML("
                <h4>Accuracy Score = 0.999438</h4><br>   
                <h4>Precision Score = 0.934211</h4><br>
                <h4>Recall Score = 0.72449</h4><br>
                <h4>F1 Score = 0.816092</h4><br>
              "),
                               br(),
                               uiOutput(outputId = "image_randomForest_roc")
                             )
                           ),
                           tabPanel(
                             title = "Individual Trees",
                             selectInput(inputId = "treeNumber", label = "Decision Tree", choices = 0:99),
                             uiOutput(outputId = "image_decisionTrees",
                                      style = "height:800px; width:1400px; overflow-y: scroll;overflow-x: scroll;")
                           )
                         )
                       ),
                       tabItem(
                         tabName = "logisticRegression",
                         tabsetPanel(
                           tabPanel(
                             title = "Model Validation",
                             column(
                               width = 8,
                               uiOutput(outputId = "image_logistic_confusionMatrix")
                             ),
                             column(
                               width = 4,
                               HTML("
                <h4>Accuracy Score = 0.928934</h4><br>   
                <h4>Precision Score = 0.966667</h4><br>
                <h4>Recall Score = 0.887755</h4><br>
                <h4>F1 Score = 0.925532</h4><br>
              "),
                               br(),
                               uiOutput(outputId = "image_logistic_roc")
                             )
                           ),
                           tabPanel(
                             title = "Estimate Tool",
                             fluidRow(
                               box(
                                 title = "Logistic Regression Overview",
                                 width = 6,
                                 height = "400px",
                                 uiOutput(outputId = "logistic_overview")
                               ),
                               box(
                                 title = "Parameters",
                                 width = 6,
                                 height = "400px",
                                 # br(),
                                 DT::dataTableOutput(outputId = "dataTable_logisticCoefficients")
                               )
                             ),
                             fluidRow(
                               box(
                                 title = "Estimates",
                                 width = 6,
                                 height = "450px",
                                 column(
                                   width = 2,
                                   lapply(1:5, function(i) {
                                     numericInput(inputId = paste0("logistic_input_V", i), label = paste0("V", i), value = round(VARIABLE_MEANS[i], digits = 3), step = 0.1)
                                   })
                                 ),
                                 column(
                                   width = 2,
                                   lapply(6:10, function(i) {
                                     numericInput(inputId = paste0("logistic_input_V", i), label = paste0("V", i), value = round(VARIABLE_MEANS[i], digits = 3), step = 0.1)
                                   })
                                 ),
                                 column(
                                   width = 2,
                                   lapply(11:15, function(i) {
                                     numericInput(inputId = paste0("logistic_input_V", i), label = paste0("V", i), value = round(VARIABLE_MEANS[i], digits = 3), step = 0.1)
                                   })
                                 ),
                                 column(
                                   width = 2,
                                   lapply(16:20, function(i) {
                                     numericInput(inputId = paste0("logistic_input_V", i), label = paste0("V", i), value = round(VARIABLE_MEANS[i], digits = 3), step = 0.1)
                                   })
                                 ),
                                 column(
                                   width = 2,
                                   lapply(21:25, function(i) {
                                     numericInput(inputId = paste0("logistic_input_V", i), label = paste0("V", i), value = round(VARIABLE_MEANS[i], digits = 3), step = 0.1)
                                   })
                                 ),
                                 column(
                                   width = 2,
                                   lapply(26:28, function(i) {
                                     numericInput(inputId = paste0("logistic_input_V", i), label = paste0("V", i), value = round(VARIABLE_MEANS[i], digits = 3), step = 0.1)
                                   }),
                                   numericInput(inputId = "logistic_input_amount", label = "Amount", value = round(VARIABLE_MEANS[29], digits = 3)),
                                   br(), actionBttn(inputId = "logistic_reset", label = "Reset", color = "success")
                                 )
                               ),
                               box(
                                 title = "Results",
                                 column(
                                   width = 4,
                                   uiOutput(outputId = "logistic_result"),
                                   selectInput(inputId = "logistic_fixVariable", label = "Regressor to Fix", choices = LOGISTIC_FIXED_VARIABLE_CHOICES,
                                               selected = 1)
                                 ),
                                 column(
                                   width = 8,
                                   plotOutput(outputId = "logistic_plot")
                                 )
                               )
                             )
                           )
                         )
                       ),
                       tabItem(
                         tabName = "summary",
                         a("Link to code (TBD)",
                           href = NULL, target = "_blank")
                       )
                     )
                   )
)


server = function(input, output, session) {
  output$image_introduction = renderImage(expr = {
    list(
      src = paste0("www/images/introduction.png"),
      contentType = "image/png",
      width = APP_WIDTH,
      height = APP_HEIGHT
    )
  }, deleteFile = FALSE)
  
  output$image_problem = renderImage(expr = {
    list(
      src = paste0("www/images/problem.png"),
      contentType = "image/png",
      width = APP_WIDTH,
      height = APP_HEIGHT
    )
  }, deleteFile = FALSE)
  
  output$dataTable = DT::renderDataTable(expr = {
    DATASET_TRANSACTIONS
  })
  
  output$image_correlationMatrix = renderUI(expr = {
    imageUrl = "https://cloudcomputinggroup10data.s3.us-east-2.amazonaws.com/sagemaker/figures/corrmatrix.png"
    
    tags$img(src = imageUrl, alt = "Image not available at this time!")
  })
  
  output$image_overTime = renderUI(expr = {
    # imageSource = "https://cloudcomputinggroup10data.s3.us-east-2.amazonaws.com/sagemaker/figures/plotly_figures/EDA/Transactions+Over+Time%3A+Legitimate+and+Fraudulent.html"
    # tags$img(src = imageSource)
    includeHTML(path = "https://cloudcomputinggroup10data.s3.us-east-2.amazonaws.com/sagemaker/figures/plotly_figures/EDA/Transactions+Over+Time%3A+Legitimate+and+Fraudulent.html")
  })
  
  output$image_classDistribution = renderUI(expr = {
    includeHTML(path = "https://cloudcomputinggroup10data.s3.us-east-2.amazonaws.com/sagemaker/figures/plotly_figures/EDA/Distribution+of+Transaction+Amounts%3A+Legitimate+and+Fradulent.html")
  })
  
  output$image_architecture = renderUI(expr = {
    imageUrl = "https://cloudcomputinggroup10data.s3.us-east-2.amazonaws.com/Cloud+Infrastructure+Updated.PNG"
    
    tags$img(src = imageUrl, alt = "Image not available at this time!")
  })
  
  output$image_recall = renderUI(expr = {
    includeHTML(path = "https://cloudcomputinggroup10data.s3.us-east-2.amazonaws.com/sagemaker/figures/plotly_figures/precision-recall/Recall%2BPrecision_2.html")
  })
  
  output$image_recallAndPrecision = renderUI(expr = {
    includeHTML(path = "https://cloudcomputinggroup10data.s3.us-east-2.amazonaws.com/sagemaker/figures/plotly_figures/precision-recall/Recall%2BPrecision_1.html")
  })
  
  output$image_randomForest_confusionMatrix = renderUI(expr = {
    imageUrl = "https://cloudcomputinggroup10data.s3.us-east-2.amazonaws.com/sagemaker/figures/ranfor_confmatrix.png"
    
    tags$img(src = imageUrl, alt = "Image not available at this time!")
  })
  
  output$image_randomForest_roc = renderUI(expr = {
    imageUrl = "https://cloudcomputinggroup10data.s3.us-east-2.amazonaws.com/sagemaker/figures/Random_Forest_ROC_Curve.png"
    
    tags$img(src = imageUrl, alt = "Image not available at this time!")
  })
  
  output$image_decisionTrees = renderUI(expr = {
    imageUrl = paste0("https://cloudcomputinggroup10data.s3.us-east-2.amazonaws.com/sagemaker/figures/creditcard_",
                      input$treeNumber, ".png")
    
    tags$img(src = imageUrl, alt = "Image not available at this time!")
  })
  
  output$image_logistic_confusionMatrix = renderUI(expr = {
    imageUrl = "https://cloudcomputinggroup10data.s3.us-east-2.amazonaws.com/sagemaker/figures/logreg_confmatrix.png"
    
    tags$img(src = imageUrl, alt = "Image not available at this time!")
  })
  
  output$image_logistic_roc = renderUI(expr = {
    imageUrl = "https://cloudcomputinggroup10data.s3.us-east-2.amazonaws.com/sagemaker/figures/Logistic_ROC_Curve.png"
    
    tags$img(src = imageUrl, alt = "Image not available at this time!")
  })
  
  output$logistic_overview = renderUI(expr = {
    withMathJax(
      "$$k = \\# \\, variables$$",
      "$$\\mu = probability \\, of \\, fraud$$",
      "$$y = fraud \\, estimate \\, (1 = fraud, \\, 0 = not \\, fraud)$$",
      "$$variables: X_1,...,X_k$$",
      "$$coefficients: \\beta_0,\\beta_1,...,\\beta_k$$",
      "$$log \\, odds \\, ratio: ln\\left(\\frac{\\mu}{1-\\mu}\\right) = \\beta_0 + \\sum_{j=1}^k \\beta_j X_j = x$$",
      "$$\\mu = \\frac{1}{1+e^{-x}}$$",
      "$$y = \\begin{cases} 1, \\, \\mu\\geq0.5 \\\\ 0, \\, \\mu<0.5 \\end{cases}$$"
    )
  })
  
  output$dataTable_logisticCoefficients = renderDataTable(expr = {
    datatable(DATASET_LOGISTIC_COEFFICIENTS %>% mutate(Coefficient = round(Coefficient, digits = 3)),
              rownames = NULL,
              options = list(
                pageLength = 7,
                searching = FALSE,
                lengthChange = FALSE
              )
    )
  })
  
  observeEvent(eventExpr = input$logistic_reset, handlerExpr = {
    
    lapply(1:28, function(i) {
      updateNumericInput(session = session, inputId = paste0("logistic_input_V", i), value = round(VARIABLE_MEANS[i][[1]], digits = 3))
    })
    
    updateNumericInput(session = session, inputId = "logistic_input_amount", value = round(VARIABLE_MEANS[29][[1]], digits = 3))
  })
  
  output$logistic_result = renderUI(expr = {
    logit_estimate = LOGISTIC_BETA_INTERCEPT
    for(i in 1:28) {
      logit_estimate = logit_estimate + DATASET_LOGISTIC_COEFFICIENTS$Coefficient[i] * input[[paste0("logistic_input_V", i)]]
      # print(input[[paste0("logistic_input_V", i)]])
    }
    logit_estimate = logit_estimate + DATASET_LOGISTIC_COEFFICIENTS$Coefficient[29] * input[["logistic_input_amount"]]
    mu_estimate = 1/(1+exp(-logit_estimate))
    fraud_estimate = if(is.na(mu_estimate)) {
      NA
    } else if(mu_estimate>=0.5) {
      1
    } else {
      0
    }
    
    withMathJax(
      sprintf(
        "$$\\hat \\mu = %.3f$$",
        mu_estimate
      ),
      sprintf(
        "$$\\hat y = %i$$",
        fraud_estimate
      )
    )
  })
  
  output$logistic_plot = renderPlot(expr = {
    if(input$logistic_fixVariable == "29") {
      x_coord = -5000:5000
      logit_estimate = LOGISTIC_BETA_INTERCEPT
      for(i in 1:28) {
        logit_estimate = logit_estimate + DATASET_LOGISTIC_COEFFICIENTS$Coefficient[i] * input[[paste0("logistic_input_V", i)]]
      }
      logit_estimate = logit_estimate + DATASET_LOGISTIC_COEFFICIENTS$Coefficient[29] * x_coord
      mu_estimate = 1/(1+exp(-logit_estimate))
      
      ggplot(mapping = aes(x = x_coord, y = mu_estimate)) +
        geom_line() +
        scale_x_continuous(breaks = -5:5*1000, labels = -5:5*1000) +
        scale_y_continuous(breaks = 0:10/10, labels = 0:10/10) +
        xlab(label = "Amount") +
        ylab(label = "Probability of Fraud")
    } else {
      x_coord = -100:100
      logit_estimate = LOGISTIC_BETA_INTERCEPT
      for(i in 1:28) {
        if(as.numeric(input$logistic_fixVariable) == i) {
          NULL
        } else {
          logit_estimate = logit_estimate + DATASET_LOGISTIC_COEFFICIENTS$Coefficient[i] * input[[paste0("logistic_input_V", i)]]
        }
      }
      logit_estimate = logit_estimate + DATASET_LOGISTIC_COEFFICIENTS$Coefficient[29] * input[["logistic_input_amount"]]
      logit_estimate = logit_estimate + DATASET_LOGISTIC_COEFFICIENTS$Coefficient[as.numeric(input$logistic_fixVariable)] * x_coord
      mu_estimate = 1/(1+exp(-logit_estimate))
      
      ggplot(mapping = aes(x = x_coord, y = mu_estimate)) +
        geom_line() +
        scale_x_continuous(breaks = -5:5*20, labels = -5:5*20) +
        scale_y_continuous(breaks = 0:10/10, labels = 0:10/10) +
        xlab(label = paste0("V", input$logistic_fixVariable)) +
        ylab(label = "Probability of Fraud")
    }
    
  })
  
  # observeEvent(eventExpr = input$logistic_fixVariable, handlerExpr = {
  #   print(input$logistic_fixVariable)
  # })
}

shinyApp(ui, server)
