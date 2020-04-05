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
                         text = "Decision Tree",
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
                         tabName = "eda"
                       ),
                       tabItem(
                         tabName = "architecture",
                         imageOutput(outputId = "image_architecture")
                       ),
                       tabItem(
                         tabName = "sagemaker"
                       ),
                       tabItem(
                         tabName = "decisionTree"
                       ),
                       tabItem(
                         tabName = "logisticRegression",
                         #temp
                         #####
                         column(width = 6,
                                uiOutput(outputId = "modelVisual2_1"),
                                uiOutput(outputId = "modelVisual2_2")
                         ),
                         column(width = 6,
                                sliderInput(inputId = "x1", label = "X1", min = 0, max = 0.3, value = 0.15, step = 0.01),
                                sliderInput(inputId = "x2", label = "X2", min = 0, max = 50, value = 25, step = 1),
                                sliderInput(inputId = "x3", label = "X3", min = 0, max = 50, value = 25, step = 1),
                                uiOutput(outputId = "logOdds")
                         )
                         #####
                       ),
                       tabItem(
                         tabName = "summary",
                         a("Link to code (TBD)",
                           href = NULL, target = "_blank")
                       )
                     )
                   )
)


server = function(input, output) {
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
    datasetFormatted
  })
  
  # output$image_architecture = renderImage(expr = {
  #   list(
  #     src = paste0("www/images/architecture.png"),
  #     contentType = "image/png",
  #     width = APP_WIDTH,
  #     height = APP_HEIGHT
  #   )
  # }, deleteFile = FALSE)
  
  # temp
  #####
  output$modelVisual2_1 = renderUI(expr = {
    withMathJax(
      "$$y_i = malignance \\, of \\, cancer \\, case \\, i$$",
      "$$x_{1,i} = max \\, concave \\, points \\, of \\, cancer \\, case \\, i$$",
      "$$x_{2,i} = texture \\, of \\, cancer \\, case \\, i \\, (grayscale \\, deviation)$$",
      "$$x_{3,i} = max \\, radius \\, of \\, cancer \\, case \\, i$$",
      "$$logistic(x) = \\frac{1}{1 + e^{-x}}$$"
    )
  })
  
  output$modelVisual2_2 = renderUI(expr = {
    withMathJax(
      "$$\\hat{\\beta}_0 = -34.58$$",
      "$$\\hat{\\beta}_1 = 57.71$$",
      "$$\\hat{\\beta}_2 = 0.306$$",
      "$$\\hat{\\beta}_3 = 1.154$$",
      "$$\\hat{\\alpha} = 0.0001$$"
    )
  })
  
  output$logOdds = renderUI(expr = {
    withMathJax(
      sprintf(
        "$$x = \\hat{\\beta}_0 + %.2f  \\hat{\\beta}_1 + %.0f  \\hat{\\beta}_2 + %.0f  \\hat{\\beta}_3 = %.2f$$",
        input$x1, input$x2, input$x3, -34.58+57.71*input$x1+0.306*input$x2+1.154*input$x3
      ),
      sprintf(
        "$$\\hat{\\mu} = \\frac{1-\\alpha}{1+e^{-x}} + \\frac{1}{2}\\alpha = %.2f$$",
        (1-0.0001)/(1+exp(-(-34.58+57.71*input$x1+0.306*input$x2+1.154*input$x3))) + 0.5*0.0001
      ),
      sprintf(
        "$$y=%.0f$$",
        if((1-0.0001)/(1+exp(-(-34.58+57.71*input$x1+0.306*input$x2+1.154*input$x3))) + 0.5*0.0001 > 0.5) {
          1
        } else {
          0
        }
      )
    )
  })
  #####
}

shinyApp(ui, server)
