library(shiny)
shinyUI(
  #--format of the web application interface
  pageWithSidebar(
    #--Application title ---------------------
    headerPanel("Diabetes prediction"),
    
    #-- Format of the sidebar panel ----------
    
    sidebarPanel(
      # The default value for the input is 50
      numericInput('glucose', 'Glucose mg/dl', 90, min=50, max=200, step = 5),
      submitButton('Submmit')
    ),
    
    #-- Main Panel --------------------------
    mainPanel(
      h3('Results of prediction'),
      h4('You entered'),
      # the "inputValue" label comes from server.R file
      verbatimTextOutput("inputValue"),
      h4('Which resulted in a prediction of '),
      # the "prediction" is also a label which comes from server.R function
      verbatimTextOutput("prediction")
    )
  )
)
