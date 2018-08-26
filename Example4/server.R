# This is the prediction function
# prediction function is written outside of the shinyServer
diabetesRisk <- function(glucose) glucose/200

shinyServer(
  #---- Write the function to pass inputs and outputs
  function(input, output){
    output$inputValue <- renderPrint({input$glucose})
    output$prediction <- renderPrint({diabetesRisk(input$glucose)})
  }
  
)