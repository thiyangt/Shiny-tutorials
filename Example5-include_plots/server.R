# First the dataset I need to grab
library(UsingR)
data(galton)

# now write the content of server.R
shinyServer(
  # define the function of inputs and outputs ------
  function(input, output){
    # I give the name to my histogram as "newHist", this name is used in ui.R
    output$newHist <- renderPlot({
      hist(galton$child, xlab="child height", col="lightblue",          main="Histogram")
      mu <- input$mu
      lines(c(mu,mu), c(0, 200), col="red", led=5)
      mse <- mean((galton$child-mu)^2)
      text(63, 150, paste("mu = ", mu))
      text(63, 140, paste("MSE = ", round(mse, 2)))
    })
    
  }
)
