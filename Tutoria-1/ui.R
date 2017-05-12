
library(shiny)


# Define UI for iris application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Using Shiny with the iris dataset"),
  
  # Sidebar with controls to select the variable to plot against mpg
  # and to specify whether outliers should be included
  sidebarPanel(
    selectInput("variable", "First variable:",
                list("Sepal length" = "Sepal.Length",
                     "Sepal width"  = "Sepal.Width",
                     "Petal length" = "Petal.Length",
                     "Petal width"  = "Petal.Width")),
    
    selectInput("variable2", "Second variable:",
                list("Sepal length" = "Sepal.Length",
                     "Sepal width"  = "Sepal.Width",
                     "Petal length" = "Petal.Length",
                     "Petal width"  = "Petal.Width"))
  ),
  
  mainPanel(
    h3(textOutput("caption")),
    plotOutput("plot")
  )
))