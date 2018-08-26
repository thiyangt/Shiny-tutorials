library(shiny)
shinyUI(pageWithSidebar(
  headerPanel("Data science FTWI"),
  sidebarPanel(
    h3('Sidebar text')
  ),
  mainPanel(
    h3('Main Panel text') # h3: 3rd level html heading
  )
))