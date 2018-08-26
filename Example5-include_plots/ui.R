shinyUI(pageWithSidebar(
  headerPanel("Example plot"),
  sidebarPanel(
    sliderInput('mu', 'Guess at the mean', value=70, min=62, max=74, step=0.05)
  ),
  mainPanel(
    # newHist is the name that I've given in the server.R file to create the corresponding histogram
    plotOutput('newHist')
  )
))