library(shiny)
shinyUI(pageWithSidebar(
  headerPanel("Hello Shine!"),
  
  sidebarPanel(
    textInput(inputId="text1", label="Input Text1"),
    textInput(inputId="text2", label="Input Text2"),
    # Input idea is `goButton, the label above the button is Go!`
    actionButton("goButton", "Go!")
  ),
  mainPanel(
    p('Output text1'),
    textOutput('text1'),
    p('Output text2'),
    textOutput('text2'),
    p('Output text3'),
    textOutput('text3')
  )
))
