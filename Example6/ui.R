library(shiny)
shinyUI(pageWithSidebar(
  headerPanel("Hello Shiny!"),
  
  #-- sidebar panel
  sidebarPanel(
    textInput(inputId="text1", label="Input Text1"),
    textInput(inputId="text2", label="Input Text2")
  ),
  mainPanel(
    p('Output text1'),
    textOutput('text1'),
    p('Output text2'),
    textOutput('text2'),
    p('Output text3'),
    textOutput('text3'),
    p('Outside text (x is defined out side the function)'),
    textOutput('text4'),
    p('Inside text, but non-reactive (y is defined inside the function)'),
    textOutput('text5')
  )
  
))
