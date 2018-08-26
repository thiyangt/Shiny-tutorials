library(shiny)
library(shiny)
shinyUI(pageWithSidebar(
  
#----- title of my shiny app -----------
  headerPanel("Illustating markup"),
#----- display of the sidebar panel-----
  sidebarPanel(
    h1('Sidebar text'), # comma after every h label
    h1('H1 text'), 
    h2('H2 text'), 
    h3('H3 text'), 
    h4('H4 text')
  ),
#----- display of the main panel--------
  mainPanel(
    h3('Main Panel text') ,
    code('some code'),
    p('some ordinary text')
  )
#---------------------------------------
))