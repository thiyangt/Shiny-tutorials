library(shiny)
shinyUI(pageWithSidebar(
  ############### body ################
  #----- title of my shiny app -----------
  headerPanel("Illustating inputs"),
  #----- display of the sidebar panel-----
  sidebarPanel(
    #........................................
    numericInput('id1','Numeric input, labeled id1', 0, min=0, max=10, step=1),
    ## You will get a dropdown list where min is 0 and max is 10, increment by 1,
    ## id1: is the input label name
    ## "Numeric input, labeled id1" is the display label
    
    #........................................
    checkboxGroupInput("id2", "Checkbox",
                       c("Value 1" = "1",
                         "Value 2" = "2",
                         "Value 3" = "3")),
    ## Here you'll get a checkbox to include inputs
    ## id2: label name
    ## Checkbox: display label
    #........................................
    dateInput("date", "Date:")
  ),
  ## Here you'll get a blank box to include the date
  ## date: label name
  ## Date: display label
  #----- display of the main panel--------
  mainPanel(
    h3('Illustrating outputs'),
    h4('You entered'),
    # "id1" is the label you've given above, then you have to put "o" infront of the label "oid1"
    verbatimTextOutput("oid1"),
    h4('You entered'),
    verbatimTextOutput("oid2"),
    h4('You entered'),
    verbatimTextOutput("odate")
   )
  #---------------------------------------
  ##########################################
  
))