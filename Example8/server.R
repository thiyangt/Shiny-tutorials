shinyServer(
  function(input, output){
    # As soon as you enter text 1 it will display text 1
    output$text1 <- renderText({input$text1})
    # As soon as you enter text 2 it will display text 2
    output$text2 <- renderText({input$text2})
    # It will print text 1 and text 2 together only when you hit the go button
    output$text3 <- renderText({
      input$goButton
      isolate(paste(input$text1, input$text2))
    })
  }
)