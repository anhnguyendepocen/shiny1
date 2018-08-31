# ----------------------------
#  App Title: LCO CI Generator
#     Author: Jimmy Doi
# ----------------------------

library(shiny)

source("LCO_generator_all_n.r")

shinyServer(function(input, output, session) {

  output$textlevel <- renderText({
    paste0("Level = ", input$level,"%")
    })

  output$textsize <- renderText({
    paste("Sample size =", input$size)
    })

  output$textdp <- renderText({
    paste(input$dpsize,"decimal place accuracy")
    })

  
    
    output$LCOresults <- renderPrint({

    lev <- input$level/100
    dp <- switch(input$dpsize,
                   "2nd" = 2,
                   "3rd" = 3,
                   "4th" = 4)
    sz <- input$size

      LCO.CI(sz,lev,dp)
    
    })
})
