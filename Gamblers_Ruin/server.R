# --------------------------
#  App Title: Gambler's Ruin
#     Author: Peter Chi
# --------------------------

library(shiny)
source("ruin.R")

  
shinyServer(function(input, output, session) {
    
  all.out<-reactive({
    input$go
    withProgress(session, {
      setProgress(message = "Calculating, please wait.",
                  detail = " ", value=1)
      
    isolate(run.lots(input$fortuneA,input$fortuneB,input$probA,input$num))
  })
  })
  output$ruinPlot <- renderPlot({
    input$go
    isolate(plot.ruin(all.out()$last.run,input$fortuneA,input$fortuneB))
  })
  output$summary <- renderText({
    input$go
    isolate(paste("Number of games run: ",input$num,sep="", "\n",
          
          "Number of games won by Player A: ",all.out()$playerA.wins," (percentage=",
          round(all.out()$playerA.wins/input$num*100,digits=2),"%)", "\n",
          
          "Average game length: ",round(all.out()$average,digits=2), " turns"))
  })
  
})
