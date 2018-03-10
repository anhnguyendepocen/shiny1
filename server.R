shinyServer(function(input, output) {
  
  output$start1 <- renderUI ({
    if(input$button3==1 || is.null(input$button3)) st1 <<- NULL
    isolate(if(input$node==1 && values$shouldShow2) st1 <<- input$start)
    if(values$shouldShow2 | values$shouldShow3){
    if(input$node==1) #withMathJax(helpText('$$A^1$$'))
    paste("Starting %A^1: ", st1)
   # expression(paste("Starting","%A"^"1"))
    
    }
  })
  
  output$start2 <- renderText ({
    if(input$button3==1 || is.null(input$button3)) st2 <<- NULL
    if(values$shouldShow3) {
      isolate(if(input$node2==1) st2 <<- input$start)
      if(input$node2==0) {""}
      else if(input$node==1) {
        paste("Starting %A^1: ", st2)}
    }
  })
  
  output$gen1 <- renderText ({
    if(input$button3==1 || is.null(input$button3)) g1 <<- NULL
    if(values$shouldShow2 | values$shouldShow3){
      isolate(if(input$node==1 && values$shouldShow2) g1 <<- input$gen)
      if(input$node==0) {""}
      else if(input$node==1) {
        paste("Number of Generations:", g1)}
    }
  })
  
  output$gen2 <- renderText ({
    if(input$button3==1 || is.null(input$button3)) g2 <<- NULL
    if(values$shouldShow3){
      isolate(if(input$node2==1) g2 <<- input$gen)
      if(input$node2==0) {""}
      else if(input$node2==1 && !is.null(input$node2)) {
        paste("Number of Generations:", g2)}
    }
  })
  
  output$pop1 <- renderText ({
    if(input$button3==1 || is.null(input$button3)) p1 <<- NULL
    if(values$shouldShow2 | values$shouldShow3){
      isolate(if(input$node==1 & values$shouldShow2) p1 <<- input$pop)
      if(input$node==0) {""}
      else if(input$node>=1) {
        paste("Population Size: ", p1)}
    }
  })
  
  output$pop2 <- renderText ({
    if(input$button3==1) p2 <<- NULL
    if(values$shouldShow3){
      isolate(if(input$node2==1) p2 <<- input$pop)
      if(input$node2==0) {""}
      else if(input$node2==1 && !is.null(input$node2)) {
        paste("Population Size: ", p2)}
    }
  })
  
  output$AA1 <- renderText ({
    if(input$button3==1 || is.null(input$button3)) A1 <<- NULL
    if(values$shouldShow2 | values$shouldShow3){
      isolate(if(input$node==1 & values$shouldShow2) A1 <<- input$AA)
      if(input$node==0) {""}
      else if(input$node>=1) {
        #withMathJax(helpText('A/A'))
        paste("A^1/A^1 Fitness %:", A1)
        #expression(paste("A"^"1"))
      }
    }
  })
  
  
  output$AA2 <- renderText ({
    if(input$button3==1 || is.null(input$button3)) A2 <<- NULL
    if(values$shouldShow3){
      isolate(if(input$node2==1) A2 <<- input$AA)
      if(input$node2==0) {""}
      else if(input$node2==1 && !is.null(input$node2)) {
        paste("A^1/A^1 Fitness %:", A2)}
    }
  })
  
  output$AB1 <- renderText ({
    if(input$button3==1 || is.null(input$button3)) AB_1 <<- NULL
    if(values$shouldShow2 | values$shouldShow3) {
      isolate(if(input$node==1 & values$shouldShow2) AB_1 <<- input$AB)
      if(input$node==0) {""}
      else if(input$node>=1) {
#         tags$div(
#           HTML(paste("A", tags$sup(1), sep = "")), 
#           HTML(paste("A", tags$sup(2), sep = "")),
#           HTML(paste("Fitness %"))
#         )
        paste("A^1/A^2 Fitness %:", AB_1)
      }
    }
  })
  
  output$AB2 <- renderText ({
    if(input$button3==1 || is.null(input$button3)) AB_2 <<- NULL
    if(values$shouldShow3){
      isolate(if(input$node2==1 && !is.null(input$node2)) AB_2 <<- input$AB)
      if(input$node2==0) {""}
      else if(input$node2==1) {
        paste("A^1/A^2 Fitness %:", AB_2)}
    }
  })
  
  output$BB1 <- renderText ({
    if(input$button3==1 || is.null(input$button3)) B1 <<- NULL
    if(values$shouldShow2 | values$shouldShow3){
      isolate(if(input$node==1 & values$shouldShow2) B1 <<- input$BB)
      if(input$node==0) {""}
      else if(input$node>=1) {
        paste("A^2/A^2 Fitness %:", B1)}
    }
  })
  
  
  output$BB2 <- renderText ({
    if(input$button3==1 || is.null(input$button3)) B2 <<- NULL
    if(values$shouldShow3){
      isolate(if(input$node2==1 && !is.null(input$node2)) B2 <<- input$BB)
      if(input$node2==0 || is.null(input$node2)) {""}
      else if(input$node2==1) {
        paste("A^2/A^2 Fitness %:", B2)}
    }
  })
  
  output$Population1 <- renderImage({
    if(values$shouldShow2 | values$shouldShow3)
      
      return(list(
        src="www/population_logo_red.png", contentType="image/png", alt="Population", align='left')) 
    
    
    
    
  }, deleteFile=FALSE) 
  
  output$Population2 <- renderImage({
    if(values$shouldShow3)
      if(input$node2==1) {
        return(list(
          src="www/population_logo_blue.png", contentType="image/png", alt="Population", align='left')) 
      }
    
    
    
  }, deleteFile=FALSE)
  
  
  output$distPlot <- renderPlot({
    
    
    if(input$node==1) {
      sim1 <<- matrix(nrow=g1,ncol=1000)
      sim1[1,] <<- st1
    }
    
    if(input$node2==1) {
      sim2 <<- matrix(nrow=g2,ncol=1000)
      sim2[1,] <<- st2
    }
    
    #labelsY=parse(text=paste("%","A", "^1", sep=""))
    #labelsY=parse(text=paste("A","^1",sep=""))
    title=expression(paste("%A"^"1"," in Population"))
    p <- ggplot()+
      xlab("Generation")+ylab(expression(paste("%A"^"1")))+labs(title=title)+
      ylim(0,100)
    
    
    input$drawpop
    withProgress(message='Generating Simulations',value=0.1, {
      Sys.sleep(0.25) #newcode
    isolate({
      j <<- 1
      for(k in 1:input$drawmorepops){
        incProgress(0.25, detail="Generating Populations")
        for(i in 2:g1) {
          
          allele1 <- (runif(p1)<sim1[i-1,j]/100)
          allele2 <- (runif(p1)<sim1[i-1,j]/100)
          
          ######FOR SURVIVAL %
          if(A1!=100) {
            aa <- (allele1==1 & allele2==1)
            minus.a <- round(sum(aa)*2*(1-(A1/100)))
          } 
          else {
            minus.a <- 0
          }
          if(B1!=100){
            bb <- (allele1==0 & allele2==0)
            minus.b <- round(sum(bb)*2*(1-(B1/100)))
          }
          else {
            minus.b <- 0
          }
          if(AB_1!=100) {
            ab <- ((allele1==1 & allele2==0) | (allele1==0 & allele2==1))
            minus.ab <- round(sum(ab)*(1-(AB_1/100)))
          }
          else {
            minus.ab <- 0
          }
          
          sim1[i,j] <<- ((sum(allele1+allele2)-minus.a-minus.ab)/((p1*2)-minus.a-minus.b-2*minus.ab))*100
        }
        if(input$node2==1) {
          for(i in 2:g2) {
            allele3 <- (runif(p2)<sim2[i-1,j]/100)
            allele4 <- (runif(p2)<sim2[i-1,j]/100)
            if(A2!=100) {
              aa <- (allele3==1 & allele4==1)
              minus.a <- round(sum(aa)*2*(1-(A2/100)))
            } 
            else {
              minus.a <- 0
            }
            if(B2!=100){
              bb <- (allele3==0 & allele4==0)
              minus.b <- round(sum(bb)*2*(1-(B2/100)))
            }
            else {
              minus.b <- 0
            }
            if(AB_2!=100) {
              ab <- ((allele3==1 & allele4==0) | (allele3==0 & allele4==1))
              minus.ab <- round(sum(ab)*(1-(AB_2/100)))
            }
            else {
              minus.ab <- 0
            }
            sim2[i,j] <<- ((sum(allele3+allele4)-minus.a-minus.ab)/((p2*2)-minus.a-minus.b-2*minus.ab))*100
          }
        }
        
        j <<- j+1
      }
    }) #isolate
    incProgress(0.5, detail="Generating Plot")
    y_sim1 <<- melt(sim1[,1:j])
    if(input$node2==1) y_sim2 <<- melt(sim2[,1:j])
    p <- p+geom_line(data = y_sim1, aes(x = Var1, y = value, group = Var2, colour="pop1"))
    if(input$node2==0) {p <- p+  scale_colour_manual(name="Population", values=c(pop1="red"),
                                                     labels=c("Population 1"))}
    if(input$node2==1) {
      p<- p+geom_line(data = y_sim2, aes(x=Var1, y=value, group=Var2, colour="pop2"))+
        scale_colour_manual(name="Population",
                            values=c(pop1="red", pop2="blue"),
                            labels=c("Population 1", "Population 2"))
    }
    print(p)
    })
  }) #renderPlot
  ####TO CLEAR POPULATIONS
  values <- reactiveValues(shouldShow = FALSE)
  values <- reactiveValues(shouldShow2 = FALSE)
  values <- reactiveValues(shouldShow3 = FALSE)
  
  observe({
    if (input$begin == 0) return()
    values$shouldShow = TRUE
    values$shouldShow2 = FALSE
    values$shouldShow3 = FALSE
  })
  
  observe({
    if(is.null(input$node) || input$node==0)
      return()
    values$shouldShow = FALSE
    values$shouldShow2 = TRUE
    values$shouldShow3 = TRUE
  })
  
  observe({
    if (is.null(input$node2) || input$node2 == 0)
      return()
    values$shouldShow2 = FALSE
  })
  
  observe({
    if(is.null(input$button3) || input$button3==1) {
      values$shouldShow=FALSE
      values$shouldShow2=FALSE
      values$shouldShow3=FALSE
    }
  })
  
  
  output$uiButton1 <- renderUI({
    if(values$shouldShow)
      actionButton("node", "Make Population 1")
  })
  
  output$uiButton2 <- renderUI({
    if (values$shouldShow2)
      actionButton("node2","Make Population 2")
  })
  
  output$uiButton3 <- renderUI({
    if(values$shouldShow3)
      actionButton("button3", "Clear Populations")
  })
  
})