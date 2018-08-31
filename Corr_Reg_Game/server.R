library(shiny)
library(shinyBS)
library(ggplot2)
library(magrittr)
library(ggvis)

###A function to create random data with a certain correlation
create = function(n,rho){
    
  x1   = rnorm(n,sample(c(0,1,3),1),sample(c(1,2),1))
  x2   = rnorm(n,sample(c(2,4,6),1),sample(c(1,2,3),1))
  xctr = scale(cbind(x1,x2),center=TRUE,scale=FALSE)
  Q    = qr.Q(qr(xctr[ , 1, drop=FALSE])) 
  P    = tcrossprod(Q) 
  x2o  = (diag(n)-P) %*% xctr[ , 2]                
  xc2  = cbind(xctr[ , 1], x2o)              
  Y    = xc2 %*% diag(1/sqrt(colSums(xc2^2))) 
  if (rho==1){
    x3 = Y[ , 1] 
  }else{
    x3 = Y[ , 2] + (1 / tan(acos(rho))) * Y[ , 1] 
  }
  return(list(x1,sample(c(1,2,3,4),1)+x3))
  
}

###Shiny Server Code###

shinyServer(function(input,output,session){
    
###Code for correlation tab###  
  correlated = reactive({
    
    input$newdataset
    create(isolate(input$nobs),sample(seq(-1,1,by=.1),size=1))
    
  })
  
  checker <- reactiveValues(cheat = "no")
  
  observe({
    if(input$newdataset != 0){
      closeAlert(session,alertId="a")
      checker$cheat <- "no"
    }
  })
  
  observe({
    input$cheat
    if(input$cheat!=0){
      checker$cheat <- "yes"
    }
  })
  
  output$showcorr = renderUI({
    
    checker$cheat
    isolate({
      if (checker$cheat == "no"){
        paste("")
      }else{
        dataCorr = data.frame(exp=unlist(correlated()[[1]]),res=unlist(correlated()[[2]]))
        withMathJax()
        paste0("The correct answer is ",round(cor(dataCorr$exp,dataCorr$res),1),".")     
      }
    })
  })
  
  
#   output$correlationPlot <- renderPlot({
  observe({  
#     corr.data = data.frame(exp=unlist(correlated()[[1]]),res=unlist(correlated()[[2]]))
#     ggplot(data=corr.data)+geom_point(aes(x=exp,y=res)) 

exp=unlist(correlated()[[1]])
res=unlist(correlated()[[2]])
contrib=((exp-mean(exp))/sd(exp))*((res-mean(res))/sd(res))/length(res)

corr.data = data.frame(exp=exp,res=res,contrib=contrib)

    corr.data %>% 
          ggvis(~exp, ~res,key:=~contrib) %>% 
          layer_points() %>% 
          add_tooltip(function(df){ 
            paste0("Correlation contribution:",br(),round(df$contrib,5),br(),"Coordinates (Exp,Res):",br(),
                   "(",round(df$exp,3),",",round(df$res,3),")")
          })%>%
          bind_shiny("correlationPlot")
  })




observe({
  
    input$answer
    isolate({
    
      corr.data = data.frame(exp=unlist(correlated()[[1]]),res=unlist(correlated()[[2]]))
    
      if (input$answer != 0){
  
        if (isolate(input$rho) == round(cor(corr.data$exp,corr.data$res),1)){
    
          createAlert(session,
                      inputId = "correct",
                      title = "Correct!",
                      message = "You have guessed the correct correlation, click 'New Data' to play again",
                      type = "success",
                      dismiss = TRUE,
                      block = FALSE,
                      append = FALSE,
                      alertId = "a"
          )
    
        }else{       
    
          createAlert(session,
                      inputId = "correct",
                      title = paste(input$rho," is incorrect..."),
                      message = "Change your correlation and click 'Submit' to try again.",
                      type = "danger",
                      dismiss = TRUE,
                      block = FALSE,
                      append = FALSE,
                      alertId = "a"
          )
    
        }
  
      }
    
    })

  })

###CODE FOR REGRESSION TAB###

  corr.dat = reactive({
  
    input$getdata
    create(isolate(input$obs),isolate(input$corr))
  
  })

check <- reactiveValues(hit = "getdata")

observe({
  input$getdata
  check$hit <- "getdata"
  check$showit <- "no"
})
observe({
  input$go
  check$hit <- "go"
  check$showit <- "no"
})
observe({
  input$showit
  if(input$showit!=0){
    check$showit <- "yes"
  }
})

  toplotornottoplot = reactive({
    input$go
  
    reg.data = data.frame(expl=unlist(corr.dat()[[1]]),resp=unlist(corr.dat()[[2]]))
    model = lm(resp~expl,data=reg.data)
    coefs = data.frame(a=input$b0,b=input$b1)
    
    pred.fits = input$b0 + input$b1*reg.data$expl
    seg.data = data.frame(x0=reg.data$expl,y0=reg.data$resp,x1=reg.data$expl,y1=pred.fits)
    
      if(input$getdata==0 & input$go ==0){
        p = ggplot()+geom_point(data=reg.data,aes(x=expl,y=resp))+xlim(0,NA)
      }else{
        if(check$hit == "getdata"){
          p = ggplot()+geom_point(data=reg.data,aes(x=expl,y=resp))+xlim(0,NA)
          closeAlert(session,alertId="a1")
        }else{
          p = ggplot()+geom_point(data=reg.data,aes(x=expl,y=resp))
          p = p+geom_abline(data=coefs,aes(intercept=a,slope=b))
          p = p+geom_segment(data=seg.data,aes(x=x0,y=y0,xend=x1,yend=y1),color='red')+xlim(0,NA)
        }
      }
      list(p)
  })

  output$regressionPlot <- renderPlot({
    
  
    reg.data = data.frame(expl=unlist(corr.dat()[[1]]),resp=unlist(corr.dat()[[2]]))
    model = lm(resp~expl,data=reg.data)
    coefs = data.frame(a=input$b0,b=input$b1)
    
    pred.fits = input$b0 + input$b1*reg.data$expl
    seg.data = data.frame(x0=reg.data$expl,y0=reg.data$resp,x1=reg.data$expl,y1=pred.fits)
    
    toplotornottoplot()




    
    
  })  

#SSE stuff

# output$SSE = renderUI({
#   
#   input$go
#   input$getdata
#   isolate({
#     if(input$go != 0){
#       if(check$hit == "getdata"){
#         h6(paste(""))
#       }else{
#         reg.data = data.frame(expl=unlist(corr.dat()[[1]]),resp=unlist(corr.dat()[[2]]))
#         model = lm(resp~expl,data=reg.data)
#         pred.fits = input$b0 + input$b1*reg.data$expl
#         realsse = sum((model$resid)^2)
#         predsse = sum((reg.data$resp-pred.fits)^2)
# #         predsst = sum((reg.data$resp-mean(reg.data$resp))^2)
# #         realRsquare = summary(model)$r.squared
# #         predRsquare = 1-(predsse/predsst)
# #         if(input$sseOrRsq == "Rsq"){
# #           h6(paste("The current R-sq from your inputs is",round(predRsquare,3),". The R-sq for the correct model is ",round(realRsquare,3),". Yours will be bit off since your are using rounded values."),align="center")
# #         }else{
#          h6(paste("The current SSE from your inputs is",round(predsse,3),". The SSE for the correct model is ",round(realsse,3),". Yours will be bit off since your are using rounded values."),align="center")  
# #         }
#         
#       }    
#     }
#   })
# })

output$realline = renderUI({
  
  check$showit
  isolate({
    if (check$showit == "no"){
      paste("")
    }else{
        reg.data = data.frame(expl=unlist(corr.dat()[[1]]),resp=unlist(corr.dat()[[2]]))
        model = lm(resp~expl,data=reg.data)
        withMathJax()
        paste0("The correct answer is b0=",round(model[[1]][1]),", and b1=",round(model[[1]][2],1),".") 

    }
  })
})

  observe({
    
    input$go
    isolate({
      
      reg.data = data.frame(expl=unlist(corr.dat()[[1]]),resp=unlist(corr.dat()[[2]]))
      model = lm(resp~expl,data=reg.data)
      coefs = data.frame(a=coef(model)[1],b=coef(model)[2])
      if(input$go != 0){
          
          if(round(model[[1]][1])==input$b0 && round(model[[1]][2],1)==input$b1){
           
                createAlert(session,
                            inputId = "success",
                            title = "Success!",
                            message = paste("Your inputs for the model were approximately correct!",br(),
                                            "The real values were:",br(),
                                            "Intercept = ",round(model[[1]][1],3)," Slope = ",round(model[[1]][2],3),br(),
                                            "Click 'New Data' to play again."),
                            type = "success",
                            dismiss = TRUE,
                            block = FALSE,
                            append = FALSE,
                            alertId = "a1")
      
      
          }else{
            
                createAlert(session,
                            inputId = "success",
                            title = "Incorrect.",
                            message = paste("Your inputs, Intercept=",input$b0," and Slope=",input$b1,
                                            " for the model were incorrect.",
                                            " Change your inputs and hit 'Submit' to try again"),
                            type = "danger",
                            dismiss = TRUE,
                            block = FALSE,
                            append = FALSE,
                            alertId = "a1")
        

          }
      
        
      }
    })
  })


})