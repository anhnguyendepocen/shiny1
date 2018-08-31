library(shiny)
source("rvg.R")

shinyServer(function(input, output) {

#######################################
# Probability Integral Transform
#
 
# Exponential Example
  exp.all.out<-reactiveValues()
  observe({
    if(input$go.exp==0){exp.all.out$history<-init.all()} 
    else{
      exp.all.out$history<-add.exp(isolate(exp.all.out$history),isolate(input$lambda),isolate(input$num.exp))
    }
  })
  
  output$PITexpPlot <- renderPlot({
    input$go.exp
    input$clear.exp
    input$lambda
    par(mfrow=c(1,2),oma=c(0,0,0,0),mar=c(5.1,2.1,1,1.1))
    isolate(plot.unif(exp.all.out$history))
    isolate(plot.exp(input$lambda,exp.all.out$history))
  })
  
  observe({
    input$pitEx
    input$lambda
    input$clear.exp
    exp.all.out$history<-init.all()
  })
  

output$totalcountExp<-renderUI({
  input$go.exp
  last<-length(exp.all.out$history$X)
  if(last>0){
    isolate(paste("Total number of replicates: ",last))
  }
})


output$summaryExp <- renderUI({
  input$go.exp
  last<-length(exp.all.out$history$U)
  if(last>0){
    strexp1<-paste("The most recent value of U is:", 
                round(exp.all.out$history$U[last],3))
    strexp2<-"This gives the following for x:"
    HTML(paste(strexp1,strexp2,sep='<br/>'))
  }})


output$invExp<-renderUI({
  input$go.exp
  last<-length(exp.all.out$history$X)
  u<-exp.all.out$history$U[last]
  x<-exp.all.out$history$X[last]
  lambda<-input$lambda
  if(last>0){
      withMathJax(sprintf("$$x= \\frac{-ln(1-u)}{\\lambda} = \\frac{-ln(1-%0.3f)}{%0.1f} = %0.3f$$", u,lambda,x))
  }
})


# Linear example
linear.all.out<-reactiveValues()
observe({
  if(input$go.linear==0){linear.all.out$history<-init.all()} 
  else{
    linear.all.out$history<-add.linear(isolate(linear.all.out$history),isolate(input$num.linear))
  }
})

output$PITlinearPlot <- renderPlot({
  input$go.linear
  input$clear.linear
  par(mfrow=c(1,2),oma=c(0,0,0,0),mar=c(5.1,2.1,1,1.1))
  isolate(plot.unif(linear.all.out$history))
  isolate(plot.linear(linear.all.out$history))
})

observe({
  input$pitEx
  input$clear.linear
  linear.all.out$history<-init.all()
})


output$totalcountLin<-renderUI({
  input$go.linear
  last<-length(linear.all.out$history$X)
  if(last>0){
    isolate(paste("Total number of replicates: ",last))
  }
})



output$summaryLin <- renderUI({
  input$go.linear
  last<-length(linear.all.out$history$U)
  if(last>0){
    strexp1<-paste("The most recent value of U is:", 
                   round(linear.all.out$history$U[last],3))
    strexp2<-"This gives the following for x:"
    HTML(paste(strexp1,strexp2,sep='<br/>'))
  }})


output$invLin<-renderUI({
  input$go.linear
  last<-length(linear.all.out$history$X)
  u<-linear.all.out$history$U[last]
  x<-linear.all.out$history$X[last]
  if(last>0){
    withMathJax(sprintf("$$x= 4\\sqrt{u} = 4\\sqrt{%0.3f} = %0.3f$$", u,x))
  }
})

  
##########################################
# Accept-Reject 
#

# Beta example

  beta.all.out<-reactiveValues()
  observe({
    if(input$go==0){beta.all.out$history<-temp.start(isolate(input$alpha),isolate(input$beta))} 
    else{
    beta.all.out$history<-temp.start2(isolate(beta.all.out$history),isolate(input$alpha),isolate(input$beta),isolate(input$num))
  }
})

observe({
  input$alpha 
  input$beta 
  input$clear
  beta.all.out$history<-temp.start(input$alpha,input$beta)
  })


output$densityPlot <- renderPlot({
  input$go
  input$clear
  input$alpha
  input$beta
  par(mfrow=c(1,2),oma=c(0,0,0,0),mar=c(5.1,2.1,1,1.1))
  isolate(plot.unif(beta.all.out$history))
  isolate(plot.beta(input$alpha,input$beta,beta.all.out$history))
})

output$summary <- renderUI({
    input$go
    last<-length(beta.all.out$history$Y)
    if(last>0){
      str1<-paste("The most recent value of U is:", 
                  round(beta.all.out$history$U[last],3), "(enlarged and green)")
      str2<-paste("The most recent value of Y is:", 
            round(beta.all.out$history$Y[last],3), "(enlarged, and green if accepted; red if rejected)")
      str3<-paste("The value of Y is", ifelse(beta.all.out$history$status[last]=="accept","<b>accepted</b>","<b>rejected</b>"),"because:")
      HTML(paste(str1,str2,str3,sep='<br/>'))
}})

output$accrej<-renderUI({
  input$go
  last<-length(beta.all.out$history$Y)
  if(last>0){
    u<-beta.all.out$history$U[last]
    fy<-beta.all.out$history$fy[last]
    M<-beta.all.out$history$M[last]
    gy<-beta.all.out$history$gy[last]
    ratio<-fy/(M*gy)
    if(beta.all.out$history$status[last]=="accept"){
       withMathJax(sprintf("$$%0.3f \\leq \\frac{f(y)}{Mg(y)} = 
\\frac{\\frac{\\Gamma(\\alpha + \\beta)}{\\Gamma(\\alpha)\\Gamma(\\beta)}y^{\\alpha-1}(1-y)^{\\beta-1}}{M \\cdot 1_{\\{0 \\leq y \\leq 1\\}}}
                           = \\frac{%0.2f}{%0.3f \\cdot 1} = %0.2f$$",
                           u,fy,M,ratio))
     } else {
       withMathJax(sprintf("$$%0.3f > \\frac{f(y)}{Mg(y)} = 
\\frac{\\frac{\\Gamma(\\alpha + \\beta)}{\\Gamma(\\alpha)\\Gamma(\\beta)}y^{\\alpha-1}(1-y)^{\\beta-1}}{M \\cdot 1_{\\{0 \\leq y \\leq 1\\}}}
                           = \\frac{%0.2f}{%0.2f \\cdot 1} = %0.2f$$",
                           u,fy,M,ratio))
     }
  }
})

output$unifnote<-renderText({
  input$alpha
  input$beta
  if(input$alpha==1 & input$beta==1){
    "Note that for the Beta(1,1) distribution, every point will be accepted, as we would expect
    since it is equivalent to the Uniform[0,1] distribution."
  }
  
})

output$M<- renderText({
  input$alpha
  input$beta
  isolate(paste("For the current set of parameter values, M = ", 
                  round(optimize(dbeta,shape1=input$alpha,shape2=input$beta,interval=c(0,1),maximum=T)$objective,digits=3),".",sep=""))
})

output$totalcount<-renderUI({
  input$go
  last<-length(beta.all.out$history$Y)
  if(last>0){
    isolate(paste("Total number of replicates: ",last))
  }
})


# Truncated normal example

tnorm.all.out<-reactiveValues()
observe({
  if(input$tnormGo==0){tnorm.all.out$history<-start.tnorm()} 
  else{
    tnorm.all.out$history<-add.tnorm(isolate(tnorm.all.out$history),isolate(input$tnormNum))
  }
})

observe({
  input$tnormClear
  tnorm.all.out$history<-start.tnorm()
})



output$tnormDensityPlot <- renderPlot({
  input$tnormGo
  input$tnormClear
  par(mfrow=c(1,2),oma=c(0,0,0,0),mar=c(5.1,2.1,1,1.1))
  isolate(plot.unif(tnorm.all.out$history))
  isolate(plot.tnorm(tnorm.all.out$history))
})

output$tnormsummary <- renderUI({
  input$tnormGo
  last<-length(tnorm.all.out$history$Y)
  if(last>0){
    str1<-paste("The most recent value of U is:", 
                round(tnorm.all.out$history$U[last],3), "(enlarged and green)")
    str2<-paste("The most recent value of Y is:", 
                round(tnorm.all.out$history$Y[last],3), "(enlarged, and green if accepted; red if rejected)")
    str3<-paste("The value of Y is", ifelse(tnorm.all.out$history$status[last]=="accept","<b>accepted</b>","<b>rejected</b>"),"because:")
    HTML(paste(str1,str2,str3,sep='<br/>'))
  }})


output$tnormaccrej<-renderUI({
  input$tnormGo
  last<-length(tnorm.all.out$history$Y)
  if(last>0){
    u<-tnorm.all.out$history$U[last]
    fy<-tnorm.all.out$history$fy[last]
    M<-tnorm.all.out$history$M[last]
    gy<-tnorm.all.out$history$gy[last]
    y<-tnorm.all.out$history$Y[last]
    ratio<-fy/(M*gy)
    if(tnorm.all.out$history$status[last]=="accept"){
      withMathJax(sprintf("$$%0.3f \\leq \\frac{f(y)}{Mg(y)} = 
                          \\frac{\\frac{1}{\\sqrt{2 \\pi}}e^{-\\frac{1}{2}(%0.2f)^2}
                    \\cdot \\left[\\frac{1}{1-\\Phi(2)}\\right] }{M \\cdot e^{2-%0.2f} }
                          = \\frac{%0.2f}{%0.3f \\cdot %0.3f} = %0.2f$$",
                          u,y,y,fy,M,gy,ratio))
    } else {
      withMathJax(sprintf("$$%0.3f > \\frac{f(y)}{Mg(y)} = 
                          \\frac{\\frac{1}{\\sqrt{2 \\pi}}e^{-\\frac{1}{2}(%0.2f)^2}
                    \\cdot \\left[\\frac{1}{1-\\Phi(2)}\\right] }{M \\cdot e^{2-%0.2f} }
                          = \\frac{%0.2f}{%0.3f \\cdot %0.3f} = %0.2f$$",
                          u,y,y,fy,M,gy,ratio))
    }
  }
  })

output$tnormRatio<-renderUI({
  input$tnormGo
  num<-length(tnorm.all.out$history$Y)
  if(num>0){
    str4<-paste("The proportion of points that have been accepted is <b>", 
                round(sum(tnorm.all.out$history$status=="accept")/num,3),"</b> (out of ",num,")",sep="")
    HTML(str4)    
  }
})



})  # end of shinyServer

