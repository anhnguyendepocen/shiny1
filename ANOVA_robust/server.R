# ------------------
# App Title: Robustness of ANOVA F-test to violation of constant variance
#    Author: Gail Potter
# ------------------

simulate.response = function(nsim, sample.sizes, s1=2, s2=2, s3=2, m1=0, m2=0, m3=0){
  ## user inputs number of samples to draw (nsim),
  ## sample size, and standard deviation of each sample (s1,s2,s3)
  ## The function outputs a matrix of nsim samples of size sample.size
  ## drawn from normal distributions with mean 0 and specified SDs.
  ## Each column is a vector of the 3 simulated output.
  
  matrix(rnorm(nsim * sum(sample.sizes), mean=rep(c(m1,m2,m3), sample.sizes), 
               sd=rep(c(s1,s2,s3), sample.sizes)), ncol=nsim)
}
  
get.f.stat = function(y,x)  return(anova(lm(y~x))[[4]][1])

create.predictor = function(sample.sizes) 
  factor(rep(paste("Group", 1:3),sample.sizes))

shinyServer(function(input, output, session) {
  
  draw.sample <- reactiveValues()
  
  observe({
    x = isolate(create.predictor(sample.sizes=c(input$n1, input$n2, input$n3)))
    
    if(input$go==0) { 
      y = simulate.response(
        nsim=input$nsim, sample.sizes=c(input$n1, input$n2, input$n3), 
        s1=input$sigma1, s2=input$sigma2, s3=input$sigma3,
        m1=input$mu1, m2=input$mu2, m3=input$mu3)  
      f.stats =  apply(y, 2, get.f.stat, x=x)
      draw.sample$f.stats <- c(f.stats, isolate(draw.sample$f.stats))
      draw.sample$y = y[1:isolate(input$n1+input$n2+input$n3)]
    }   else {
      input$go
      y = simulate.response(nsim=isolate(input$nsim), 
                            sample.sizes=c(isolate(input$n1), isolate(input$n2), isolate(input$n3)), 
                          s1=isolate(input$sigma1), s2=isolate(input$sigma2), s3=isolate(input$sigma3),
                          m1=isolate(input$mu1), m2=isolate(input$mu2), m3=isolate(input$mu3))
      
    
       
      
     withProgress(message = "Calculating, please wait.",
                   detail = " ", value=.5, {
       f.stats = isolate(apply(y, 2, get.f.stat, x=x))
         
     draw.sample$f.stats <- c(f.stats, isolate(draw.sample$f.stats))
     draw.sample$y = y[1:isolate(input$n1+input$n2+input$n3)]   
     })
    
  }
    
  })
  
  
  observe({
    input$sigma1
    input$sigma2
    input$sigma3
    input$mu1
    input$mu2
    input$mu3
    input$n1
    input$n2
    input$n3
    input$n
    input$clear
    draw.sample$y<-NULL
    draw.sample$f.stats=NULL
  })
  
  output$y <- renderText({y})
  output$dotplot <- renderPlot({
    input$sigma1
    input$sigma2
    input$sigma3
    input$n1
    input$n2
    input$n3
    input$n
    
    x = create.predictor(sample.sizes=c(input$n1, input$n2, input$n3))
    y = draw.sample$y
    
    par(mfrow=c(1,2))
    
    if (!is.null(y)){
      
      stripchart(y ~ x, 
                 vertical = TRUE, method="jitter" ,  main =paste("Sampled data"),
                 pch = 21, col = "darkblue", bg = "lightblue")
      
      crit = qf(0.95, df1=2, df2=(sum(c(input$n1, input$n2, input$n3))-1))
      type.I.error = mean(draw.sample$f.stats>=crit)
     #xmax = max(15, round(max(draw.sample$f.stats[draw.sample$f.stats<20]))+1)
     xmax = max(15, round(max(draw.sample$f.stats))+1)
      ymax = max(hist(draw.sample$f.stats, 
                      breaks=seq(0,xmax,1), plot=FALSE)$counts+2, 15)
      hist(draw.sample$f.stats, col="lightblue", 
           main="Sampling distribution",
           xlim=c(0,xmax), breaks=seq(0,xmax,1),
           ylim=c(0,ymax), xlab="F-statistics")
      
    abline(v=crit,col="red", lty=2)
      text(x=crit+1, y=ymax*.7, expression(F[0.05]), col="red")
    } 
    
    })


  output$typeI  = renderUI({  
    if (input$mu1!=input$mu2 | input$mu1!=input$mu3 | input$mu2!=input$mu3){
           typeofstudy="Power = "
    } else {
           typeofstudy="Type I error rate = "
    }
    if (!is.null(draw.sample$y)) {
      crit = qf(0.95, df1=2, df2=(input$n1+input$n2+input$n3-1))
      type.I.error = sum(draw.sample$f.stats>=crit)/length(draw.sample$f.stats)
    n.samples = length(draw.sample$f.stats)
    str1 = paste("Total samples drawn =", n.samples)
    str2 = paste(typeofstudy, sum(draw.sample$f.stats>=crit), "/", 
    length(draw.sample$f.stats), " = ", round(type.I.error,3))
    HTML(paste(str1, str2, sep = '<br/>'))
    }
  })
  
})