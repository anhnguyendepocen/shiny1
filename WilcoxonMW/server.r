# -----------------------------------------------------------------
# App Title: Performance of Wilcoxon-Mann-Whitney Test and t-test
#    Author: Jimmy Wong
# -----------------------------------------------------------------

#####################################################################################################################
#####################################################################################################################
## Libraries
#####################################################################################################################
#####################################################################################################################

library(ggplot2)
library(gridExtra)
library(googleVis)
library(shinyIncubator)

#####################################################################################################################
#####################################################################################################################
## Global Functions
#####################################################################################################################
#####################################################################################################################

# Comparing type 1 error/power of t and wmw between two normal populations
comp.norm = function(n.norm1,n.norm2,mean.normal1,mean.normal2,alpha,nsim)
{
  t.diff=NULL
  wil.diff=NULL
  tstat=NULL
  wilstat=NULL
  
  for (i in 1:nsim)
  {
    x = rnorm(n.norm1, mean=mean.normal1)
    y = rnorm(n.norm2, mean=mean.normal2)
    t = t.test(x, y)
    t.diff[i] = (t$p.value<alpha)*1
    tstat[i] = t$statistic
    wil = wilcox.test(x, y)
    wilstat[i] = wil$statistic
    wil.diff[i] = (wil$p.value<alpha)*1
  }
  
  (t.percent = mean(t.diff))
  (wil.percent = mean(wil.diff))
  return(list(t.percent, wil.percent, tstat, wilstat, t.diff, wil.diff))
}

# Comparing type 1 error/power of t and wmw between two populations with 
# different shapes
comp.diffshape = function(n.norm,n.gam,nsim,mean.normal,shape,scale,alpha)
{
  t.diff=NULL
  wil.diff=NULL
  tstat=NULL
  wilstat=NULL
  
  for (i in 1:nsim)
  {
    x = rnorm(n.norm, mean=mean.normal)
    y = rgamma(n.gam, shape=shape, scale=scale)
    t = t.test(x, y)
    t.diff[i] = (t$p.value<alpha)*1
    tstat[i] = t$statistic
    wil = wilcox.test(x, y)
    wilstat[i] = wil$statistic
    wil.diff[i] = (wil$p.value<alpha)*1
  }
  
  (t.percent = mean(t.diff))
  (wil.percent = mean(wil.diff))
  return(list(t.percent, wil.percent, tstat, wilstat, t.diff, wil.diff))
}

# Comparing type 1 error/power of t and wmw between two gamma populations
comp.skewed = function(n.gam1,n.gam2,nsim,shape,scale,alpha,far)
{
  t.diff=NULL
  wil.diff=NULL
  tstat=NULL
  wilstat=NULL
  
  for (i in 1:nsim)
  {
    x = rgamma(n.gam1, shape=shape, scale=scale)
    y = rgamma(n.gam2, shape=shape, scale=scale)+far
    
    t = t.test(x, y)
    t.diff[i] = (t$p.value<alpha)*1
    tstat[i] = t$statistic
    wil = wilcox.test(x, y)
    wilstat[i] = wil$statistic
    wil.diff[i] = (wil$p.value<alpha)*1
  }
  
  (t.percent = mean(t.diff))
  (wil.percent = mean(wil.diff))
  return(list(t.percent, wil.percent, tstat, wilstat, t.diff, wil.diff))
}

# Comparing type 1 error/power between two normals but allowing the mean of the normal
# distribution to vary
comp.vary.norm = function(norm.means,n.norm1,n.norm2,nsim,alpha,norm.mean2)
{
  norm.means1 = seq(norm.means[1],norm.means[2],by=.1)
  t.percent=NULL
  wil.percent=NULL
  i=0
  while(i<length(norm.means1))
  {
    i=i+1
    result = comp.norm(n.norm1=n.norm1,n.norm2=n.norm2,nsim=nsim,mean.normal1=norm.means1[i],
                       mean.normal2=norm.mean2,alpha=alpha)
    t.percent[i] = result[[1]]
    wil.percent[i] = result[[2]]
  }
  return(list(terror=t.percent,wmwerror=wil.percent))
}

# Comparing type 1 error/power between normal and gamma but allowing the mean of the normal
# distribution to vary
comp.vary.diffshape = function(norm.means,n.norm,n.gam,nsim,shape,scale,alpha)
{
  norm.means1 = seq(norm.means[1],norm.means[2],by=.1)
  t.percent=NULL
  wil.percent=NULL
  i=0
  while(i<length(norm.means1))
  {
    i=i+1
    result = comp.diffshape(n.norm=n.norm,n.gam=n.gam,nsim=nsim,mean.normal=norm.means1[i],
                            shape=shape,scale=scale,alpha)
    t.percent[i] = result[[1]]
    wil.percent[i] = result[[2]]
  }
  return(list(terror=t.percent,wmwerror=wil.percent))
}

# Comparing type 1 error/power between two gammas with same shape but allowing 
# the mean of 1st gamma distribution to vary
comp.vary.skewed = function(farvary,n.gam1,n.gam2,nsim,shape,scale,alpha)
{
  farvary1 = seq(farvary[1],farvary[2],by=.1)
  t.percent=NULL
  wil.percent=NULL
  i=0
  while(i<length(farvary1))
  {
    i=i+1
    result = comp.skewed(n.gam1=n.gam1,n.gam2=n.gam2,nsim=nsim,shape=shape,
                         scale=scale,alpha=alpha,far=farvary1[i])
      
    t.percent[i] = result[[1]]
    wil.percent[i] = result[[2]]
  }
  return(list(terror=t.percent,wmwerror=wil.percent))
}  

#####################################################################################################################
#####################################################################################################################
## Shiny Server
#####################################################################################################################
#####################################################################################################################

shinyServer(function(input, output, session) {
  
  #####################################################################################################################
  #####################################################################################################################
  ## Goal of Study Panel
  #####################################################################################################################
  #####################################################################################################################
  
  output$intrograph = renderPlot({
    x=seq(-3,3,length.out=200)
    x1=seq(0,6,length.out=200)
    g1 = ggplot() + geom_line(data.frame(x=x, y=dnorm(x,mean=0,sd=1)), mapping=aes(x=x, y=y), color="navy") +
      geom_line(data.frame(x=x1, y=dnorm(x1,mean=3,sd=1)), mapping=aes(x=x, y=y), color="gold2") + xlim(-3,6) +
      ggtitle("Two Normal populations\nwith different means") + xlab("") + ylab("") +
      theme(axis.text = element_blank(), axis.ticks = element_blank()) + theme_bw()
    x2=seq(0,13,length.out=200)
    g2 = ggplot() + geom_line(data.frame(x=x2, y=dgamma(x2,shape=6,scale=.5)), mapping=aes(x=x, y=y), color="navy") +
      geom_line(data.frame(x=x2+3, y=dgamma(x2,shape=6,scale=.5)), mapping=aes(x=x, y=y), color="gold2") + xlim(0,13) +
      ggtitle("Two Gamma populations\nwith different means") + xlab("") + ylab("") +
      theme(axis.text = element_blank(), axis.ticks = element_blank()) + theme_bw()
    g3 = ggplot() + geom_line(data.frame(x=x2, y=dgamma(x2,shape=6,scale=.5)), mapping=aes(x=x, y=y), color="navy") +
      geom_line(data.frame(x=x2, y=dnorm(x2,mean=3,sd=1)), mapping=aes(x=x, y=y), color="gold2") + xlim(0,13) +
      ggtitle("Two populations with same mean\nbut different shapes") + xlab("") + ylab("") +
      theme(axis.text = element_blank(), axis.ticks = element_blank()) + theme_bw()
    g4 = ggplot() + geom_line(data.frame(x=x2, y=dgamma(x2,shape=6,scale=.5)), mapping=aes(x=x, y=y), color="navy") +
      geom_line(data.frame(x=x2, y=dnorm(x2,mean=5,sd=1)), mapping=aes(x=x, y=y), color="gold2") + xlim(0,13) +
      ggtitle("Two populations with different\nmeans and shapes") + xlab("") + ylab("") +
      theme(axis.text = element_blank(), axis.ticks = element_blank()) + theme_bw()
    gg = arrangeGrob(g1,g2,g3,g4,ncol=2)
    return(gg)
  })
  
  #####################################################################################################################
  #####################################################################################################################
  ## Guess the Population Panel
  #####################################################################################################################
  #####################################################################################################################
  
  order = reactive({
    input$resetgame
    return(sample(c("normal1","normal2","gamma1","gamma2"),size=4))
  })
  
  output$guesspop = renderPlot({
    input$resetgame
    normal1 = ggplot() + geom_histogram(aes(x=rnorm(20,mean=3,sd=1)),fill="white",color="navy") + xlab("") + ylab("") +
                theme(axis.text = element_blank(), axis.ticks = element_blank()) + theme_bw()
    normal2 = ggplot() + geom_histogram(aes(x=rnorm(20,mean=3,sd=1)),fill="white",color="navy") + xlab("") + ylab("") +
                theme(axis.text = element_blank(), axis.ticks = element_blank()) + theme_bw()
    gamma1 = ggplot() + geom_histogram(aes(x=rgamma(20,shape=6,scale=.5)),fill="white",color="navy") + xlab("") + ylab("") +
                theme(axis.text = element_blank(), axis.ticks = element_blank()) + theme_bw()
    gamma2 = ggplot() + geom_histogram(aes(x=rgamma(20,shape=6,scale=.5)),fill="white",color="navy") + xlab("") + ylab("") +
                theme(axis.text = element_blank(), axis.ticks = element_blank()) + theme_bw()
    
    guesspop = arrangeGrob(get(order()[1]) + ggtitle("Random Data 1"),
                           get(order()[2]) + ggtitle("Random Data 2"),
                           get(order()[3]) + ggtitle("Random Data 3"),
                           get(order()[4]) + ggtitle("Random Data 4"),nrow=2)
    print(guesspop)
  })
  
  output$answerinsert = renderUI({
    input$resetgame
    selectizeInput("answer","Your answer (from 1,2,3,4):",choices=c("normal"="normal_1","normal"="normal_2","gamma"="gamma_1","gamma"="gamma_2"),
                   multiple=TRUE,options=list(placeholder="select the order of distributions"))
  })
  
  output$answerreveal = renderUI({
    input$resetgame
    checkboxInput("reveal", "Reveal answer", FALSE)
  })
  
  output$answercheck = renderUI({ 
    input$resetgame
    if(length(input$answer)==4)
    {
      a1 = strsplit(input$answer[1],"_")[[1]][1]
      a2 = strsplit(input$answer[2],"_")[[1]][1]
      a3 = strsplit(input$answer[3],"_")[[1]][1]
      a4 = strsplit(input$answer[4],"_")[[1]][1]  
      
      if(is.na(sum(pmatch(a1,order()[1]), pmatch(a2,order()[2]), pmatch(a3,order()[3]), pmatch(a4,order()[4]))))
      {
        return(code("Oops, try again!"))
      } else if(sum(pmatch(a1,order()[1]), pmatch(a2,order()[2]), pmatch(a3,order()[3]), pmatch(a4,order()[4]))==4)
      {
        return(code("Congratulations, you are correct!"))
      }
    } else
    {
      return(code("Waiting for inputs..."))
    }
  })
  
  output$answer = renderTable({
    input$resetgame
    tab = matrix(nrow=1,ncol=4)
    colnames(tab) = c("Random Data 1","Random Data 2","Random Data 3","Random Data 4")
    rownames(tab) = "Answer"
    tab[1,1] = order()[1]
    tab[1,2] = order()[2]
    tab[1,3] = order()[3]
    tab[1,4] = order()[4]
    return(tab)    
  })
  
  #####################################################################################################################
  #####################################################################################################################
  ## Two Normal Populations Panel
  #####################################################################################################################
  #####################################################################################################################
  
  res.comp = reactive({
    input$start1
    isolate({
    return(comp.norm(n.norm1=input$nnorm1,n.norm2=input$nnorm2,mean.normal1=input$meannorm1,
                     mean.normal2=input$meannorm2,alpha=input$alpha1,nsim=input$nsim1))
    })
  })
  
  output$normselected = renderUI({
    p("You have selected", code(paste0("N(",input$meannorm1,",1)")), "and", 
      code(paste0("N(",input$meannorm2,",1)")))
  })
  
  output$satisfied1 = renderUI({
    HTML("<li type=circle> The condition for the t-test is satisfied because the two population distributions are Normal. </li>
         <li type=circle> The condition for the WMW test is satisfied because the two Normal population distributions have the same shape.</li>")
  })
  
  output$twonormplots = renderPlot({
    x1=seq(input$meannorm1-3,input$meannorm1+3,length.out=200)
    x2=seq(input$meannorm2-3,input$meannorm2+3,length.out=200)
    ggplot() + geom_line(data.frame(x=x1, y=dnorm(x1,mean=input$meannorm1,sd=1)), mapping=aes(x=x, y=y), color="navy") +
      geom_line(data.frame(x=x2, y=dnorm(x2,mean=input$meannorm2,sd=1)), mapping=aes(x=x, y=y), color="salmon") + 
      xlim(min(input$meannorm1-3,input$meannorm2-3),max(input$meannorm1+3,input$meannorm2+3)) +
      ggtitle("Two Normal populations") + xlab("") + ylab("") + theme_bw() +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  })
  
  output$twohistnorms = renderPlot({
    input$start1
    isolate({
    dat = data.frame(t=res.comp()[[3]],wmw=res.comp()[[4]],t.ind=res.comp()[[5]],wmw.ind=res.comp()[[6]])
    
    if(sum(dat$t.ind)==length(dat$t.ind) & input$meannorm1!=input$meannorm2)
    {
      dat$t.ind1 = factor(dat$t.ind,label="Rejected Ho")
      lab1 = "seagreen3"
    } else if(sum(dat$t.ind)==0 & input$meannorm1==input$meannorm2)
    {
      dat$t.ind1 = factor(dat$t.ind,label="Failed to reject Ho")
      lab1 = "seagreen3"
    } else if(sum(dat$t.ind)!=length(dat$t.ind) & input$meannorm1==input$meannorm2)
    {
      dat$t.ind1 = factor(dat$t.ind,label=c("Failed to reject Ho", "Rejected Ho"))
      lab1 = c("seagreen3","tomato")
    } else if(sum(dat$t.ind)!=0 & input$meannorm1!=input$meannorm2)
    {
      dat$t.ind1 = factor(dat$t.ind,label=c("Failed to reject Ho", "Rejected Ho"))
      lab1 = c("tomato","seagreen3")
    }
    
    if(sum(dat$wmw.ind)==length(dat$wmw.ind) & input$meannorm1!=input$meannorm2)
    {
      dat$wmw.ind1 = factor(dat$wmw.ind,label="Rejected Ho")
      lab2 = "seagreen3"
    } else if(sum(dat$wmw.ind)==0 & input$meannorm1==input$meannorm2)
    {
      dat$wmw.ind1 = factor(dat$wmw.ind,label="Failed to reject Ho")
      lab2 = "seagreen3"
    } else if(sum(dat$wmw.ind)!=length(dat$wmw.ind) & input$meannorm1==input$meannorm2)
    {
      dat$wmw.ind1 = factor(dat$wmw.ind,label=c("Failed to reject Ho", "Rejected Ho"))
      lab2 = c("seagreen3","tomato")
    } else if(sum(dat$wmw.ind)!=0 & input$meannorm1!=input$meannorm2)
    {
      dat$wmw.ind1 = factor(dat$wmw.ind,label=c("Failed to reject Ho", "Rejected Ho"))
      lab2 = c("tomato","seagreen3")
    }
    })
    
    g1 = ggplot(dat) + geom_histogram(aes(x=t,fill=t.ind1), alpha=.7) + xlab("t-statistic") + ylab("Frequency") +  
          scale_fill_manual(name="Decision", values=lab1) + theme_bw() + theme(legend.position="bottom") +
          ggtitle("Histogram of t-statistics") 
    g2 = ggplot(dat) + geom_histogram(aes(x=wmw,fill=wmw.ind1), alpha=.7) + xlab("WMW statistic") + ylab("Frequency") +
          scale_fill_manual(name="Decision", values=lab2) + theme_bw() + theme(legend.position="bottom") +
          ggtitle("Histogram of WMW statistics") 
    g3 = arrangeGrob(g1,g2,nrow=1)
    
    return(g3)
  })
  
  output$text1 = renderUI({
    input$start1
    isolate({
    p(HTML("<li type=square> Number of correct t-statistics ="),code(round(res.comp()[[1]]*input$nsim1)),HTML("</li>"),
      HTML("<li type=square> Number of correct WMW statistics ="),code(round(res.comp()[[2]]*input$nsim1)),HTML("</li>"),
      HTML("<li type=square> Number of simulations ="),code(input$nsim1))
    })
  })
  
#   output$table1 = renderTable({
#     input$start1
#     isolate({
#     if(input$start1>0 & input$meannorm1==input$meannorm2)
#     {
#       mat = matrix(c(res.comp()[[1]],res.comp()[[2]]),nrow=1,byrow=TRUE)
#       rownames(mat) = "Type I error rate"
#       colnames(mat) = c("t-test","WMW test")
#       return(mat)
#     } else if(input$start1>0 & input$meannorm1!=input$meannorm2)
#     {
#       mat = matrix(c(res.comp()[[1]],res.comp()[[2]]),nrow=1,byrow=TRUE)
#       rownames(mat) = "Power"
#       colnames(mat) = c("t-test","WMW test")
#       return(mat)
#     }
#     })
#   })
  
  output$gauge1 = renderGvis({
    input$start1
    isolate({
      dat = data.frame(test=c("t-test","WMW"),prob=round(c(res.comp()[[1]],res.comp()[[2]]),digits=3))
      if(input$meannorm1!=input$meannorm2)
      {
        gauge=gvisGauge(dat, options=list(min=0,max=1,greenFrom=.75,greenTo=1,redFrom=0,redTo=.25,width=250,height=250))
      } else if(input$meannorm1==input$meannorm2)
      {
        gauge=gvisGauge(dat, options=list(min=0,max=1,greenFrom=0,greenTo=input$alpha1,
                                          yellowFrom=input$alpha1,yellowTo=input$alpha1*2,
                                          redFrom=input$alpha1*2,redTo=1,width=250,height=250))
      }
      return(gauge)
    })
  })
  
  res.vary = reactive({
    input$start11
    isolate({
    comp.vary.norm(norm.means=input$varymeans1,n.norm1=input$nnorm11,n.norm2=input$nnorm21,nsim=input$nsim11,
                   norm.mean2=input$meannorm21,alpha=input$alpha11)
    })
  })
  
  output$graph.norm = renderGvis({
    input$start11
    isolate({
    if(input$start11>0)
    {
      res.dat = data.frame(mean = seq(input$varymeans1[1],input$varymeans1[2],by=.1),
                           t=round(res.vary()[[1]],digits=2),wmw=round(res.vary()[[2]],digits=2))
      
      gvisLineChart(res.dat,
                    options=list(title="Comparing Type 1 error rate and power between t-test and WMW test",
                                 hAxes="[{title:'Mean of 1st Normal distribution'}]",
                                 vAxes="[{title:'P(rejecting Ho)'}]",
                                 width=800,height=600,series="[{color:'#F08080'},{color:'navy'}]"))
      
#       print(ggplot(data=res.dat) + geom_line(aes(x=mean,y=t.error,color="t-test")) +
#               geom_line(aes(x=mean,y=wmw.error,color="WMW")) + xlab("Mean of Normal distribution") + ylab("P(rejecting Ho)") +
#               scale_color_manual(name="Type of test",values=c("t-test"="navy","WMW"="gold2")) + theme(legend.position="bottom") + 
#               ggtitle("Comparing Type 1 error rate and\npower between t-test and WMW test"))
    }
    })
  })
  
  #####################################################################################################################
  #####################################################################################################################
  ## Normal and Gamma Populations Panel
  #####################################################################################################################
  #####################################################################################################################
  
  res.comp1 = reactive({
    input$start2
    isolate({
    return(comp.diffshape(n.norm=input$nnorm3,n.gam=input$ngam1,nsim=input$nsim2,
                          mean.normal=input$meannorm3,shape=input$shape1,scale=input$scale1,alpha=input$alpha2))
    })
  })
  
  gam.mean1 = reactive({
    return(input$shape1*input$scale1)
  })
  
  output$normgamselected = renderUI({
    p("You have selected", code(paste0("N(",input$meannorm3,",1)")), "and", 
      code(paste0("Gamma(",input$shape1,",",input$scale1,")")))
  })
  
  output$twonormgamplots = renderPlot({   
    x1=seq(min(input$meannorm3-3,0),max(input$meannorm3+3,gam.mean1()+7),length.out=200)
    x2=seq(min(input$meannorm3-3,0),max(input$meannorm3+3,gam.mean1()+7),length.out=200)
    
    ggplot() + geom_line(data.frame(x=x1, y=dnorm(x1,mean=input$meannorm3,sd=1)), mapping=aes(x=x, y=y), color="navy") +
      geom_line(data.frame(x=x2, y=dgamma(x2,shape=input$shape1,scale=input$scale1)), mapping=aes(x=x, y=y), color="salmon") + 
      xlim(min(input$meannorm3-3,0),max(input$meannorm3+3,gam.mean1()+7)) +
      ggtitle("Normal and Gamma populations") + xlab("") + ylab("") + theme_bw() +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  })
  
  output$focusinfo1 = renderUI({
    if(input$meannorm3==gam.mean1())
    {
      return(div("These two distributions have the same means; focus on", code("Type I error rate"), style = "color:navy"))
    } else if(input$meannorm3!=gam.mean1())
    {
      return(div("These two distributions have different means; focus on", code("Power"), style = "color:navy"))
    }
  })

  output$satisfied2 = renderUI({
    HTML("<li type=circle> The condition for the t-test will be satisfied if the sample size for the Gamma population distribution is large enough (at least 20-30). </li>
         <li type=circle> The condition for the WMW test is not satisfied because the two population distributions will never have the same shape.</li>")
  })
  
  output$twohistnormgams = renderPlot({
    input$start2
    isolate({
    dat = data.frame(t=res.comp1()[[3]],wmw=res.comp1()[[4]],t.ind=res.comp1()[[5]],wmw.ind=res.comp1()[[6]])
    
    if(sum(dat$t.ind)==length(dat$t.ind) & input$meannorm3!=gam.mean1())
    {
      dat$t.ind1 = factor(dat$t.ind,label="Rejected Ho")
      lab1 = "seagreen3"
    } else if(sum(dat$t.ind)==0 & input$meannorm3==gam.mean1())
    {
      dat$t.ind1 = factor(dat$t.ind,label="Failed to reject Ho")
      lab1 = "seagreen3"
    } else if(sum(dat$t.ind)!=length(dat$t.ind) & input$meannorm3==gam.mean1())
    {
      dat$t.ind1 = factor(dat$t.ind,label=c("Failed to reject Ho", "Rejected Ho"))
      lab1 = c("seagreen3","tomato")
    } else if(sum(dat$t.ind)!=0 & input$meannorm3!=gam.mean1())
    {
      dat$t.ind1 = factor(dat$t.ind,label=c("Failed to reject Ho", "Rejected Ho"))
      lab1 = c("tomato","seagreen3")
    }
    
    if(sum(dat$wmw.ind)==length(dat$wmw.ind) & input$meannorm3!=gam.mean1())
    {
      dat$wmw.ind1 = factor(dat$wmw.ind,label="Rejected Ho")
      lab2 = "seagreen3"
    } else if(sum(dat$wmw.ind)==0 & input$meannorm3==gam.mean1())
    {
      dat$wmw.ind1 = factor(dat$wmw.ind,label="Failed to reject Ho")
      lab2 = "seagreen3"
    } else if(sum(dat$wmw.ind)!=length(dat$wmw.ind) & input$meannorm3==gam.mean1())
    {
      dat$wmw.ind1 = factor(dat$wmw.ind,label=c("Failed to reject Ho", "Rejected Ho"))
      lab2 = c("seagreen3","tomato")
    } else if(sum(dat$wmw.ind)!=0 & input$meannorm3!=gam.mean1())
    {
      dat$wmw.ind1 = factor(dat$wmw.ind,label=c("Failed to reject Ho", "Rejected Ho"))
      lab2 = c("tomato","seagreen3")
    }
    })
    
    g1 = ggplot(dat) + geom_histogram(aes(x=t,fill=t.ind1), alpha=.7) + xlab("t-statistic") + ylab("Frequency") +  
      scale_fill_manual(name="Decision", values=lab1) + theme_bw() + theme(legend.position="bottom") +
      ggtitle("Histogram of t-statistics") 
    g2 = ggplot(dat) + geom_histogram(aes(x=wmw,fill=wmw.ind1), alpha=.7) + xlab("WMW statistic") + ylab("Frequency") +
      scale_fill_manual(name="Decision", values=lab2) + theme_bw() + theme(legend.position="bottom") +
      ggtitle("Histogram of WMW statistics") 
    g3 = arrangeGrob(g1,g2,nrow=1)
    return(g3)
  })
  
  output$text2 = renderUI({
    input$start2
    isolate({
    p(HTML("<li type=square> Number of correct t-statistics "),code(round(res.comp1()[[1]]*input$nsim2)),HTML("</li>"),
      HTML("<li type=square> Number of correct WMW statistics ="),code(round(res.comp1()[[2]]*input$nsim2)),HTML("</li>"),
      HTML("<li type=square> Number of simulations ="),code(input$nsim2))
    })
  })
  
#   output$table2 = renderTable({
#     input$start2
#     isolate({
#     if(input$start2>0 & input$meannorm3==gam.mean1())
#     {
#       mat = matrix(c(res.comp1()[[1]],res.comp1()[[2]]),nrow=1,byrow=TRUE)
#       rownames(mat) = "Type I error rate"
#       colnames(mat) = c("t-test","WMW test")
#       return(mat)
#     } else if(input$start2>0 & input$meannorm3!=gam.mean1())
#     {
#       mat = matrix(c(res.comp1()[[1]],res.comp1()[[2]]),nrow=1,byrow=TRUE)
#       rownames(mat) = "Power"
#       colnames(mat) = c("t-test","WMW test")
#       return(mat)
#     }
#     })
#   })

  output$whichone = renderUI({
    if(input$meannorm3==gam.mean1())
    {
      return(strong("Type I error rate:"))
    } else if(input$meannorm3!=gam.mean1())
    {
      return(strong("Power:"))
    }
  })

  output$gauge2 = renderGvis({
    input$start2
    isolate({
      dat = data.frame(test=c("t-test","WMW"),prob=round(c(res.comp1()[[1]],res.comp1()[[2]]),digits=3))
      if(input$meannorm3!=gam.mean1())
      {
        gauge=gvisGauge(dat, options=list(min=0,max=1,greenFrom=.75,greenTo=1,redFrom=0,redTo=.25,width=250,height=250))
      } else if(input$meannorm3==gam.mean1())
      {
          gauge=gvisGauge(dat, options=list(min=0,max=1,greenFrom=0,greenTo=input$alpha2,
                                            yellowFrom=input$alpha2,yellowTo=input$alpha2*2,
                                            redFrom=input$alpha2*2,redTo=1,width=250,height=250))
      }
      return(gauge)
    })
  })
  
  res.vary1 = reactive({
    input$start21
    isolate({
    comp.vary.diffshape(norm.means=input$varymeans2,n.norm=input$nnorm31,n.gam=input$ngam11,
                        nsim=input$nsim21,shape=input$shape11,scale=input$scale11,alpha=input$alpha21)
    })
  })
  
  output$graph.normgam = renderGvis({
    input$start21
    isolate({
    if(input$start21>0)
    {
      res.dat = data.frame(mean = seq(input$varymeans2[1],input$varymeans2[2],by=.1), 
                           t=round(res.vary1()[[1]],digits=2),wmw=round(res.vary1()[[2]],digits=2)) 
      
      gvisLineChart(res.dat,
                    options=list(title="Comparing Type 1 error rate and power between t-test and WMW test",
                                 hAxes="[{title:'Mean of Normal distribution'}]",
                                 vAxes="[{title:'P(rejecting Ho)'}]",
                                 width=800,height=600,series="[{color:'#F08080'},{color:'navy'}]"))
      
#       print(ggplot(data=res.dat) + geom_line(aes(x=mean,y=t.error,color="t-test")) +
#               geom_line(aes(x=mean,y=wmw.error,color="WMW")) + xlab("Mean of Normal distribution") + ylab("P(rejecting Ho)") +
#               scale_color_manual(name="Type of test",values=c("t-test"="navy","WMW"="gold2")) + theme(legend.position="bottom") + 
#               ggtitle("Comparing Type 1 error rate and\npower between t-test and WMW test"))
    }
    })
  })
  
  #####################################################################################################################
  #####################################################################################################################
  ## Two Gamma Populations Panel
  #####################################################################################################################
  #####################################################################################################################
  
  res.comp2 = reactive({
    input$start3
    isolate({
      return(comp.skewed(n.gam1=input$ngam2,n.gam2=input$ngam3,nsim=input$nsim3,shape=input$shape2,scale=input$scale2,
                         alpha=input$alpha3,far=input$far1))
    })
  })
  
  gam.means = reactive({
    return(c(input$shape2*input$scale2,(input$shape2*input$scale2)+input$far1))
  })
  
  output$gamselected = renderUI({
    p("You have selected",code(paste0("Gamma(",input$shape2,",",input$scale2,")")), "and", 
      code(paste0("Gamma(",input$shape2,",",input$scale2,")+",input$far1)))

  })
  
  output$twogamplots = renderPlot({
    x1=seq(0,max(gam.means()[1]*input$scale2*3+3,gam.means()[2]*input$scale2*3+3),length.out=1000)
      
    ggplot() + geom_line(data.frame(x=x1, y=dgamma(x1,shape=input$shape2,scale=input$scale2)), mapping=aes(x=x, y=y), color="navy") +
      geom_line(data.frame(x=x1+input$far1, y=dgamma(x1,shape=input$shape2,scale=input$scale2)), mapping=aes(x=x, y=y), color="salmon") + 
      xlim(0,max(gam.means()[1]*input$scale2*3+3,gam.means()[2]*input$scale2*3+3)) +
      ggtitle("Two Gamma populations") + xlab("") + ylab("") + theme_bw() +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  })
  
  output$focusinfo2 = renderUI({
    if(gam.means()[1]==gam.means()[2])
    {
      return(div("These two Gamma distributions have the same means; focus on", code("Type I error rate"), style = "color:navy"))
    } else if(gam.means()[1]!=gam.means()[2])
    {
      return(div("These two Gamma distributions have different means; focus on", code("Power"), style = "color:navy"))
    }
  })

  output$satisfied3 = renderUI({
    HTML("<li type=circle> The condition for the t-test will be satisfied if the sample sizes for the two Gamma population distributions is large enough (at least 20-30). </li>
         <li type=circle> The condition for the WMW test is satisfied because the two population distributions have the same shape.</li>")
  })
  
  output$twohistgams = renderPlot({
    input$start3
    isolate({
    dat = data.frame(t=res.comp2()[[3]],wmw=res.comp2()[[4]],t.ind=res.comp2()[[5]],wmw.ind=res.comp2()[[6]])
    
    if(sum(dat$t.ind)==length(dat$t.ind) & gam.means()[1]!=gam.means()[2])
    {
      dat$t.ind1 = factor(dat$t.ind,label="Rejected Ho")
      lab1 = "seagreen3"
    } else if(sum(dat$t.ind)==0 & gam.means()[1]==gam.means()[2])
    {
      dat$t.ind1 = factor(dat$t.ind,label="Failed to reject Ho")
      lab1 = "seagreen3"
    } else if(sum(dat$t.ind)!=length(dat$t.ind) & gam.means()[1]==gam.means()[2])
    {
      dat$t.ind1 = factor(dat$t.ind,label=c("Failed to reject Ho", "Rejected Ho"))
      lab1 = c("seagreen3","tomato")
    } else if(sum(dat$t.ind)!=0 & gam.means()[1]!=gam.means()[2])
    {
      dat$t.ind1 = factor(dat$t.ind,label=c("Failed to reject Ho", "Rejected Ho"))
      lab1 = c("tomato","seagreen3")
    }
    
    if(sum(dat$wmw.ind)==length(dat$wmw.ind) & gam.means()[1]!=gam.means()[2])
    {
      dat$wmw.ind1 = factor(dat$wmw.ind,label="Rejected Ho")
      lab2 = "seagreen3"
    } else if(sum(dat$wmw.ind)==0 & gam.means()[1]==gam.means()[2])
    {
      dat$wmw.ind1 = factor(dat$wmw.ind,label="Failed to reject Ho")
      lab2 = "seagreen3"
    } else if(sum(dat$wmw.ind)!=length(dat$wmw.ind) & gam.means()[1]==gam.means()[2])
    {
      dat$wmw.ind1 = factor(dat$wmw.ind,label=c("Failed to reject Ho", "Rejected Ho"))
      lab2 = c("seagreen3","tomato")
    } else if(sum(dat$wmw.ind)!=0 & gam.means()[1]!=gam.means()[2])
    {
      dat$wmw.ind1 = factor(dat$wmw.ind,label=c("Failed to reject Ho", "Rejected Ho"))
      lab2 = c("tomato","seagreen3")
    }
    })
    
    g1 = ggplot(dat) + geom_histogram(aes(x=t,fill=t.ind1), alpha=.7) + xlab("t-statistic") + ylab("Frequency") +  
      scale_fill_manual(name="Decision", values=lab1) + theme_bw() + theme(legend.position="bottom") +
      ggtitle("Histogram of t-statistics") 
    g2 = ggplot(dat) + geom_histogram(aes(x=wmw,fill=wmw.ind1), alpha=.7) + xlab("WMW statistic") + ylab("Frequency") +
      scale_fill_manual(name="Decision", values=lab2) + theme_bw() + theme(legend.position="bottom") +
      ggtitle("Histogram of WMW statistics") 
    g3 = arrangeGrob(g1,g2,nrow=1)
    return(g3)
  })
  
  output$text3 = renderUI({
    input$start3
    isolate({
    p(HTML("<li type=square> Number of correct t-statistics "),code(round(res.comp2()[[1]]*input$nsim3)),HTML("</li>"),
      HTML("<li type=square> Number of correct WMW statistics ="),code(round(res.comp2()[[2]]*input$nsim3)),HTML("</li>"),
      HTML("<li type=square> Number of simulations ="),code(input$nsim3))
    })
  })

  output$type1powertext = renderUI({
    input$start3
    isolate({
      if(input$far1==0)
      {
        return(strong("Type I error rate:"))
      } else if(input$far1>0)
      {
        return(strong("Power:"))
      }
    })
  })
  
#   output$table3 = renderTable({
#     input$start3
#     isolate({
#     if(input$start3>0 & gam.means()[1]==gam.means()[2])
#     {
#       mat = matrix(c(res.comp2()[[1]],res.comp2()[[2]]),nrow=1,byrow=TRUE)
#       rownames(mat) = "Type I error rate"
#       colnames(mat) = c("t-test","WMW test")
#       return(mat)
#     } else if(input$start3>0 & (gam.means()[1]!=gam.means()[2]))
#     {
#       mat = matrix(c(res.comp2()[[1]],res.comp2()[[2]]),nrow=1,byrow=TRUE)
#       rownames(mat) = "Power"
#       colnames(mat) = c("t-test","WMW test")
#       return(mat)
#     }
#     })
#   })

  output$gauge3 = renderGvis({
    input$start3
    isolate({
      dat = data.frame(test=c("t-test","WMW"),prob=round(c(res.comp2()[[1]],res.comp2()[[2]]),digits=3))
      if(gam.means()[1]!=gam.means()[2])
      {
        gauge=gvisGauge(dat, options=list(min=0,max=1,greenFrom=.75,greenTo=1,redFrom=0,redTo=.25,width=250,height=250))
      } else if(gam.means()[1]==gam.means()[2])
      {
        gauge=gvisGauge(dat, options=list(min=0,max=1,greenFrom=0,greenTo=input$alpha3,
                                          yellowFrom=input$alpha3,yellowTo=input$alpha3*2,
                                          redFrom=input$alpha3*2,redTo=1,width=250,height=250))
      }
      return(gauge)
    })
  })
  
  res.vary2 = reactive({
    input$start31
    isolate({
      comp.vary.skewed(farvary=input$far2,n.gam1=input$ngam21,n.gam2=input$ngam31,
                       nsim=input$nsim31,shape=input$shape21,scale=input$scale21,alpha=input$alpha31)
    })
  })
  
  output$graph.gamgam = renderGvis({
    input$start31
    isolate({
      if(input$start31>0)
      {
        res.dat = data.frame(mean=seq((input$shape21*input$scale21)+input$far2[1],(input$shape21*input$scale21)+input$far2[2],length.out=length(res.vary2()[[1]])),
                               t=round(res.vary2()[[1]],digits=2),wmw=round(res.vary2()[[2]],digits=2))
          
        gvisLineChart(res.dat,
                      options=list(title="Comparing Type 1 error rate and power between t-test and WMW test",
                                   hAxes="[{title:'Mean of 2nd Gamma distribution'}]",
                                   vAxes="[{title:'P(rejecting Ho)'}]",
                                   width=800,height=600,series="[{color:'#F08080'},{color:'navy'}]"))
      
#       print(ggplot(data=res.dat) + geom_line(aes(x=mean,y=t.error,color="t-test")) +
#               geom_line(aes(x=mean,y=wmw.error,color="WMW")) + xlab("Mean of 2nd Gamma distribution") + ylab("P(rejecting Ho)") +
#               scale_color_manual(name="Type of test",values=c("t-test"="navy","WMW"="gold2")) + theme(legend.position="bottom") + 
#               ggtitle("Comparing Type 1 error rate and\npower between t-test and WMW test"))
      }
    })
  })
})