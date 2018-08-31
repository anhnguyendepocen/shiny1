##############################################
# App Title: Probability Distribution Viewer #
#           Author: Irvin Alcaraz            #
##############################################

###################
# Misc Functions  #
###################

library(shinysky)
library(shiny)

##Function to convert all values to xvalues so I can standardly
##use the density distribution functions
xvalue = function(value,type,dist,params){
  
  switch(type,
         p = do.call(paste("q",dist,sep=""),c(value,params)),
         d = value
  )
  
}

###################
#Shiny Server Code#
###################
shinyServer(function(input, output, session) {

  observe({
    if(input$dist == "beta"){
      if(input$p1.beta <= 0 || input$p2.beta <= 0 || is.na(input$p1.beta) || is.na(input$p2.beta)){
        showshinyalert(session,"shinyalert1",
                       "Looks like something is wrong in your parameters, please check them and then hit submit again",
                       "danger")
      }else return ()
    }else return ()
  })
  
  observe({
    if(input$dist == "cauchy"){
      if(input$p2.cauchy <= 0 || is.na(input$p2.cauchy)){
        showshinyalert(session,"shinyalert2",
                       "Looks like something is wrong in your parameters, please check them and then hit submit again",
                       "danger")
      }else return()
    }else return()
  })
  
  observe({
    if(input$dist == "chisq"){
      if(input$p1.chisq <= 0 || is.na(input$p1.chisq)){
        showshinyalert(session,"shinyalert3",
                       "Looks like something is wrong in your parameters, please check them and then hit submit again",
                       "danger")
      }else return()
    }else return()
  })
  
  observe({
    if(input$dist == "exp"){
      if(input$p1.exp <= 0 || is.na(input$p1.exp)){
        showshinyalert(session,"shinyalert4",
                       "Looks like something is wrong in your parameters, please check them and then hit submit again",
                       "danger")
      }else return()
    }else return()
  })
  
  observe({
    if(input$dist == "f"){
      if(input$p1.f <= 0 || input$p2.f <= 0 || is.na(input$p1.f) || is.na(input$p2.f)){
        showshinyalert(session,"shinyalert5",
                       "Looks like something is wrong in your parameters, please check them and then hit submit again",
                       "danger")
      }else return()
    }else return()
  })
  
  observe({
    if(input$dist == "gamma"){
      if(input$p1.gamma <= 0 || input$p2.gamma <= 0 || is.na(input$p1.gamma) || is.na(input$p2.gamma)){
        showshinyalert(session,"shinyalert6",
                       "Looks like something is wrong in your parameters, please check them and then hit submit again",
                       "danger")
      }else return()
    }else return()
  })
  
  observe({
    if(input$dist == "logis"){
      if(input$p2.logis <= 0 || is.na(input$p2.logis)){
        showshinyalert(session,"shinyalert7",
                       "Looks like something is wrong in your parameters, please check them and then hit submit again",
                       "danger")
      }else return()
    }else return()
  })
  
  observe({
    if(input$dist == "lnorm"){
      if(input$p1.lnorm <= 0 || input$p2.lnorm <= 0 || is.na(input$p1.lnorm) || is.na(input$p2.lnorm)){
        showshinyalert(session,"shinyalert8",
                       "Looks like something is wrong in your parameters, please check them and then hit submit again",
                       "danger")
      }else return()
    }else return()
  })
  
  observe({
    input$go
    isolate({
      if(input$dist == "t"){
         if(input$p1.t <= 0 || is.na(input$p1.t)){
        showshinyalert(session,"shinyalert9",
                       "Looks like something is wrong in your parameters, please check them and then hit submit again",
                       "danger")
        }else return()
      }else return()
    })
  })
  
  observe({
    if(input$dist == "weibull"){
      if(input$p1.weibull <= 0 || input$p2.weibull <= 0 || is.na(input$p1.weibull) || is.na(input$p2.weibull)){
        showshinyalert(session,"shinyalert10",
                       "Looks like something is wrong in your parameters, please check them and then hit submit again",
                       "danger")
      }else return()
    }else return()
  })
  
  observe({
    if(input$dist == "norm"){
      if(input$p2.norm <= 0 || is.na(input$p1.norm) || is.na(input$p2.norm)){
        showshinyalert(session,"shinyalert11",
                       "Looks like something is wrong in your parameters, please check them and then hit submit again",
                       "danger")
      }else return()
    }else return()
  })
  
  output$distPlot <- renderPlot({
    input$go
    
    isolate({

##Each distribution has a different number of parameters 
##so I create lists that contain them based on the distribution.
##I omitted the ability to have a threshold and NCP for all distributions.
##I originally had very general code for this but it ended up causing to many
##problems so this method works.
    params = switch(input$dist,
                    "beta"= list(input$p1.beta,input$p2.beta),
                    "cauchy" = list(input$p1.cauchy,input$p2.cauchy),
                    "chisq"=list(input$p1.chisq),
                    "exp" = list(input$p1.exp),
                    "f"=list(input$p1.f,input$p2.f),             
       		          "gamma"= list(input$p1.gamma,input$p2.gamma),
                    "logis" = list(input$p1.logis,input$p2.logis),
                    "lnorm" = list(input$p1.lnorm,input$p2.lnorm),
                    "norm" = list(input$p1.norm,input$p2.norm),
                    "t" = list(input$p1.t),
                    "unif" = list(input$p1.unif,input$p2.unif),
                    "weibull" = list(input$p1.weibull,input$p2.weibull)
                    )
    
##Distributions I still can implement
    ##Continuous
    ##tukey

    ##Discrete
    ##binom geom hypergeometric nbinom pois
    
    ##Nonparametric
    ##Wilcoxon

##These if statements take the value to be shaded and converts it
##to xvalues that will later be used by the polygon function
    if (!is.null("input$shadeval2")){
      xvalue1 = xvalue(input$shadeval1,input$type,input$dist,params)      
      xvalue2 = xvalue(input$shadeval2,input$type,input$dist,params)
    }else{
      xvalue1 = xvalue(input$shadeval1,input$type,input$dist,params)  
    }


##These if statements draw the graphs based on the different distributions
##Some Distributions were easier to graph through a more specific method so 
##they are separated from the others
	if (input$dist == 'beta'
	    || input$dist == 'logis' || input$dist == 'norm' 
	    || input$dist == 't' || input$dist == 'unif'){

    minx = do.call(paste("q",input$dist,sep=""),c(.0001,params))
    maxx = do.call(paste("q",input$dist,sep=""),c(.9999,params))
    x = seq(from=minx,to=maxx,length=1000)
    hx = x
    
    for(k in 1:1000){  
      hx[k] = do.call(paste("d",input$dist,sep=""),c(x[k],params))
    }
    
    miny = 0
    miny = 0
    if (is.infinite(max(hx)) || max(hx)>1)
    {
      maxy = 1
    }else{
      maxy = round(max(hx),digits=2)      
    }
    
    plot(x,hx,type="n",xlab="X",ylab="Density",
         main="Probability Density",axes=FALSE,ylim=c(miny,maxy))
    lines(x,hx)
#     axis(1,pos=0,col.axis="grey",col.ticks="grey",col="grey")
    axis(1,pos=0)
    axis(2,at=round(seq(from=miny,to=maxy,length=5),digits=3),pos=minx)  
    
	} else if (input$dist == 'chisq' || input$dist == 'exp' 
	  	    || input$dist == 'f'    || input$dist == 'gamma'
		    || input$dist == 'lnorm' || input$dist == 'weibull'){

    minx = 0
    maxx = do.call(paste("q",input$dist,sep=""),c(.999,params))
    x = NULL
    x = seq(from=minx,to=maxx,length=1000)
    hx = x
    
    for(k in 1:1000){  
      hx[k] = do.call(paste("d",input$dist,sep=""),c(x[k],params))
    }
    
    miny = 0
    if (is.infinite(max(hx)) || max(hx)>1)
    {
      maxy = 1
    }else{
      maxy = round(max(hx),digits=2)      
    }
    
    plot(x,hx,type="n",xlab="X",ylab="Density",
         main="Probability Density",axes=FALSE,ylim=c(miny,maxy))
    lines(x,hx)
    axis(1,pos=0)
    axis(2,at=round(seq(from=miny,to=maxy,length=5),digits=3),pos=minx)  

	} else if (input$dist == 'cauchy'){
    
	  minx = do.call(paste("q",input$dist,sep=""),c(.04,params))
	  maxx = do.call(paste("q",input$dist,sep=""),c(.96,params))
      x = NULL
	  x = seq(from=minx,to=maxx,length=1000)
	  hx = x
	  
	  for(k in 1:1000){  
	    hx[k] = do.call(paste("d",input$dist,sep=""),c(x[k],params))
	  }
	  
	  miny = 0
	  if (is.infinite(max(hx)) || max(hx)>1)
	  {
	    maxy = 1
	  }else{
	    maxy = round(max(hx),digits=2)      
	  }
    
	  plot(x,hx,type="n",xlab="X",ylab="Density",
	       main="Probability Density",axes=FALSE,ylim=c(miny,maxy))
	  lines(x,hx)
	  axis(1,pos=0)
	  axis(2,at=round(seq(from=miny,to=maxy,length=5),digits=3),pos=minx)  
	  
	} 

# # Debug
# plot.new()    
# title(paste(maxx))
if (input$type != 'none'){
    
# These if statements shade the graphs correctly
	if(input$shade=="left"){
	  
	  i = x<=xvalue1
	  polygon(c(minx,x[i],xvalue1),c(0,hx[i],0),col="deepskyblue3")
	  area = do.call(paste("p",input$dist,sep=""),c(xvalue1,params))
	  result = paste("P(X<",signif(xvalue1,digits=4),")=",signif(area,digits=3))
	  mtext(result,3)
    axis(1,at=xvalue1,pos=0,col.ticks="red",col.axis="red",lwd.ticks = 2,cex.axis=2)
              
	}else if(input$shade=="right"){

	  i = x>=xvalue1	
	  polygon(c(xvalue1,x[i],maxx),c(0,hx[i],0),col="deepskyblue3")
	  area = 1-do.call(paste("p",input$dist,sep=""),c(xvalue1,params))
	  result = paste("P(X>",signif(xvalue1,digits=4),")=",signif(area,digits=3))
	  mtext(result,3)
	  axis(1,at=xvalue1,pos=0,col.ticks="red",col.axis="red",lwd.ticks = 2,cex.axis=2)
    
	}else if(input$shade=="middle"){
 
    i = (x>=xvalue1)&(x<=xvalue2)
    polygon(c(xvalue1,x[i],xvalue2),c(0,hx[i],0),col="deepskyblue3")    
    area = do.call(paste("p",input$dist,sep=""),c(xvalue2,params))-
      do.call(paste("p",input$dist,sep=""),c(xvalue1,params))
    result = paste("P(",signif(xvalue1,digits=4),"< X <",signif(xvalue2,digits=4),
                   ")=",signif(area,digits=3))
    mtext(result,3)
	  axis(1,at=c(xvalue1,xvalue2),pos=0,col.ticks="red",col.axis="red",lwd.ticks = 2,,cex.axis=2)
    
	}else if(input$shade=="both"){

    i = (x<=xvalue1)
    j = (x>=xvalue2)
    polygon(c(minx,x[i],xvalue1),c(0,hx[i],0),col="deepskyblue3")
    polygon(c(xvalue2,x[j],maxx),c(0,hx[j],0),col="deepskyblue3")
    
    area = (1-do.call(paste("p",input$dist,sep=""),c(xvalue2,params)))+
	    do.call(paste("p",input$dist,sep=""),c(xvalue1,params))
	  result = paste("P(X < ",signif(xvalue1,digits=4)," or X > ",signif(xvalue2,digits=4),
	                 ")=",signif(area,digits=3))
	  mtext(result,3)
	  axis(1,at=c(xvalue1,xvalue2),pos=0,col.ticks="red",col.axis="red",lwd.ticks = 2,cex.axis=2)
	}
  

}
    })
  })

})
