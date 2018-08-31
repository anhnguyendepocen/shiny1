# ----------------------------------------
#  App Title: Chaos Game -- 2 Dimensions
#     Author: Jimmy Doi
# ----------------------------------------

library(shiny)
library(shinyBS)
library(shape)

###################################################################
# Triangle
###################################################################

tri.gen <- function(wt){
  	weight <- wt
		
		len <- 50000
		
    # loci matrix to contain all endpoints
		loci <- matrix(NA,ncol=3,nrow=3)
		
		loci[1,] <- c(1,0,0)
		loci[2,] <- c(2,0.5,sqrt(3)/2)
		loci[3,] <- c(3,1,0)

    # vertices contains all random vertex points
    vertices <- runif(len)
		vertices[which(vertices>2/3)]<- 3
		vertices[which(1/3<vertices & vertices<=2/3)]<- 2
		vertices[which(vertices<=1/3)]<- 1
		
		coords <- matrix(NA,ncol=2,nrow=(len+1))
		colnames(coords)<-c("x","y") #needed for ggvis
		
		coords[1,] <- c(runif(1),runif(1)*sqrt(3)/2)
		
		for (i in 1:len){
		  row <- i+1
      spot <- which(loci[,1]==vertices[i])
      x <- loci[spot,2]
      y <- loci[spot,3]
      x.new <- weight*x + (1-weight)*coords[i,1]
      y.new <- weight*y + (1-weight)*coords[i,2]
	    coords[row,]<-c(x.new,y.new)
	    x <- x.new
	    y <- y.new
		}
    return(list(loci,vertices,coords))
}


###################################################################
# Square
###################################################################

sqr.gen <- function(wt){
  
    weight <- wt
    
		len <- 50000
		
    # loci matrix to contain all endpoints
		loci <- matrix(NA,ncol=3,nrow=8)
		
		loci[1,] <- c(1,0.0,0.0)
		loci[2,] <- c(2,0.5,0.0)
		loci[3,] <- c(3,1.0,0.0)
  	loci[4,] <- c(4,1.0,0.5)
  	loci[5,] <- c(5,1.0,1.0)
  	loci[6,] <- c(6,0.5,1.0)
  	loci[7,] <- c(7,0.0,1.0)
  	loci[8,] <- c(8,0.0,0.5)

    # vertices contains all random vertex points
    vertices <- runif(len)
		vertices[which(vertices>7/8)]<- 8
  	vertices[which(6/8<vertices & vertices<=7/8)]<- 7
  	vertices[which(5/8<vertices & vertices<=6/8)]<- 6
  	vertices[which(4/8<vertices & vertices<=5/8)]<- 5
  	vertices[which(3/8<vertices & vertices<=4/8)]<- 4
  	vertices[which(2/8<vertices & vertices<=3/8)]<- 3
		vertices[which(1/8<vertices & vertices<=2/8)]<- 2
		vertices[which(vertices<=1/8)]<- 1
		
		coords <- matrix(NA,ncol=2,nrow=(len+1))
		colnames(coords)<-c("x","y") #needed for ggvis
		
    # randomly selected initial point in field of view
		coords[1,] <- c(runif(1),runif(1))
		
  	for (i in 1:len){
		  row <- i+1
      spot <- which(loci[,1]==vertices[i])
      x <- loci[spot,2]
      y <- loci[spot,3]
      x.new <- (weight)*x + (1-weight)*coords[i,1]
	    y.new <- (weight)*y + (1-weight)*coords[i,2]
	    coords[row,]<-c(x.new,y.new)
		}
    
    return(list(loci,vertices,coords))
}


###################################################################
# Pentagon
###################################################################

pent.gen <- function(wt){
  
    weight <- wt
    
  	len <- 50000
		
    # loci matrix to contain all endpoints
		loci <- matrix(NA,ncol=3,nrow=5)
    
    c1 <- 0.25*(sqrt(5)-1)
    c2 <- 0.25*(sqrt(5)+1)
    s1 <- 0.25*(sqrt(10+2*sqrt(5)))
    s2 <- 0.25*(sqrt(10-2*sqrt(5)))
		
		loci[1,] <- c(1,0,1)
		loci[2,] <- c(2,s1,c1)
		loci[3,] <- c(3,s2,-c2)
  	loci[4,] <- c(4,-s2,-c2)
  	loci[5,] <- c(5,-s1,c1)

    # vertices contains all random vertex points
    vertices <- runif(len)
		vertices[which(vertices>4/5)]<- 5
  	vertices[which(3/5<vertices & vertices<=4/5)]<- 4
  	vertices[which(2/5<vertices & vertices<=3/5)]<- 3
		vertices[which(1/5<vertices & vertices<=2/5)]<- 2
		vertices[which(vertices<=1/5)]<- 1
		
		coords <- matrix(NA,ncol=2,nrow=(len+1))
		colnames(coords)<-c("x","y") #needed for ggvis
		
    # randomly selected initial point in field of view
		coords[1,] <- c(runif(1,-s1,s1),runif(1,-c2,1))
		
  	for (i in 1:len){
		  row <- i+1
      spot <- which(loci[,1]==vertices[i])
      x <- loci[spot,2]
      y <- loci[spot,3]
      x.new <- (weight)*x + (1-weight)*coords[i,1]
	    y.new <- (weight)*y + (1-weight)*coords[i,2]
	    coords[row,]<-c(x.new,y.new)
		}
    
    return(list(loci,vertices,coords))
}

###################################################################
# Hexagon
###################################################################

hex.gen <- function(wt){
  
    weight <- wt
    
    len <- 50000
		
    # loci matrix to contain all endpoints
		loci <- matrix(NA,ncol=3,nrow=6)

    alpha <- 0.5
    beta  <- sqrt(3)/2
		
		loci[1,] <- c(1,0,2*alpha)
		loci[2,] <- c(2,beta,alpha)
		loci[3,] <- c(3,beta,-alpha)
  	loci[4,] <- c(4,0,-2*alpha)
  	loci[5,] <- c(5,-beta,-alpha)
    loci[6,] <- c(6,-beta,alpha)

    # vertices contains all random vertex points
    vertices <- runif(len)
		vertices[which(vertices>5/6)]<- 6
    vertices[which(4/6<vertices & vertices<=5/6)]<- 5
  	vertices[which(3/6<vertices & vertices<=4/6)]<- 4
  	vertices[which(2/6<vertices & vertices<=3/6)]<- 3
		vertices[which(1/6<vertices & vertices<=2/6)]<- 2
		vertices[which(vertices<=1/6)]<- 1
		
		coords <- matrix(NA,ncol=2,nrow=(len+1))
		colnames(coords)<-c("x","y") #needed for ggvis
		
    # randomly selected initial point in field of view
		coords[1,] <- c(runif(1,-beta,beta),runif(1,-2*alpha,2*alpha))
		
  	for (i in 1:len){
		  row <- i+1
      spot <- which(loci[,1]==vertices[i])
      x <- loci[spot,2]
      y <- loci[spot,3]
      x.new <- (weight)*x + (1-weight)*coords[i,1]
	    y.new <- (weight)*y + (1-weight)*coords[i,2]
	    coords[row,]<-c(x.new,y.new)
		}
    
    return(list(loci,vertices,coords))
}

##############################################################################
# Shiny Server Contents 
##############################################################################

shinyServer(function(input, output, session) {

output$my.init <- renderUI({
  input$shape
    sliderInput(inputId = "init",
      "Number of points (n):",
      min = 1,
      max = 100,
      step = 1,
      value = 1,
      animate=animationOptions(interval = 1000))
  })

output$my.extend <- renderUI({
  input$shape
    sliderInput("extend",
      "Number of points (n):",
      min = 100,
      max = 1000,
      step = 25,
      value = 100,
      animate=animationOptions(interval = 400))
  })

output$my.pts <- renderUI({
  input$shape
    sliderInput("pts",
      "Number of points (n):",
      min = 1000,
      max = 50000,
      step = 1000,
      value = 1000,
      animate=animationOptions(interval = 200))
    })
  
  updateButton(session, "gen", style = "primary", size = "default", disabled = FALSE)
  
  all.list <- reactive({
    if (input$shape == "tri") {
      return(tri.gen(input$dist.tri*(input$gen>-1)))
      }
    if (input$shape == "sqr") {
      return(sqr.gen(input$dist.sqr*(input$gen>-1)))
      }
    if (input$shape == "pent") {
      return(pent.gen(input$dist.pent*(input$gen>-1)))
      }
    if (input$shape == "hex") {
      return(hex.gen(input$dist.hex*(input$gen>-1)))
      }
    })  
  

##################################
# initPlot                       #
##################################

  output$initPlot <- renderPlot({
    
    loci      <- all.list()[[1]]
    vertices  <- all.list()[[2]]
    coords    <- all.list()[[3]]
    
    #############################
    # Triangle:INIT             #
    #############################
    if (input$shape == "tri") {
      
        par(mar=c(0.5,0.5,0.5,0.5))
    plot(0,0,xlim=c(0,1),ylim=c(0,sqrt(3)/2),col=0,
            yaxt="n",xaxt="n",xlab="",ylab="",bty="n")
    
    
    if (!is.null(input$init)) {
      if (input$init==1) {
      points(coords[1,1],coords[1,2],pch=20,cex=3,col="blue")  
      
      if (coords[1,1]>=0.5 & coords[1,2]<=sqrt(3)/4) {
        text(coords[1,1],coords[1,2]+0.04,"Random Starting Point",col="blue",pos=2)
      }
      if (coords[1,1]>=0.5 & coords[1,2]>sqrt(3)/4) {
        text(coords[1,1],coords[1,2]-0.04,"Random Starting Point",col="blue",pos=2)
      }
      if (coords[1,1]<0.5 & coords[1,2]>sqrt(3)/4) {
        text(coords[1,1],coords[1,2]-0.04,"Random Starting Point",col="blue",pos=4)
      }
      if (coords[1,1]<0.5 & coords[1,2]<=sqrt(3)/4) {
        text(coords[1,1],coords[1,2]+0.04,"Random Starting Point",col="blue",pos=4)
      }
    }
    }
    }
    
    #############################
    # Square:INIT               #
    #############################
    if (input$shape == "sqr") {
    
          par(mar=c(0.5,0.5,0.5,0.5))
      plot(0,0,xlim=c(0,1),ylim=c(0,1),col=0,
            yaxt="n",xaxt="n",xlab="",ylab="",bty="n")

    if (input$init==1) {
      points(coords[1,1],coords[1,2],pch=20,cex=3,col="blue")  
      
      if (coords[1,1]>=0.5 & coords[1,2]<=0.5) { # LOWER RIGHT
        text(coords[1,1],coords[1,2]+0.04,"Random Starting Point",col="blue",pos=2)
      }
      if (coords[1,1]>=0.5 & coords[1,2]>0.5) {  # UPPER RIGHT
        text(coords[1,1],coords[1,2]-0.04,"Random Starting Point",col="blue",pos=2)
      }
      if (coords[1,1]<0.5 & coords[1,2]>0.5) {   # UPPER LEFT
        text(coords[1,1],coords[1,2]-0.04,"Random Starting Point",col="blue",pos=4)
      }
      if (coords[1,1]<0.5 & coords[1,2]<=0.5) {  # LOWER LEFT
        text(coords[1,1],coords[1,2]+0.04,"Random Starting Point",col="blue",pos=4)
      }
    }
    }
    
    #############################
    # Pentagon:INIT             #
    #############################
    if (input$shape == "pent") {
    
    c1 <- 0.25*(sqrt(5)-1)
    c2 <- 0.25*(sqrt(5)+1)
    s1 <- 0.25*(sqrt(10+2*sqrt(5)))
    s2 <- 0.25*(sqrt(10-2*sqrt(5)))

      par(mar=c(0.5,0.5,0.5,0.5))
      plot(0,0,xlim=c(-s1,s1),ylim=c(-c2,1),col=0,
            yaxt="n",xaxt="n",xlab="",ylab="",bty="n")

    if (input$init==1) {
      points(coords[1,1],coords[1,2],pch=20,cex=3,col="blue")  
      
      if (coords[1,1]>=0 & coords[1,2]<=0) { # LOWER RIGHT
        text(coords[1,1],coords[1,2]+0.04,"Random Starting Point",col="blue",pos=2)
      }
      if (coords[1,1]>=0 & coords[1,2]>0) {  # UPPER RIGHT
        text(coords[1,1],coords[1,2]-0.04,"Random Starting Point",col="blue",pos=2)
      }
      if (coords[1,1]<0 & coords[1,2]>0) {   # UPPER LEFT
        text(coords[1,1],coords[1,2]-0.04,"Random Starting Point",col="blue",pos=4)
      }
      if (coords[1,1]<0 & coords[1,2]<=0) {  # LOWER LEFT
        text(coords[1,1],coords[1,2]+0.04,"Random Starting Point",col="blue",pos=4)
      }
    }
    }

    #############################
    # Hexagon:INIT              #
    #############################
    if (input$shape == "hex") {
    
      alpha <- 0.5
      beta  <- sqrt(3)/2

      par(mar=c(0.5,0.5,0.5,0.5))
      plot(0,0,xlim=c(-beta,beta),ylim=c(-2*alpha,2*alpha),col=0,
            yaxt="n",xaxt="n",xlab="",ylab="",bty="n")

    if (input$init==1) {
      points(coords[1,1],coords[1,2],pch=20,cex=3,col="blue")  
      
      if (coords[1,1]>=0 & coords[1,2]<=0) { # LOWER RIGHT
        text(coords[1,1],coords[1,2]+0.04,"Random Starting Point",col="blue",pos=2)
      }
      if (coords[1,1]>=0 & coords[1,2]>0) {  # UPPER RIGHT
        text(coords[1,1],coords[1,2]-0.04,"Random Starting Point",col="blue",pos=2)
      }
      if (coords[1,1]<0 & coords[1,2]>0) {   # UPPER LEFT
        text(coords[1,1],coords[1,2]-0.04,"Random Starting Point",col="blue",pos=4)
      }
      if (coords[1,1]<0 & coords[1,2]<=0) {  # LOWER LEFT
        text(coords[1,1],coords[1,2]+0.04,"Random Starting Point",col="blue",pos=4)
      }
    }
    }
    ##################################################################################
    ### APPLIED TO ALL    

    if (!is.null(input$init)) {
      if (input$init!=1) {
    points(coords[1:input$init-1,1],coords[1:input$init-1,2],pch=20,cex=1,col="blue")  
    
    points(coords[input$init-1,1],coords[input$init-1,2],pch=20,cex=2.75,col="blue")  
    points(coords[input$init,1],coords[input$init,2],pch=21,cex=3,col="blue",bg="white")  
    points(coords[input$init,1],coords[input$init,2],pch=20,cex=2.75,col="blue")  
    
    x0 <- coords[input$init-1,1]
    y0 <- coords[input$init-1,2]
    x1 <- coords[input$init,1]
    y1 <- coords[input$init,2]
    
    Arrows((.6*x0+.4*x1),(.6*y0+.4*y1),(.4*x0+.6*x1),(.4*y0+.6*y1),col="blue",lwd=2)
    
    v.x <- loci[loci[,1]==vertices[input$init-1],2]
    v.y <- loci[loci[,1]==vertices[input$init-1],3]

    points(v.x,v.y,pch=1,cex=4,lwd=2)
    points(v.x,v.y,pch=1,cex=3,lwd=2)
    }
        }
    
    points(loci[,2],loci[,3],pch=20,cex=2,col="red")

    
  }) # initPlot's renderPlot

##################################
# extendPlot                     #
##################################
  output$extendPlot <- renderPlot({

    loci      <- all.list()[[1]]
    vertices  <- all.list()[[2]]
    coords    <- all.list()[[3]]

    #############################
    # Triangle:EXTEND           #
    #############################
    if (input$shape == "tri") {
      
        par(mar=c(0.5,0.5,0.5,0.5))
    plot(0,0,xlim=c(0,1),ylim=c(0,sqrt(3)/2),col=0,
            yaxt="n",xaxt="n",xlab="",ylab="",bty="n")
    }
    
    #############################
    # Square:EXTEND             #
    #############################
    if (input$shape == "sqr") {
      
        par(mar=c(0.5,0.5,0.5,0.5))
    plot(0,0,xlim=c(0,1),ylim=c(0,1),col=0,
            yaxt="n",xaxt="n",xlab="",ylab="",bty="n")
    }
    
    #############################
    # Pentagon:EXTEND           #
    #############################
    if (input$shape == "pent") {
      
    c1 <- 0.25*(sqrt(5)-1)
    c2 <- 0.25*(sqrt(5)+1)
    s1 <- 0.25*(sqrt(10+2*sqrt(5)))
    s2 <- 0.25*(sqrt(10-2*sqrt(5)))

    par(mar=c(0.5,0.5,0.5,0.5))
      plot(0,0,xlim=c(-s1,s1),ylim=c(-c2,1),col=0,
            yaxt="n",xaxt="n",xlab="",ylab="",bty="n")
    }

    #############################
    # Hexagon:EXTEND            #
    #############################
    if (input$shape == "hex") {
      
      alpha <- 0.5
      beta  <- sqrt(3)/2

      par(mar=c(0.5,0.5,0.5,0.5))
      plot(0,0,xlim=c(-beta,beta),ylim=c(-2*alpha,2*alpha),col=0,
            yaxt="n",xaxt="n",xlab="",ylab="",bty="n")
    }

    ############################################################################
    ### APPLIED TO ALL
    if (!is.null(input$extend)) {
    if (input$extend!=0) { 
    points(coords[1:input$extend,1],coords[1:input$extend,2],pch=20,cex=1,col="blue")  
    }
    }
    
    points(loci[,2],loci[,3],pch=20,cex=2,col="red")
    
  }) # extendPlot's renderPlot

##################################
# compPlot                       #
##################################
  output$compPlot <- renderPlot({
    
    loci      <- all.list()[[1]]
    vertices  <- all.list()[[2]]
    coords    <- all.list()[[3]]

    #############################
    # Triangle:COMPLETE         #
    #############################
    if (input$shape == "tri") {

        par(mar=c(0.5,0.5,0.5,0.5))
    plot(0,0,xlim=c(0,1),ylim=c(0,sqrt(3)/2),col=0,
            yaxt="n",xaxt="n",xlab="",ylab="",bty="n")
    }
    
    #############################
    # Square:COMPLETE           #
    #############################
    if (input$shape == "sqr") {

        par(mar=c(0.5,0.5,0.5,0.5))
    plot(0,0,xlim=c(0,1),ylim=c(0,1),col=0,
            yaxt="n",xaxt="n",xlab="",ylab="",bty="n")
    }


    #############################
    # Pentagon:COMPLETE         #
    #############################
    if (input$shape == "pent") {

      c1 <- 0.25*(sqrt(5)-1)
      c2 <- 0.25*(sqrt(5)+1)
      s1 <- 0.25*(sqrt(10+2*sqrt(5)))
      s2 <- 0.25*(sqrt(10-2*sqrt(5)))

      par(mar=c(0.5,0.5,0.5,0.5))
      plot(0,0,xlim=c(-s1,s1),ylim=c(-c2,1),col=0,
            yaxt="n",xaxt="n",xlab="",ylab="",bty="n")
    }
    
    
    #############################
    # Hexagon:COMPLETE          #
    #############################
    if (input$shape == "hex") {

      alpha <- 0.5
      beta  <- sqrt(3)/2

      par(mar=c(0.5,0.5,0.5,0.5))
      plot(0,0,xlim=c(-beta,beta),ylim=c(-2*alpha,2*alpha),col=0,
            yaxt="n",xaxt="n",xlab="",ylab="",bty="n")
    }

    ##############################################################################  
    ### APPLIED TO ALL    
    if (!is.null(input$pts)) { 
      if (input$pts!=0) { 
    points(coords[1:input$pts,1],coords[1:input$pts,2],pch=".",cex=2.5,col="blue")  
      }
    }
    points(loci[,2],loci[,3],pch=20,cex=2,col="red")

  }) # compPlot's renderPlot
  
  
})

