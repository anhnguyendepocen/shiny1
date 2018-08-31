# ----------------------------------------
#  App Title: Benford's Law and Sequences
#     Author: Jimmy Doi
# ----------------------------------------

library(RColorBrewer)
library(shinyIncubator)

########################################
seq.gen <- function(length,val.1,val.2){
  
  seq <- numeric(length)
  seq[1] <- val.1
  seq[2] <- val.2
  
  for (i in 3:length) { 
    seq[i] <- seq[i-1]+seq[i-2]
  } 
  return(seq)
}

########################################
pow.gen <- function(base,length){
  
  seq <- base**(1:length)
  
  return(seq)
}

########################################
prime.gen <- function(n){
  n <- as.integer(n)
  if(n > 1e8) stop("n too large")
  primes <- rep(TRUE, n)
  primes[1] <- FALSE
  last.prime <- 2L
  fsqr <- floor(sqrt(n))
  while (last.prime <= fsqr)
  {
    primes[seq.int(2L*last.prime, n, last.prime)] <- FALSE
    sel <- which(primes[(last.prime+1):(fsqr+1)])
    if(any(sel)){
      last.prime <- last.prime + min(sel)
    }else last.prime <- fsqr+1
  }
  which(primes)
}

########################################
string.1st <- function(sequence,max){
  string1 <- substr(as.character(sequence[1:max]),1,1)
  #  string1<-paste(" ",string1,sep="")
  return(string1)
}

########################################
string.2nd <- function(sequence,max){
  string2 <- substr(as.character(sequence[1:max]),2,1000000L)
  string2.a<-paste(string2,", ",sep="")
  string2.a[max]<-string2[max]
  return(string2.a)
}

########################################
print.string <- function(string,num){
  #names(string)<-""
  return(string[num])
}

########################################
goodness.test <- function(sequence){
  
  string <- substr(as.character(sequence),1,1)
  
  first.digits <- as.integer(string)
  
  # In the event any of the 9 possible digits is not observed in the string, 
  # table(first.digits) will not display all digits 1, 2, ..., 9. An example is
  #
  # NAME     1  2  3  4  5  6  7  9
  # FREQ    12 11  9  8  6  5  4  2
  #
  # Here, obs <- as.integer(table(first.digits)) would only contain 8 elements.
  # To correct for this, initialize obs to a zero vector, loop through NAME, 
  # find FREQ corresponding to NAME,then paste that FREQ in obs[NAME].
  # This will also work for the case when all 9 possible digits are observed.
  
  Obs <- seq(0,0,,9)
  
  for (i in as.numeric(names(table(first.digits))))  {
    pos <- which(as.numeric(names(table(first.digits)))==i)
    Obs[i]<- as.numeric(table(first.digits))[pos]
  }  
  i <- seq(1,9)
  
  Exp <- length(sequence)*log10(1+1/i)
  
  Digit <- seq(1,9)
  
  #print(info)
  
  chisq <- sum(((Obs-Exp)**2)/Exp)
  
  Exp <- round(Exp,3)
  info <- cbind(Digit,Obs,Exp)
  rownames(info)<-rep("",nrow(info))
  
  #print(chisq)
  
  p.value <- 1-pchisq(chisq,8)
  
  text.chi <- paste("Chi Square = ",format(round(chisq,3),nsmall=3),sep="")
  names(text.chi)<-""
  
  text.pval <- paste("P Value = ",format(round(p.value,3),nsmall=3),sep="")
  names(text.pval)<-""
  
  print(info)
  print(text.chi,quote=F)
  print(text.pval,quote=F)
  
}

########################################
pr.goodness.test <- function(sequence,big.n){
  
  string <- substr(as.character(sequence),1,1)
  
  first.digits <- as.integer(string)
  
  # In the event any of the 9 possible digits is not observed in the string, 
  # table(first.digits) will not display all digits 1, 2, ..., 9. An example is
  #
  # NAME     1  2  3  4  5  6  7  9
  # FREQ    12 11  9  8  6  5  4  2
  #
  # Here, obs <- as.integer(table(first.digits)) would only contain 8 elements.
  # To correct for this, initialize obs to a zero vector, loop through NAME, 
  # find FREQ corresponding to NAME,then paste that FREQ in obs[NAME].
  # This will also work for the case when all 9 possible digits are observed.
  
  Obs <- seq(0,0,,9)
  
  for (i in as.numeric(names(table(first.digits))))  {
    pos <- which(as.numeric(names(table(first.digits)))==i)
    Obs[i]<- as.numeric(table(first.digits))[pos]
  }  

  i <- seq(1,9)
  
  alpha <- 1/(log(big.n)-1.10)
  
  gbl <- 1/(10**(1-alpha)-1)*((i+1)**(1-alpha)-i**(1-alpha))
  
  Exp <- length(sequence)*gbl
  
  Digit <- seq(1,9)
  
  chisq <- sum(((Obs-Exp)**2)/Exp)
  
  Exp <- round(Exp,3)
  info <- cbind(Digit,Obs,Exp)
  rownames(info)<-rep("",nrow(info))
  
  p.value <- 1-pchisq(chisq,8)
  
  text.chi <- paste("Chi Square = ",format(round(chisq,3),nsmall=3),sep="")
  names(text.chi)<-""
  
  text.pval <- paste("P Value = ",format(round(p.value,3),nsmall=3),sep="")
  names(text.pval)<-""
  
  print(info)
  print(text.chi,quote=F)
  print(text.pval,quote=F)
  
}

############################################
pmf.compare<-function(sequence){
  
  string <- substr(as.character(sequence),1,1)
  
  first.digits <- as.integer(string)
  
  # In the event any of the 9 possible digits is not observed in the string, 
  # table(first.digits) will not display all digits 1, 2, ..., 9. An example is
  #
  # NAME     1  2  3  4  5  6  7  9
  # FREQ    12 11  9  8  6  5  4  2
  #
  # Here, obs <- as.integer(table(first.digits)) would only contain 8 elements.
  # To correct for this, initialize obs to a zero vector, loop through NAME, 
  # find FREQ corresponding to NAME,then paste that FREQ in obs[NAME].
  # This will also work for the case when all 9 possible digits are observed.
  
  Obs <- seq(0,0,,9)
  
  for (i in as.numeric(names(table(first.digits))))  {
    pos <- which(as.numeric(names(table(first.digits)))==i)
    Obs[i]<- as.numeric(table(first.digits))[pos]
  }  
  i <- seq(1,9)
  
  pmf.Exp <- cbind(seq(1,9)+0.065,log10(1+1/i))
  pmf.Obs <- cbind(seq(1,9)-0.065,Obs/sum(Obs))
  
  col1 <- brewer.pal(n = 12, name = "Paired")[8]
  col2 <- brewer.pal(n = 12, name = "Paired")[2]
  
  par(mar=c(3.5,4.5,2,0))
  
  my.lwd <- 3.75
  
  if (max(pmf.Obs[,2])>=max(pmf.Exp[,2])){
    plot(pmf.Obs[,1],pmf.Obs[,2],type="n",col=col1,xlim=c(0.75,9.25), xlab="",ylab="Proportion", xaxt="n", main="Proportion of First Digits")
    axis(1, at=seq(1,9), labels=c("1","2","3","4","5","6","7","8","9"))
    mtext("First Digit",1,line=2.5)
    for (i in 1:nrow(pmf.Obs)){
      lines(c(pmf.Obs[i,1],pmf.Obs[i,1]),c(0,pmf.Obs[i,2]),col=col1,lwd=my.lwd)
    }
    for (i in 1:nrow(pmf.Exp)){
      lines(c(pmf.Exp[i,1],pmf.Exp[i,1]),c(0,pmf.Exp[i,2]),col=col2,lwd=my.lwd)
      legend("topright", inset=.02,
             c("Observed Proportion","Benford's Law"), fill=c(col1,col2), horiz=F)
    }
  }
  
  if (max(pmf.Obs[,2])<max(pmf.Exp[,2])){
    plot(pmf.Exp[,1],pmf.Exp[,2],type="n",col=col2,xlim=c(0.75,9.25), xlab="", ylab="Proportion", xaxt="n", main="Proportion of First Digits")
    axis(1, at=seq(1,9), labels=c("1","2","3","4","5","6","7","8","9"))
    mtext("First Digit",1,line=2.5)
    for (i in 1:nrow(pmf.Obs)){
      lines(c(pmf.Exp[i,1],pmf.Exp[i,1]),c(0,pmf.Exp[i,2]),col=col2,lwd=my.lwd)
    }
    for (i in 1:nrow(pmf.Obs)){
      lines(c(pmf.Obs[i,1],pmf.Obs[i,1]),c(0,pmf.Obs[i,2]),col=col1,lwd=my.lwd)
    }
    legend("topright", inset=.02, 
           c("Observed Proportion","Benford's Law"), fill=c(col1,col2), horiz=F)
  }
}


############################################
pr.pmf.compare<-function(sequence,big.n){
  
  string <- substr(as.character(sequence),1,1)
  
  first.digits <- as.integer(string)
  
  # In the event any of the 9 possible digits is not observed in the string, 
  # table(first.digits) will not display all digits 1, 2, ..., 9. An example is
  #
  # NAME     1  2  3  4  5  6  7  9
  # FREQ    12 11  9  8  6  5  4  2
  #
  # Here, obs <- as.integer(table(first.digits)) would only contain 8 elements.
  # To correct for this, initialize obs to a zero vector, loop through NAME, 
  # find FREQ corresponding to NAME,then paste that FREQ in obs[NAME].
  # This will also work for the case when all 9 possible digits are observed.
  
  Obs <- seq(0,0,,9)
  
  for (i in as.numeric(names(table(first.digits))))  {
    pos <- which(as.numeric(names(table(first.digits)))==i)
    Obs[i]<- as.numeric(table(first.digits))[pos]
  }  
  i <- seq(1,9)
  
  alpha <- 1/(log(big.n)-1.10)
  
  gbl <- 1/(10**(1-alpha)-1)*((i+1)**(1-alpha)-i**(1-alpha))
  
  pmf.Exp <- cbind(seq(1,9)+0.065,gbl)
  
  pmf.Obs <- cbind(seq(1,9)-0.065,Obs/sum(Obs))
  
  col1 <- brewer.pal(n = 12, name = "Paired")[8]
  col2 <- brewer.pal(n = 12, name = "Paired")[2]
  
  par(mar=c(3.5,4.5,2,0))
  
  my.lwd <- 3.75
  
  if (max(pmf.Obs[,2])>=max(pmf.Exp[,2])){
    plot(pmf.Obs[,1],pmf.Obs[,2],type="n",col=col1,xlim=c(0.75,9.25), xlab="",ylab="Proportion", xaxt="n", main="Proportion of First Digits", 
         ylim=c(0.06,
         max(1.05*max(pmf.Obs[,2]),1.05*max(pmf.Exp[,2])))
      )
    axis(1, at=seq(1,9), labels=c("1","2","3","4","5","6","7","8","9"))
    mtext("First Digit",1,line=2.5)
    for (i in 1:nrow(pmf.Obs)){
      lines(c(pmf.Obs[i,1],pmf.Obs[i,1]),c(0,pmf.Obs[i,2]),col=col1,lwd=my.lwd)
    }
    for (i in 1:nrow(pmf.Exp)){
      lines(c(pmf.Exp[i,1],pmf.Exp[i,1]),c(0,pmf.Exp[i,2]),col=col2,lwd=my.lwd)
      legend("topright", inset=.02,
             c("Observed Proportion","Generalized Benford's Law"), fill=c(col1,col2), horiz=F)
    }
  }
  
  if (max(pmf.Obs[,2])<max(pmf.Exp[,2])){
    plot(pmf.Exp[,1],pmf.Exp[,2],type="n",col=col2,xlim=c(0.75,9.25), xlab="", ylab="Proportion", xaxt="n", main="Proportion of First Digits", 
         ylim=c(0.06,
      max(1.05*max(pmf.Obs[,2]),1.05*max(pmf.Exp[,2]))))
    axis(1, at=seq(1,9), labels=c("1","2","3","4","5","6","7","8","9"))
    mtext("First Digit",1,line=2.5)
    for (i in 1:nrow(pmf.Obs)){
      lines(c(pmf.Exp[i,1],pmf.Exp[i,1]),c(0,pmf.Exp[i,2]),col=col2,lwd=my.lwd)
    }
    for (i in 1:nrow(pmf.Obs)){
      lines(c(pmf.Obs[i,1],pmf.Obs[i,1]),c(0,pmf.Obs[i,2]),col=col1,lwd=my.lwd)
    }
    legend("topright", inset=.02, 
           c("Observed Proportion","Generalized Benford's Law"), fill=c(col1,col2), horiz=F)
  }
}


##############################################################################
# Shiny Server Contents 
##############################################################################

shinyServer(function(input, output, session) {
  
  #########################################################################
  ## Additive Sequence
  #########################################################################

    withProgress(session, {
      setProgress(message = "Calculating, please wait.",
                  detail = " ", value=1)
  
  output$val.1st <- renderText({input$seq1})
  
  output$val.2nd <- renderText({input$seq2})
  
  output$textlength <- renderText({input$length})
  
  seqInput <- reactive({
    seq.gen(input$length,input$seq1,input$seq2)  
  })

  output$goodness <- renderPrint({
    goodness.test(seqInput())  
  })
  
  firstDigit <- reactive({
    string.1st(seqInput(),50)    
  })
  
  restDigit <- reactive({
    string.2nd(seqInput(),50)    
  })

  output$num.1a <- renderText({print.string(firstDigit(),  1)})
  output$num.1b <- renderText({print.string(restDigit(),   1)})
  output$num.2a <- renderText({print.string(firstDigit(),  2)})
  output$num.2b <- renderText({print.string(restDigit(),   2)})
  output$num.3a <- renderText({print.string(firstDigit(),  3)})
  output$num.3b <- renderText({print.string(restDigit(),   3)})
  output$num.4a <- renderText({print.string(firstDigit(),  4)})
  output$num.4b <- renderText({print.string(restDigit(),   4)})
  output$num.5a <- renderText({print.string(firstDigit(),  5)})
  output$num.5b <- renderText({print.string(restDigit(),   5)})
  output$num.6a <- renderText({print.string(firstDigit(),  6)})
  output$num.6b <- renderText({print.string(restDigit(),   6)})
  output$num.7a <- renderText({print.string(firstDigit(),  7)})
  output$num.7b <- renderText({print.string(restDigit(),   7)})
  output$num.8a <- renderText({print.string(firstDigit(),  8)})
  output$num.8b <- renderText({print.string(restDigit(),   8)})
  output$num.9a <- renderText({print.string(firstDigit(),  9)})
  output$num.9b <- renderText({print.string(restDigit(),   9)})
  output$num.10a <- renderText({print.string(firstDigit(),10)})
  output$num.10b <- renderText({print.string(restDigit(), 10)})
  output$num.11a <- renderText({print.string(firstDigit(),11)})
  output$num.11b <- renderText({print.string(restDigit(), 11)})
  output$num.12a <- renderText({print.string(firstDigit(),12)})
  output$num.12b <- renderText({print.string(restDigit(), 12)})
  output$num.13a <- renderText({print.string(firstDigit(),13)})
  output$num.13b <- renderText({print.string(restDigit(), 13)})
  output$num.14a <- renderText({print.string(firstDigit(),14)})
  output$num.14b <- renderText({print.string(restDigit(), 14)})
  output$num.15a <- renderText({print.string(firstDigit(),15)})
  output$num.15b <- renderText({print.string(restDigit(), 15)})
  output$num.16a <- renderText({print.string(firstDigit(),16)})
  output$num.16b <- renderText({print.string(restDigit(), 16)})
  output$num.17a <- renderText({print.string(firstDigit(),17)})
  output$num.17b <- renderText({print.string(restDigit(), 17)})
  output$num.18a <- renderText({print.string(firstDigit(),18)})
  output$num.18b <- renderText({print.string(restDigit(), 18)})
  output$num.19a <- renderText({print.string(firstDigit(),19)})
  output$num.19b <- renderText({print.string(restDigit(), 19)})
  output$num.20a <- renderText({print.string(firstDigit(),20)})
  output$num.20b <- renderText({print.string(restDigit(), 20)})
  output$num.21a <- renderText({print.string(firstDigit(), 21)})
  output$num.21b <- renderText({print.string(restDigit(),  21)})
  output$num.22a <- renderText({print.string(firstDigit(), 22)})
  output$num.22b <- renderText({print.string(restDigit(),  22)})
  output$num.23a <- renderText({print.string(firstDigit(), 23)})
  output$num.23b <- renderText({print.string(restDigit(),  23)})
  output$num.24a <- renderText({print.string(firstDigit(), 24)})
  output$num.24b <- renderText({print.string(restDigit(),  24)})
  output$num.25a <- renderText({print.string(firstDigit(), 25)})
  output$num.25b <- renderText({print.string(restDigit(),  25)})
  output$num.26a <- renderText({print.string(firstDigit(), 26)})
  output$num.26b <- renderText({print.string(restDigit(),  26)})
  output$num.27a <- renderText({print.string(firstDigit(), 27)})
  output$num.27b <- renderText({print.string(restDigit(),  27)})
  output$num.28a <- renderText({print.string(firstDigit(), 28)})
  output$num.28b <- renderText({print.string(restDigit(),  28)})
  output$num.29a <- renderText({print.string(firstDigit(), 29)})
  output$num.29b <- renderText({print.string(restDigit(),  29)})
  output$num.30a <- renderText({print.string(firstDigit(),30)})
  output$num.30b <- renderText({print.string(restDigit(), 30)})
  output$num.31a <- renderText({print.string(firstDigit(), 31)})
  output$num.31b <- renderText({print.string(restDigit(),  31)})
  output$num.32a <- renderText({print.string(firstDigit(), 32)})
  output$num.32b <- renderText({print.string(restDigit(),  32)})
  output$num.33a <- renderText({print.string(firstDigit(), 33)})
  output$num.33b <- renderText({print.string(restDigit(),  33)})
  output$num.34a <- renderText({print.string(firstDigit(), 34)})
  output$num.34b <- renderText({print.string(restDigit(),  34)})
  output$num.35a <- renderText({print.string(firstDigit(), 35)})
  output$num.35b <- renderText({print.string(restDigit(),  35)})
  output$num.36a <- renderText({print.string(firstDigit(), 36)})
  output$num.36b <- renderText({print.string(restDigit(),  36)})
  output$num.37a <- renderText({print.string(firstDigit(), 37)})
  output$num.37b <- renderText({print.string(restDigit(),  37)})
  output$num.38a <- renderText({print.string(firstDigit(), 38)})
  output$num.38b <- renderText({print.string(restDigit(),  38)})
  output$num.39a <- renderText({print.string(firstDigit(), 39)})
  output$num.39b <- renderText({print.string(restDigit(),  39)})
  output$num.40a <- renderText({print.string(firstDigit(),40)})
  output$num.40b <- renderText({print.string(restDigit(), 40)})
  output$num.41a <- renderText({print.string(firstDigit(), 41)})
  output$num.41b <- renderText({print.string(restDigit(),  41)})
  output$num.42a <- renderText({print.string(firstDigit(), 42)})
  output$num.42b <- renderText({print.string(restDigit(),  42)})
  output$num.43a <- renderText({print.string(firstDigit(), 43)})
  output$num.43b <- renderText({print.string(restDigit(),  43)})
  output$num.44a <- renderText({print.string(firstDigit(), 44)})
  output$num.44b <- renderText({print.string(restDigit(),  44)})
  output$num.45a <- renderText({print.string(firstDigit(), 45)})
  output$num.45b <- renderText({print.string(restDigit(),  45)})
  output$num.46a <- renderText({print.string(firstDigit(), 46)})
  output$num.46b <- renderText({print.string(restDigit(),  46)})
  output$num.47a <- renderText({print.string(firstDigit(), 47)})
  output$num.47b <- renderText({print.string(restDigit(),  47)})
  output$num.48a <- renderText({print.string(firstDigit(), 48)})
  output$num.48b <- renderText({print.string(restDigit(),  48)})
  output$num.49a <- renderText({print.string(firstDigit(), 49)})
  output$num.49b <- renderText({print.string(restDigit(),  49)})
  output$num.50a <- renderText({print.string(firstDigit(),50)})
  output$num.50b <- renderText({print.string(restDigit(), 50)})
  
  output$pmf <- renderPlot({
    pmf.compare(seqInput())
    })

  #######################################################################################

  output$powlength <- renderText({input$pow.length})
  
  e.seqInput <- reactive({
    pow.gen(input$pow.base,input$pow.length)  
  })
  
  output$e.goodness <- renderPrint({
    goodness.test(e.seqInput())  
  })
  
  e.firstDigit <- reactive({
    string.1st(e.seqInput(),20)    
  })
  
  e.restDigit <- reactive({
    string.2nd(e.seqInput(),20)    
  })
  
  output$e.num.1a <- renderText({print.string(e.firstDigit(),  1)})
  output$e.num.1b <- renderText({print.string(e.restDigit(),   1)})
  output$e.num.2a <- renderText({print.string(e.firstDigit(),  2)})
  output$e.num.2b <- renderText({print.string(e.restDigit(),   2)})
  output$e.num.3a <- renderText({print.string(e.firstDigit(),  3)})
  output$e.num.3b <- renderText({print.string(e.restDigit(),   3)})
  output$e.num.4a <- renderText({print.string(e.firstDigit(),  4)})
  output$e.num.4b <- renderText({print.string(e.restDigit(),   4)})
  output$e.num.5a <- renderText({print.string(e.firstDigit(),  5)})
  output$e.num.5b <- renderText({print.string(e.restDigit(),   5)})
  output$e.num.6a <- renderText({print.string(e.firstDigit(),  6)})
  output$e.num.6b <- renderText({print.string(e.restDigit(),   6)})
  output$e.num.7a <- renderText({print.string(e.firstDigit(),  7)})
  output$e.num.7b <- renderText({print.string(e.restDigit(),   7)})
  output$e.num.8a <- renderText({print.string(e.firstDigit(),  8)})
  output$e.num.8b <- renderText({print.string(e.restDigit(),   8)})
  output$e.num.9a <- renderText({print.string(e.firstDigit(),  9)})
  output$e.num.9b <- renderText({print.string(e.restDigit(),   9)})
  output$e.num.10a <- renderText({print.string(e.firstDigit(),10)})
  output$e.num.10b <- renderText({print.string(e.restDigit(), 10)})
  output$e.num.11a <- renderText({print.string(e.firstDigit(),11)})
  output$e.num.11b <- renderText({print.string(e.restDigit(), 11)})
  output$e.num.12a <- renderText({print.string(e.firstDigit(),12)})
  output$e.num.12b <- renderText({print.string(e.restDigit(), 12)})
  output$e.num.13a <- renderText({print.string(e.firstDigit(),13)})
  output$e.num.13b <- renderText({print.string(e.restDigit(), 13)})
  output$e.num.14a <- renderText({print.string(e.firstDigit(),14)})
  output$e.num.14b <- renderText({print.string(e.restDigit(), 14)})
  output$e.num.15a <- renderText({print.string(e.firstDigit(),15)})
  output$e.num.15b <- renderText({print.string(e.restDigit(), 15)})
  output$e.num.16a <- renderText({print.string(e.firstDigit(),16)})
  output$e.num.16b <- renderText({print.string(e.restDigit(), 16)})
  output$e.num.17a <- renderText({print.string(e.firstDigit(),17)})
  output$e.num.17b <- renderText({print.string(e.restDigit(), 17)})
  output$e.num.18a <- renderText({print.string(e.firstDigit(),18)})
  output$e.num.18b <- renderText({print.string(e.restDigit(), 18)})
  output$e.num.19a <- renderText({print.string(e.firstDigit(),19)})
  output$e.num.19b <- renderText({print.string(e.restDigit(), 19)})
  output$e.num.20a <- renderText({print.string(e.firstDigit(),20)})
  output$e.num.20b <- renderText({print.string(e.restDigit(), 20)})
  
  output$e.pmf <- renderPlot({pmf.compare(e.seqInput())})

  #########################################################################
  ## Power Sequence
  #########################################################################
  
  pr.seqInput <- reactive({
    
    pr.big.n <-as.integer(input$my.n)
    
    prime.gen(pr.big.n)  
  })
  
  output$num.primes <- reactive({length(pr.seqInput())})
  
  output$pr.goodness <- renderPrint({
    pr.big.n <-as.integer(input$my.n)
    
    pr.goodness.test(pr.seqInput(),pr.big.n)  
  })
  
  pr.firstDigit <- reactive({
    string.1st(pr.seqInput(),80)    
  })
  
  pr.restDigit <- reactive({
    string.2nd(pr.seqInput(),80)    
  })
  
  output$pr.num.1a <- renderText({print.string(pr.firstDigit(),  1)})
  output$pr.num.1b <- renderText({print.string(pr.restDigit(),   1)})
  output$pr.num.2a <- renderText({print.string(pr.firstDigit(),  2)})
  output$pr.num.2b <- renderText({print.string(pr.restDigit(),   2)})
  output$pr.num.3a <- renderText({print.string(pr.firstDigit(),  3)})
  output$pr.num.3b <- renderText({print.string(pr.restDigit(),   3)})
  output$pr.num.4a <- renderText({print.string(pr.firstDigit(),  4)})
  output$pr.num.4b <- renderText({print.string(pr.restDigit(),   4)})
  output$pr.num.5a <- renderText({print.string(pr.firstDigit(),  5)})
  output$pr.num.5b <- renderText({print.string(pr.restDigit(),   5)})
  output$pr.num.6a <- renderText({print.string(pr.firstDigit(),  6)})
  output$pr.num.6b <- renderText({print.string(pr.restDigit(),   6)})
  output$pr.num.7a <- renderText({print.string(pr.firstDigit(),  7)})
  output$pr.num.7b <- renderText({print.string(pr.restDigit(),   7)})
  output$pr.num.8a <- renderText({print.string(pr.firstDigit(),  8)})
  output$pr.num.8b <- renderText({print.string(pr.restDigit(),   8)})
  output$pr.num.9a <- renderText({print.string(pr.firstDigit(),  9)})
  output$pr.num.9b <- renderText({print.string(pr.restDigit(),   9)})
  output$pr.num.10a <- renderText({print.string(pr.firstDigit(),10)})
  output$pr.num.10b <- renderText({print.string(pr.restDigit(), 10)})
  output$pr.num.11a <- renderText({print.string(pr.firstDigit(),11)})
  output$pr.num.11b <- renderText({print.string(pr.restDigit(), 11)})
  output$pr.num.12a <- renderText({print.string(pr.firstDigit(),12)})
  output$pr.num.12b <- renderText({print.string(pr.restDigit(), 12)})
  output$pr.num.13a <- renderText({print.string(pr.firstDigit(),13)})
  output$pr.num.13b <- renderText({print.string(pr.restDigit(), 13)})
  output$pr.num.14a <- renderText({print.string(pr.firstDigit(),14)})
  output$pr.num.14b <- renderText({print.string(pr.restDigit(), 14)})
  output$pr.num.15a <- renderText({print.string(pr.firstDigit(),15)})
  output$pr.num.15b <- renderText({print.string(pr.restDigit(), 15)})
  output$pr.num.16a <- renderText({print.string(pr.firstDigit(),16)})
  output$pr.num.16b <- renderText({print.string(pr.restDigit(), 16)})
  output$pr.num.17a <- renderText({print.string(pr.firstDigit(),17)})
  output$pr.num.17b <- renderText({print.string(pr.restDigit(), 17)})
  output$pr.num.18a <- renderText({print.string(pr.firstDigit(),18)})
  output$pr.num.18b <- renderText({print.string(pr.restDigit(), 18)})
  output$pr.num.19a <- renderText({print.string(pr.firstDigit(),19)})
  output$pr.num.19b <- renderText({print.string(pr.restDigit(), 19)})
  output$pr.num.20a <- renderText({print.string(pr.firstDigit(),20)})
  output$pr.num.20b <- renderText({print.string(pr.restDigit(), 20)})
  output$pr.num.21a <- renderText({print.string(pr.firstDigit(), 21)})
  output$pr.num.21b <- renderText({print.string(pr.restDigit(),  21)})
  output$pr.num.22a <- renderText({print.string(pr.firstDigit(), 22)})
  output$pr.num.22b <- renderText({print.string(pr.restDigit(),  22)})
  output$pr.num.23a <- renderText({print.string(pr.firstDigit(), 23)})
  output$pr.num.23b <- renderText({print.string(pr.restDigit(),  23)})
  output$pr.num.24a <- renderText({print.string(pr.firstDigit(), 24)})
  output$pr.num.24b <- renderText({print.string(pr.restDigit(),  24)})
  output$pr.num.25a <- renderText({print.string(pr.firstDigit(), 25)})
  output$pr.num.25b <- renderText({print.string(pr.restDigit(),  25)})
  output$pr.num.26a <- renderText({print.string(pr.firstDigit(), 26)})
  output$pr.num.26b <- renderText({print.string(pr.restDigit(),  26)})
  output$pr.num.27a <- renderText({print.string(pr.firstDigit(), 27)})
  output$pr.num.27b <- renderText({print.string(pr.restDigit(),  27)})
  output$pr.num.28a <- renderText({print.string(pr.firstDigit(), 28)})
  output$pr.num.28b <- renderText({print.string(pr.restDigit(),  28)})
  output$pr.num.29a <- renderText({print.string(pr.firstDigit(), 29)})
  output$pr.num.29b <- renderText({print.string(pr.restDigit(),  29)})
  output$pr.num.30a <- renderText({print.string(pr.firstDigit(),30)})
  output$pr.num.30b <- renderText({print.string(pr.restDigit(), 30)})
  output$pr.num.31a <- renderText({print.string(pr.firstDigit(), 31)})
  output$pr.num.31b <- renderText({print.string(pr.restDigit(),  31)})
  output$pr.num.32a <- renderText({print.string(pr.firstDigit(), 32)})
  output$pr.num.32b <- renderText({print.string(pr.restDigit(),  32)})
  output$pr.num.33a <- renderText({print.string(pr.firstDigit(), 33)})
  output$pr.num.33b <- renderText({print.string(pr.restDigit(),  33)})
  output$pr.num.34a <- renderText({print.string(pr.firstDigit(), 34)})
  output$pr.num.34b <- renderText({print.string(pr.restDigit(),  34)})
  output$pr.num.35a <- renderText({print.string(pr.firstDigit(), 35)})
  output$pr.num.35b <- renderText({print.string(pr.restDigit(),  35)})
  output$pr.num.36a <- renderText({print.string(pr.firstDigit(), 36)})
  output$pr.num.36b <- renderText({print.string(pr.restDigit(),  36)})
  output$pr.num.37a <- renderText({print.string(pr.firstDigit(), 37)})
  output$pr.num.37b <- renderText({print.string(pr.restDigit(),  37)})
  output$pr.num.38a <- renderText({print.string(pr.firstDigit(), 38)})
  output$pr.num.38b <- renderText({print.string(pr.restDigit(),  38)})
  output$pr.num.39a <- renderText({print.string(pr.firstDigit(), 39)})
  output$pr.num.39b <- renderText({print.string(pr.restDigit(),  39)})
  output$pr.num.40a <- renderText({print.string(pr.firstDigit(),40)})
  output$pr.num.40b <- renderText({print.string(pr.restDigit(), 40)})
  output$pr.num.41a <- renderText({print.string(pr.firstDigit(), 41)})
  output$pr.num.41b <- renderText({print.string(pr.restDigit(),  41)})
  output$pr.num.42a <- renderText({print.string(pr.firstDigit(), 42)})
  output$pr.num.42b <- renderText({print.string(pr.restDigit(),  42)})
  output$pr.num.43a <- renderText({print.string(pr.firstDigit(), 43)})
  output$pr.num.43b <- renderText({print.string(pr.restDigit(),  43)})
  output$pr.num.44a <- renderText({print.string(pr.firstDigit(), 44)})
  output$pr.num.44b <- renderText({print.string(pr.restDigit(),  44)})
  output$pr.num.45a <- renderText({print.string(pr.firstDigit(), 45)})
  output$pr.num.45b <- renderText({print.string(pr.restDigit(),  45)})
  output$pr.num.46a <- renderText({print.string(pr.firstDigit(), 46)})
  output$pr.num.46b <- renderText({print.string(pr.restDigit(),  46)})
  output$pr.num.47a <- renderText({print.string(pr.firstDigit(), 47)})
  output$pr.num.47b <- renderText({print.string(pr.restDigit(),  47)})
  output$pr.num.48a <- renderText({print.string(pr.firstDigit(), 48)})
  output$pr.num.48b <- renderText({print.string(pr.restDigit(),  48)})
  output$pr.num.49a <- renderText({print.string(pr.firstDigit(), 49)})
  output$pr.num.49b <- renderText({print.string(pr.restDigit(),  49)})
  output$pr.num.50a <- renderText({print.string(pr.firstDigit(),50)})
  output$pr.num.50b <- renderText({print.string(pr.restDigit(), 50)})
  output$pr.num.51a <- renderText({print.string(pr.firstDigit(), 51)})
  output$pr.num.51b <- renderText({print.string(pr.restDigit(),  51)})
  output$pr.num.52a <- renderText({print.string(pr.firstDigit(), 52)})
  output$pr.num.52b <- renderText({print.string(pr.restDigit(),  52)})
  output$pr.num.53a <- renderText({print.string(pr.firstDigit(), 53)})
  output$pr.num.53b <- renderText({print.string(pr.restDigit(),  53)})
  output$pr.num.54a <- renderText({print.string(pr.firstDigit(), 54)})
  output$pr.num.54b <- renderText({print.string(pr.restDigit(),  54)})
  output$pr.num.55a <- renderText({print.string(pr.firstDigit(), 55)})
  output$pr.num.55b <- renderText({print.string(pr.restDigit(),  55)})
  output$pr.num.56a <- renderText({print.string(pr.firstDigit(), 56)})
  output$pr.num.56b <- renderText({print.string(pr.restDigit(),  56)})
  output$pr.num.57a <- renderText({print.string(pr.firstDigit(), 57)})
  output$pr.num.57b <- renderText({print.string(pr.restDigit(),  57)})
  output$pr.num.58a <- renderText({print.string(pr.firstDigit(), 58)})
  output$pr.num.58b <- renderText({print.string(pr.restDigit(),  58)})
  output$pr.num.59a <- renderText({print.string(pr.firstDigit(), 59)})
  output$pr.num.59b <- renderText({print.string(pr.restDigit(),  59)})
  output$pr.num.60a <- renderText({print.string(pr.firstDigit(),60)})
  output$pr.num.60b <- renderText({print.string(pr.restDigit(), 60)})
  output$pr.num.61a <- renderText({print.string(pr.firstDigit(), 61)})
  output$pr.num.61b <- renderText({print.string(pr.restDigit(),  61)})
  output$pr.num.62a <- renderText({print.string(pr.firstDigit(), 62)})
  output$pr.num.62b <- renderText({print.string(pr.restDigit(),  62)})
  output$pr.num.63a <- renderText({print.string(pr.firstDigit(), 63)})
  output$pr.num.63b <- renderText({print.string(pr.restDigit(),  63)})
  output$pr.num.64a <- renderText({print.string(pr.firstDigit(), 64)})
  output$pr.num.64b <- renderText({print.string(pr.restDigit(),  64)})
  output$pr.num.65a <- renderText({print.string(pr.firstDigit(), 65)})
  output$pr.num.65b <- renderText({print.string(pr.restDigit(),  65)})
  output$pr.num.66a <- renderText({print.string(pr.firstDigit(), 66)})
  output$pr.num.66b <- renderText({print.string(pr.restDigit(),  66)})
  output$pr.num.67a <- renderText({print.string(pr.firstDigit(), 67)})
  output$pr.num.67b <- renderText({print.string(pr.restDigit(),  67)})
  output$pr.num.68a <- renderText({print.string(pr.firstDigit(), 68)})
  output$pr.num.68b <- renderText({print.string(pr.restDigit(),  68)})
  output$pr.num.69a <- renderText({print.string(pr.firstDigit(), 69)})
  output$pr.num.69b <- renderText({print.string(pr.restDigit(),  69)})
  output$pr.num.70a <- renderText({print.string(pr.firstDigit(),70)})
  output$pr.num.70b <- renderText({print.string(pr.restDigit(), 70)})
  output$pr.num.71a <- renderText({print.string(pr.firstDigit(), 71)})
  output$pr.num.71b <- renderText({print.string(pr.restDigit(),  71)})
  output$pr.num.72a <- renderText({print.string(pr.firstDigit(), 72)})
  output$pr.num.72b <- renderText({print.string(pr.restDigit(),  72)})
  output$pr.num.73a <- renderText({print.string(pr.firstDigit(), 73)})
  output$pr.num.73b <- renderText({print.string(pr.restDigit(),  73)})
  output$pr.num.74a <- renderText({print.string(pr.firstDigit(), 74)})
  output$pr.num.74b <- renderText({print.string(pr.restDigit(),  74)})
  output$pr.num.75a <- renderText({print.string(pr.firstDigit(), 75)})
  output$pr.num.75b <- renderText({print.string(pr.restDigit(),  75)})
  output$pr.num.76a <- renderText({print.string(pr.firstDigit(), 76)})
  output$pr.num.76b <- renderText({print.string(pr.restDigit(),  76)})
  output$pr.num.77a <- renderText({print.string(pr.firstDigit(), 77)})
  output$pr.num.77b <- renderText({print.string(pr.restDigit(),  77)})
  output$pr.num.78a <- renderText({print.string(pr.firstDigit(), 78)})
  output$pr.num.78b <- renderText({print.string(pr.restDigit(),  78)})
  output$pr.num.79a <- renderText({print.string(pr.firstDigit(), 79)})
  output$pr.num.79b <- renderText({print.string(pr.restDigit(),  79)})
  output$pr.num.80a <- renderText({print.string(pr.firstDigit(),80)})
  output$pr.num.80b <- renderText({print.string(pr.restDigit(), 80)})
 
  output$pr.pmf <- renderPlot({
    pr.big.n <-as.integer(input$my.n)
        pr.pmf.compare(pr.seqInput(),pr.big.n)
    })

  })#close withProgress
  
})#close shinyServer
  