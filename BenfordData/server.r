# -------------------------------------------
#  App Title: Benford's Law and Data Examples
#     Author: Jimmy Doi
# -------------------------------------------

library(RColorBrewer)
library(rvest)
library(xml2)

##############################################################
## US Census                                                ##
##############################################################

########################################
# Download complete data set from:
# https://www.census.gov/data/datasets/2016/demo/popest/counties-total.html

comp.cens.data <- function(){

  vals <- as.matrix(read.csv("co-est2016-alldata.csv", header=T))
  vals[,5] <- as.integer(vals[,5])
  vals <- vals[vals[,5]!=0,] #only leave county data

  return(vals)
}

########################################
cens.data <- function(census.data,ID){

  sequence <- as.numeric(census.data[,ID])

  # Convert to scientific notation
  sequence <- format(sequence,scientific=TRUE)

  return(sequence)
}

##############################################################
## Census Quick Facts                                       ##
##############################################################



########################################
# Download complete data set from:
# quickfacts.census.gov/qfd/download/DataSet.txt

comp.quick.data <- function(){

  vals <- as.matrix(read.csv("DataSet.txt", header=T))
  vals <- vals[vals[,1]%%1000!=0,] #only leave county data

  return(vals)
}

########################################
quick.data <- function(census.data,ID){

  sequence <- as.numeric(census.data[,ID])

  # Convert to scientific notation
  sequence <- format(sequence,scientific=TRUE)

  return(sequence)
}

##############################################################
## Stock Market                                             ##
##############################################################

########################################
comp.data <- function(mkt){

  vals <- as.matrix(read.csv(paste("http://online.wsj.com/public/resources/documents/",mkt,".csv",sep=""), header=T,skip=3))

  return(vals)
}

########################################
date.data <- function(){
  date1 <- as.matrix(read.csv("http://online.wsj.com/public/resources/documents/NYSE.csv",header=F,skip=1, nrows=1))
  date2 <- as.matrix(read.csv("http://online.wsj.com/public/resources/documents/Nasdaq.csv",header=F,skip=1, nrows=1))
  date3 <- as.matrix(read.csv("http://online.wsj.com/public/resources/documents/SCAP.csv",header=F,skip=1, nrows=1))
  date4 <- as.matrix(read.csv("http://online.wsj.com/public/resources/documents/AMEX.csv",header=F,skip=1, nrows=1))

  min.length <- min(nchar(date1),nchar(date2),nchar(date3),nchar(date4))

  if (nchar(date1)==min.length) return(date1)
  else if (nchar(date2)==min.length) return(date2)
  else if (nchar(date3)==min.length) return(date3)
  else if (nchar(date4)==min.length) return(date4)
}


##############################################################
## World Stock Market                                       ##
##############################################################

########################################
w.comp.data <- function(mkt){
  url <- paste("https://www.investing.com/indices/",mkt,sep="")

  #vals <- as.matrix(read.csv(paste("http://online.wsj.com/public/resources/documents/",mkt,".csv",sep=""), header=T,skip=3))

  vals <- url %>%
    read_html() %>%
    html_nodes(xpath='//*[@id="cr1"]') %>%
    html_table()

  vals <- vals[[1]]

  # Following line removes any "0 volume" stocks, but at
  # some points of the day volume can be 0 for all stocks
  # and that would lead to NO STOCKS READ
  #
  # vals<-vals[which(vals[,8]!=0),]

  return(vals)
}

########################################
w.status <- function(mkt){
  url <- paste("https://www.investing.com/indices/",mkt,sep="")

  my.status  <- url %>%
    read_html() %>%
    html_nodes(xpath='//*[@id="quotes_summary_current_data"]/div[1]/div[2]/div[2]/text()') %>%
    html_text()

  status <- "ACTIVE"
  if (length(grep("Closed",my.status[3]))) status <- "CLOSED"

  return(status)
}

########################################
seq.data <- function(all.data,var){

  # Use gsub() to remove embedded commas if any (for volume data)
  # Will not alter data without embedded commas (open, high, low, close)
  sequence <- as.numeric(gsub(",","",all.data[,var]))

  # Convert to scientific notation
  sequence <- format(sequence,scientific=TRUE)

  return(sequence)

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

  Exp <- sum(Obs)*log10(1+1/i)

  Digit <- seq(1,9)

  #print(info)

  chisq <- sum(((Obs-Exp)**2)/Exp)

  Exp <- round(Exp,3)
  info <- cbind(Digit,Obs,Exp)
  rownames(info)<-rep("",nrow(info))

  #print(chisq)

  p.value <- 1-pchisq(chisq,8)

  text.chi <- paste("Chi-Square=",format(round(chisq,3),nsmall=3),sep="")
  names(text.chi)<-""

  text.pval <- paste("P-Value=",format(round(p.value,3),nsmall=3),sep="")
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

  col1 <- brewer.pal(n = 8, name = "Dark2")[6]
  col2 <- brewer.pal(n = 8, name = "Dark2")[1]

  par(mar=c(3.5,4.5,2,0))

  my.lwd <- 3.75

  if (max(pmf.Obs[,2])>=max(pmf.Exp[,2])){
    plot(pmf.Obs[,1],pmf.Obs[,2],type="n",col=col1,xlim=c(0.75,9.25),
         ylim=c(0,max(pmf.Obs[,2])),
         xlab="",ylab="Proportion", xaxt="n", main="Proportion of First Digits")
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
    plot(pmf.Exp[,1],pmf.Exp[,2],type="n",col=col2,xlim=c(0.75,9.25),
         ylim=c(0,max(pmf.Exp[,2])),
         xlab="", ylab="Proportion", xaxt="n", main="Proportion of First Digits")
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

##############################################################################
# Shiny Server Contents
##############################################################################

shinyServer(function(input, output, session) {

  #########################################################################
  ## Census I: Pop'n Estimates
  #########################################################################

  output$out.var.cens <- renderText({
      if (input$sel.var.cens==13) return("Resident Total Population Estimate 7/1/2013")
      if (input$sel.var.cens==14) return("Resident Total Population Estimate 7/1/2014")
      if (input$sel.var.cens==15) return("Resident Total Population Estimate 7/1/2015")
      if (input$sel.var.cens==16) return("Resident Total Population Estimate 7/1/2016")
      if (input$sel.var.cens==27) return("Births in Period 7/1/2012 to 6/30/2013")
      if (input$sel.var.cens==28) return("Births in Period 7/1/2013 to 6/30/2014")
      if (input$sel.var.cens==29) return("Births in Period 7/1/2014 to 6/30/2015")
      if (input$sel.var.cens==30) return("Births in Period 7/1/2015 to 6/30/2016")
      if (input$sel.var.cens==34) return("Deaths in Period 7/1/2012 to 6/30/2013")
      if (input$sel.var.cens==35) return("Deaths in Period 7/1/2013 to 6/30/2014")
      if (input$sel.var.cens==36) return("Deaths in Period 7/1/2014 to 6/30/2015")
      if (input$sel.var.cens==37) return("Deaths in Period 7/1/2015 to 6/30/2016")
      })

  complete.cens <- reactive({
    withProgress(message = 'Accessing Data', style = 'notification', value = 0.5, {
      Sys.sleep(.6)
      return(comp.cens.data())
    })
  })

  output$comp.rows.cens <- reactive({nrow(complete.cens())})

  data.seq.cens <- reactive({cens.data(complete.cens(),as.numeric(input$sel.var.cens))})

  output$goodness.cens <- renderPrint({
    withProgress(message = 'Performing Goodness of Fit', style = 'notification', value = 0.75, {
      Sys.sleep(0.6)
      goodness.test(data.seq.cens())
    })
  })

  output$cens.pmf <- renderPlot({
    withProgress(message = 'Generating Plot', style = 'notification', value = 1, {
      Sys.sleep(0.6)
      pmf.compare(data.seq.cens())
    })
  })

  #########################################################################
  ## Census II: Quick Data
  #########################################################################

  output$out.var.quick <- renderText({
      if (input$sel.var.quick==25) return("Housing Units, 2013")
      if (input$sel.var.quick==29) return("Households, 2008-12")
      if (input$sel.var.quick==23) return("Veterans, 2008-12")
      if (input$sel.var.quick==37) return("Nonemployer Establishments, 2012")
      if (input$sel.var.quick==34) return("*Private Nonfarm Establishments, 2012")
      if (input$sel.var.quick==35) return("*Private Nonfarm Employment, 2012")
      if (input$sel.var.quick==47) return("*Retail Sales, 2007 ($1000)")
      })

  complete.quick <- reactive({
    withProgress(message = 'Accessing Data', style = 'notification', value = 0.5, {
      Sys.sleep(.6)
      return(comp.quick.data())
    })
  })

  output$comp.rows.quick <- reactive({nrow(complete.quick())})

  data.seq.quick <- reactive({quick.data(complete.quick(),as.numeric(input$sel.var.quick))})

  output$goodness.quick <- renderPrint({
    withProgress(message = 'Performing Goodness of Fit', style = 'notification', value = 0.75, {
      Sys.sleep(0.6)
      goodness.test(data.seq.quick())
    })
  })

  output$quick.pmf <- renderPlot({
    withProgress(message = 'Generating Plot', style = 'notification', value = 1, {
      Sys.sleep(0.6)
      pmf.compare(data.seq.quick())
    })
  })

  #########################################################################
  ## US Stock Market                                                     ##
  #########################################################################

  output$out.var <- renderText({
    if (input$sel.var==9) return("Volume of Shares Traded")
    if (input$sel.var==3) return("Opening Price")
    if (input$sel.var==4) return("High Price")
    if (input$sel.var==5) return("Low Price")
    if (input$sel.var==6) return("Closing Price")
  })

  output$param.stock <- renderText({
    if (input$stock == "New York Stock Exchange") return("NYSE")
    if (input$stock == "Nasdaq Stock Market") return("Nasdaq")
    if (input$stock == "Nasdaq Capital Market") return("SCAP")
    if (input$stock == "NYSE MKT Stock Exchange") return("AMEX")
  })

  output$short.stock <- renderText({
    if (input$stock == "New York Stock Exchange") return("NYSE")
    if (input$stock == "Nasdaq Stock Market") return("Nasdaq")
    if (input$stock == "Nasdaq Capital Market") return("Nasdaq Cap")
    if (input$stock == "NYSE MKT Stock Exchange") return("NYSE MKT")
  })

  complete <- reactive({
    if (input$stock == "New York Stock Exchange") mkt <- "NYSE"
    if (input$stock == "Nasdaq Stock Market")     mkt <- "Nasdaq"
    if (input$stock == "Nasdaq Capital Market")   mkt <- "SCAP"
    if (input$stock == "NYSE MKT Stock Exchange") mkt <- "AMEX"
    withProgress(message = 'Accessing Data', style = 'notification', value = 0.5, {
    return(comp.data(mkt))
    })
  })

  output$comp.date <- renderText({
    return(date.data())
  })

  output$comp.rows <- reactive({nrow(complete())})

  data.seq <- reactive({seq.data(complete(),as.numeric(input$sel.var))})

  output$goodness <- renderPrint({
    withProgress(message = 'Performing Goodness of Fit', style = 'notification', value = 0.75, {
      Sys.sleep(0.6)
      goodness.test(data.seq())
    })
  })

  output$stock.pmf <- renderPlot({
    withProgress(message = 'Generating Plot', style = 'notification', value = 1, {
      Sys.sleep(0.6)
      pmf.compare(data.seq())
    })
  })

  #########################################################################
  ## World Stock Market
  #########################################################################

  output$w.out.var <- renderText({
      if (input$w.sel.var==3) return("Most Recent Price")
      if (input$w.sel.var==4) return("High Price")
      if (input$w.sel.var==5) return("Low Price")
      })

  UTC.time <- as.POSIXct(Sys.time(), format="%m/%d/%Y %H:%M:%S")
  attr(UTC.time,"tzone")<-"UTC"

  output$w.time.stock <- renderText({
    if(input$w.stock == "Australia")   return(paste(toupper(format(UTC.time, "%Y%b%d"))," -- ",toupper(format(UTC.time, "%H:%M:%S %Z")),sep=""))
    if(input$w.stock == "Canada")      return(paste(toupper(format(UTC.time, "%Y%b%d"))," -- ",toupper(format(UTC.time, "%H:%M:%S %Z")),sep=""))
    if(input$w.stock == "China")       return(paste(toupper(format(UTC.time, "%Y%b%d"))," -- ",toupper(format(UTC.time, "%H:%M:%S %Z")),sep=""))
    if(input$w.stock == "Denmark")     return(paste(toupper(format(UTC.time, "%Y%b%d"))," -- ",toupper(format(UTC.time, "%H:%M:%S %Z")),sep=""))
    if(input$w.stock == "England")     return(paste(toupper(format(UTC.time, "%Y%b%d"))," -- ",toupper(format(UTC.time, "%H:%M:%S %Z")),sep=""))
    if(input$w.stock == "France")      return(paste(toupper(format(UTC.time, "%Y%b%d"))," -- ",toupper(format(UTC.time, "%H:%M:%S %Z")),sep=""))
    if(input$w.stock == "Germany")     return(paste(toupper(format(UTC.time, "%Y%b%d"))," -- ",toupper(format(UTC.time, "%H:%M:%S %Z")),sep=""))
    if(input$w.stock == "India")       return(paste(toupper(format(UTC.time, "%Y%b%d"))," -- ",toupper(format(UTC.time, "%H:%M:%S %Z")),sep=""))
    if(input$w.stock == "Japan")       return(paste(toupper(format(UTC.time, "%Y%b%d"))," -- ",toupper(format(UTC.time, "%H:%M:%S %Z")),sep=""))
    if(input$w.stock == "Korea")       return(paste(toupper(format(UTC.time, "%Y%b%d"))," -- ",toupper(format(UTC.time, "%H:%M:%S %Z")),sep=""))
    if(input$w.stock == "Netherlands") return(paste(toupper(format(UTC.time, "%Y%b%d"))," -- ",toupper(format(UTC.time, "%H:%M:%S %Z")),sep=""))
    if(input$w.stock == "Poland")      return(paste(toupper(format(UTC.time, "%Y%b%d"))," -- ",toupper(format(UTC.time, "%H:%M:%S %Z")),sep=""))
    if(input$w.stock == "Sri Lanka")   return(paste(toupper(format(UTC.time, "%Y%b%d"))," -- ",toupper(format(UTC.time, "%H:%M:%S %Z")),sep=""))
    if(input$w.stock == "Switzerland") return(paste(toupper(format(UTC.time, "%Y%b%d"))," -- ",toupper(format(UTC.time, "%H:%M:%S %Z")),sep=""))
    if(input$w.stock == "Turkey")      return(paste(toupper(format(UTC.time, "%Y%b%d"))," -- ",toupper(format(UTC.time, "%H:%M:%S %Z")),sep=""))  })

  output$USA.icon.stock <- renderImage({
    return(list(src=normalizePath(file.path('./flags',paste('USA.png', sep=''))),height=24))
  },deleteFile=FALSE)

    output$w.icon.stock <- renderImage({
    if(input$w.stock == "Australia")   return(list(src=normalizePath(file.path('./flags',paste('AUS.png', sep=''))),height=24))
    if(input$w.stock == "Canada")      return(list(src=normalizePath(file.path('./flags',paste('CAN.png', sep=''))),height=24))
    if(input$w.stock == "China")       return(list(src=normalizePath(file.path('./flags',paste('CHN.png', sep=''))),height=24))
    if(input$w.stock == "Denmark")     return(list(src=normalizePath(file.path('./flags',paste('DEN.png', sep=''))),height=24))
    if(input$w.stock == "England")     return(list(src=normalizePath(file.path('./flags',paste('ENG.png', sep=''))),height=24))
    if(input$w.stock == "France")      return(list(src=normalizePath(file.path('./flags',paste('FRA.png', sep=''))),height=24))
    if(input$w.stock == "Germany")     return(list(src=normalizePath(file.path('./flags',paste('GER.png', sep=''))),height=24))
    if(input$w.stock == "India")       return(list(src=normalizePath(file.path('./flags',paste('IND.png', sep=''))),height=24))
    if(input$w.stock == "Japan")       return(list(src=normalizePath(file.path('./flags',paste('JPN.png', sep=''))),height=24))
    if(input$w.stock == "Korea")       return(list(src=normalizePath(file.path('./flags',paste('KOR.png', sep=''))),height=24))
    if(input$w.stock == "Netherlands") return(list(src=normalizePath(file.path('./flags',paste('NET.png', sep=''))),height=24))
    if(input$w.stock == "Poland")      return(list(src=normalizePath(file.path('./flags',paste('POL.png', sep=''))),height=24))
    if(input$w.stock == "Sri Lanka")   return(list(src=normalizePath(file.path('./flags',paste('SRI.png', sep=''))),height=24))
    if(input$w.stock == "Switzerland") return(list(src=normalizePath(file.path('./flags',paste('SWI.png', sep=''))),height=24))
    if(input$w.stock == "Turkey")      return(list(src=normalizePath(file.path('./flags',paste('TUR.png', sep=''))),height=24))
    },deleteFile=FALSE)

  output$w.curr.stock <- renderText({
    if(input$w.stock == "Australia")   return("AUD")
    if(input$w.stock == "Canada")      return("CAD")
    if(input$w.stock == "China")       return("CNY")
    if(input$w.stock == "Denmark")     return("DKK")
    if(input$w.stock == "England")     return("GBP")
    if(input$w.stock == "France")      return("EUR")
    if(input$w.stock == "Germany")     return("EUR")
    if(input$w.stock == "India")       return("INR")
    if(input$w.stock == "Japan")       return("JPY")
    if(input$w.stock == "Korea")       return("KRW")
    if(input$w.stock == "Netherlands") return("EUR")
    if(input$w.stock == "Poland")      return("PLN")
    if(input$w.stock == "Sri Lanka")   return("LKR")
    if(input$w.stock == "Switzerland") return("CHF")
    if(input$w.stock == "Turkey")      return("TRY")  })

  output$USA.curr.stock <- renderText({
    return("USD")})

    output$w.short.stock <- renderText({
    if(input$w.stock == "Australia")   return("S&P/ASX 200 (AXJO)")
    if(input$w.stock == "Canada")      return("S&P/TSX Comp (GSPTSE)")
    if(input$w.stock == "China")       return("SZSE Component (SZSC1)")
    if(input$w.stock == "Denmark")     return("OMX Copenhagen (OMXCGI)")
    if(input$w.stock == "England")     return("FTSE 350 (FTLC)")
    if(input$w.stock == "France")      return("CAC All Shares (PAX)")
    if(input$w.stock == "Germany")     return("Classic All Share (CLALL)")
    if(input$w.stock == "India")       return("S&P BSE-500 (BSE500)")
    if(input$w.stock == "Japan")       return("Nikkei 225 (N225)")
    if(input$w.stock == "Korea")       return("KOSPI (KS11)")
    if(input$w.stock == "Netherlands") return("Next 150 (N150)")
    if(input$w.stock == "Poland")      return("WIG (WIG)")
    if(input$w.stock == "Sri Lanka")   return("CSE All-Share (CSE)")
    if(input$w.stock == "Switzerland") return("Swiss All Share (SSIR)")
    if(input$w.stock == "Turkey")      return("BIST 100 (XU100)")
  })

  w.complete <- reactive({
    if(input$w.stock == "Australia")   w.mkt <- "aus-200-components"
    if(input$w.stock == "Canada")      w.mkt <- "s-p-tsx-composite-components"
    if(input$w.stock == "China")       w.mkt <- "szse-component-components"
    if(input$w.stock == "Denmark")     w.mkt <- "omx-copenhagen-all-shares-components"
    if(input$w.stock == "England")     w.mkt <- "ftse-350-components"
    if(input$w.stock == "France")      w.mkt <- "cac-allshares-components"
    if(input$w.stock == "Germany")     w.mkt <- "classic-all-share-components"
    if(input$w.stock == "India")       w.mkt <- "s-p-bse-500-components"
    if(input$w.stock == "Japan")       w.mkt <- "japan-ni225-components"
    if(input$w.stock == "Korea")       w.mkt <- "kospi-components"
    if(input$w.stock == "Netherlands") w.mkt <- "next-150-index-components"
    if(input$w.stock == "Poland")      w.mkt <- "wig-components"
    if(input$w.stock == "Sri Lanka")   w.mkt <- "cse-all-share-components"
    if(input$w.stock == "Switzerland") w.mkt <- "swiss-allshare-components"
    if(input$w.stock == "Turkey")      w.mkt <- "ise-100-components"
    withProgress(message = 'Accessing Data', style = 'notification', value = 0.5, {
    return(w.comp.data(w.mkt))
    })
  })

  output$w.mkt.status <- reactive({
    if(input$w.stock == "Australia")   w.mkt <- "aus-200-components"
    if(input$w.stock == "Canada")      w.mkt <- "s-p-tsx-composite-components"
    if(input$w.stock == "China")       w.mkt <- "szse-component-components"
    if(input$w.stock == "Denmark")     w.mkt <- "omx-copenhagen-all-shares-components"
    if(input$w.stock == "England")     w.mkt <- "ftse-350-components"
    if(input$w.stock == "France")      w.mkt <- "cac-allshares-components"
    if(input$w.stock == "Germany")     w.mkt <- "classic-all-share-components"
    if(input$w.stock == "India")       w.mkt <- "s-p-bse-500-components"
    if(input$w.stock == "Japan")       w.mkt <- "japan-ni225-components"
    if(input$w.stock == "Korea")       w.mkt <- "kospi-components"
    if(input$w.stock == "Netherlands") w.mkt <- "next-150-index-components"
    if(input$w.stock == "Poland")      w.mkt <- "wig-components"
    if(input$w.stock == "Sri Lanka")   w.mkt <- "cse-all-share-components"
    if(input$w.stock == "Switzerland") w.mkt <- "swiss-allshare-components"
    if(input$w.stock == "Turkey")      w.mkt <- "ise-100-components"
    withProgress(message = 'Accessing Market Status', style = 'notification', value = 0.25, {
                  return(w.status(w.mkt))
    })
  })

  output$w.comp.rows <- reactive({nrow(w.complete())})

  w.data.seq <- reactive({seq.data(w.complete(),as.numeric(input$w.sel.var))})

  output$w.goodness <- renderPrint({
    withProgress(message = 'Performing Goodness of Fit', style = 'notification', value = 0.75, {
      Sys.sleep(0.6)
      goodness.test(w.data.seq())
    })
  })

  output$w.stock.pmf <- renderPlot({
    withProgress(message = 'Generating Plot', style = 'notification', value = 1, {
      Sys.sleep(0.6)
      pmf.compare(w.data.seq())
    })
  })

})#close shinyServer
