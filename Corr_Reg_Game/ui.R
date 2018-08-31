# ------------------------------------------------
#  App Title: Games of Correlation and Regression
#     Author: Irvin Alcaraz
# ------------------------------------------------

library(shiny)
library(shinyBS)
library(ggplot2)
library(ggvis)
library(magrittr)

shinyUI(navbarPage("Data games",

  tabPanel("Correlation Game",
           
           withMathJax(),
           p("Correlation is a statistical measurement used to quantify the strength and direction of
           a linear relationship.",br(), 
           "\\(\\bullet\\) This value is unitless, and thus is not affected by location and scale of the variables,
           and bound between -1 and 1.",br(),
           "\\(\\bullet\\) It is typically denoted by \\(r\\) or by \\(\\rho\\).",br(),
           "\\(\\bullet\\) A correlation of -1 would mean that the data have a perfectly negative relationship, which would appear in the scatterplot as
           a perfect line with a negative slope.",br(),
           "\\(\\bullet\\) Similarly, a correlation of  1 would mean that the data are perfectly positively correlated, which would appear in the scatterplot as a perfect line
           with a positive slope.",br(),
           "\\(\\bullet\\) If data have no relationship, they would have a correlation of 0, and would appear as a random
           scatter of points in the scatterplot.",br(),
           "\\(\\bullet\\) The correlation presented in this application is generated using the Pearson
           Correlation Coefficient method.",br(),
           "\\(\\bullet\\) The formula used to calculate this is value is \\({1\\over n-1}\\sum_{i=1}^n{(x_i-\\bar{x})(y_i-\\bar{y})\\over s_xs_y}\\)"),
          
           sidebarLayout(


            sidebarPanel(
              helpText("Number of Observations"),
              selectInput("nobs","",c(10,100,1000),100),
              actionButton("newdataset","New Data"),
              HTML("<hr style='height: 2px; color: #F3F3F3; background-color: #F3F3F3; border: none;'>"),
              helpText('Guess the Correlation, \\(\\rho\\)'),
              sliderInput("rho","",min=-1,max=1,value=0,step=.1),
              actionButton("answer","Submit"),
              actionButton("cheat","Show Answer"),
              
              div("Shiny app by", 
                  a(href="https://www.linkedin.com/in/irvinalcaraz",target="_blank", 
                    "Irvin Alcaraz"),align="right", style = "font-size: 8pt"),
              
              div("Base R code by", 
                  a(href="https://www.linkedin.com/in/irvinalcaraz",target="_blank", 
                    "Irvin Alcaraz"),align="right", style = "font-size: 8pt"),
              
              div("Shiny source files:",
                  a(href="https://gist.github.com/calpolystat/8e898319968d31b27310",
                    target="_blank","GitHub Gist"),align="right", style = "font-size: 8pt"),
              
              div(a(href="http://www.statistics.calpoly.edu/shiny",target="_blank", 
                    "Cal Poly Statistics Dept Shiny Series"),align="right", style = "font-size: 8pt")
              
            ),
            mainPanel(
              bsAlert("correct"),
#               plotOutput("correlationPlot")
            ggvisOutput("correlationPlot"),
            uiOutput("showcorr")
            )
            
            )),                                     
  tabPanel("Regression Game",
           sidebarLayout(
   
    sidebarPanel(
      
      tags$head(tags$link(rel = "icon", type = "image/x-icon",href = "https://webresource.its.calpoly.edu/cpwebtemplate/5.0.1/common/images_html/favicon.ico")),
      withMathJax(),
      selectInput("obs","Number of Observations",c(10,100,1000),100),
      helpText('Correlation, \\(\\rho\\)'),
      sliderInput("corr","",min=-1,max=1,value=0,step=.1),
      actionButton("getdata","New Data"),
      HTML("<hr style='height: 2px; color: #F3F3F3; background-color: #F3F3F3; border: none;'>"),
      HTML("<hr style='height: 2px; color: #F3F3F3; background-color: #F3F3F3; border: none;'>"),
      helpText('The intercept, \\(\\hat{\\beta_0}\\) (round to the nearest whole number)'),
      numericInput("b0","",value=0),
      helpText('The slope, \\(\\hat{\\beta_1}\\) (round to the nearest tenth)'),
      numericInput("b1","",value=0),
      ##helpText("Assessment method"),
      ##radioButtons("sseOrRsq","",choices=c("Maximize \\(R^2\\)"="Rsq","Minimize \\(SSE\\)"="sse"),selected="Rsq"),
      HTML("<hr style='height: 2px; color: #F3F3F3; background-color: #F3F3F3; border: none;'>"),
      actionButton("go","Submit"),
      actionButton("showit","Show Answer"),
      
      div("Shiny app by", 
          a(href="https://www.linkedin.com/in/irvinalcaraz",target="_blank", 
            "Irvin Alcaraz"),align="right", style = "font-size: 8pt"),
      
      div("Base R code by", 
          a(href="https://www.linkedin.com/in/irvinalcaraz",target="_blank", 
            "Irvin Alcaraz"),align="right", style = "font-size: 8pt"),
      
      div("Shiny source files:",
          a(href="https://gist.github.com/calpolystat/8e898319968d31b27310",
            target="_blank","GitHub Gist"),align="right", style = "font-size: 8pt"),
      
      div(a(href="http://www.statistics.calpoly.edu/shiny",target="_blank", 
            "Cal Poly Statistics Dept Shiny Series"),align="right", style = "font-size: 8pt")
            
    ),
    mainPanel(
      p("Often, we wish to predict the value of some variable, called the response, based on
        the value of another linearly related variable, called the explanatory. This idea is called linear regression.
        We will deal with simple linear regression, which makes use of a single response and predictor.
        In order to estimate the response variable based on the explanatory we will fit a line,
        also called a model, to the data. This line is called the least squares regression line, and it attempts to
        minimize the deviations of the points from the line, or the residuals.",br(),
        "\\(\\bullet\\) The population regression line is: \\(Y=\\beta_o+\\beta_1X\\)",br(),
        "\\(\\bullet\\) When given a random sample of data, we estimate this by: \\(\\hat{y}=b_0+b_1x\\)",br(),
        "\\(\\bullet\\)To assess, whether or not the estimated line is the best line we can look at two values.",br(),
        "\\(\\qquad\\circ\\) We can minimize a value called the sum of squared errors, denoted \\(SSE=\\sum_{i=1}^n(y_i-\\hat{y_i})^2\\).",br(),
        "\\(\\qquad\\circ\\) Equivalently, we can maximize a value called the coefficient of determination. We denote this value as
        \\(R^2=1-{SSE \\over SST}\\), where \\(SST=\\sum_{i=1}^n(y_i-\\bar{y})^2\\)"),
      bsAlert("success"),
      uiOutput("SSE"),
      plotOutput("regressionPlot"),
      uiOutput("realline")

    )
    ))
))
