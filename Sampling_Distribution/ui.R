# ------------------
# App Title: Sampling distribution demonstration
#    Author: Gail Potter
# ------------------


if (!require("devtools")) install.packages("devtools")
 
if (!require("shinyBS")) install.packages("shinyBS")
  library(shinyBS)

if (!require(shinyIncubator)) devtools::install_github("rstudio/shiny-incubator")
library(shinyIncubator)

if (!require("shinysky")) devtools::install_github("ShinySky","AnalytixWare")
library(shinysky)


shinyUI(fluidPage(
  includeCSS('styles.css'),
  
  progressInit(),
  
  tags$head(tags$link(rel = "icon", type = "image/x-icon", href =  
  "https://webresource.its.calpoly.edu/cpwebtemplate/5.0.1/common/images_html/favicon.ico")),  
    
  h3("Sampling distribution demonstration"),
  fluidRow(
    column(3,
           wellPanel( 
             selectInput("popdist", label = h5("Population distribution"), 
                         choices = list("Normal" = "normal", "Left-skewed" = "left.skewed",
                                        "Uniform" = "uniform", "Right-skewed" = "right.skewed",
                                        "Bimodal"="bimodal"), selected = "normal"),
             br(),
              shinyalert("shinyalert3", TRUE, auto.close.after=5),
            numericInput("popmean", label = h5("Population mean"), value=0),
            br(),
              shinyalert("shinyalert4", TRUE, auto.close.after=5),
            numericInput("popsd", label = h5("Population standard deviation"), value=1),
            br(),
              shinyalert("shinyalert1", TRUE, auto.close.after=5),

             numericInput("n", label=h5("Sample size"), value=10, min=1, max=1000),
             selectInput("statistic", label = h5("Statistic"), 
                         choices = list("Mean" = "mean", "Median" = "median",
                                        "1st quartile (Q1)" = "Q1",
                                        "3rd quartile (Q3)" = "Q3", 
                                        "Standard deviation" = "standard deviation",
                                        "Maximum"="maximum", "Minimum"="minimum"), selected = "mean"),
                           
           div("Shiny app by", 
               a(href="http://www.gailpotter.org",target="_blank", 
                 "Gail Potter"),align="right", style = "font-size: 8pt"),
           
           div("Base R code by", 
               a(href="http://www.gailpotter.org",target="_blank", 
                 "Gail Potter"),align="right", style = "font-size: 8pt"),
           
           div("Shiny source files:",
               a(href="https://gist.github.com/calpolystat/d7ed9873137267ee557b",
                 target="_blank","GitHub Gist"),align="right", style = "font-size: 8pt"),
           
           div(a(href="http://www.statistics.calpoly.edu/shiny",target="_blank", 
                 "Cal Poly Statistics Dept Shiny Series"),align="right", style = "font-size: 8pt"))
           
    ),
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    
    column(9, wellPanel(
      p("In the left panel, specify a population shape, sample size, and statistic of interest.  When you press the 
 'Draw samples' button, a sample from that population will be generated and plotted below left.  The statistic will be
calculated and added to the histogram at right.  By generating many different samples, you can see how the statistic tends to vary from one sample to the next.  
That distribution is called the 'sampling distribution'.  You can change the population distribution 
to see how that impacts your sample histogram as well as the sampling distribution."),
      shinyalert("shinyalert2", TRUE, auto.close.after=5),

      numericInput("nsim", label=h5("Number of samples"), value=1, min=1, max=1000000),
      actionButton("go", label = "Draw samples"),
      actionButton("clear",label="Clear"),
      
      bsCollapse(multiple = FALSE, open = NULL, id = "collapse1",
                 bsCollapsePanel("Click here to display population characteristics.  (Click again to hide.)", 
                                 plotOutput("popdistn", height="200px"), 
                                 id="popcurve", value="test3")
      ) ,
      
      plotOutput("dotplot", height="290px"),
      textOutput("numsims"),
      checkboxInput("display", label="Display summaries of sampling distribution"),
      htmlOutput("display")
      ))
  )

))
