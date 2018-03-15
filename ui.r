library(ggplot2)
library(reshape2)
shinyUI(navbarPage(
  "Population Genetics",
  
  tabPanel("Make Your Population",
  tags$head(tags$link(rel = "icon", type = "image/x-icon", href =
       "https://webresource.its.calpoly.edu/cpwebtemplate/5.0.1/common/images_html/favicon.ico")),
           
  fluidRow(
    column(4,
      numericInput("start", tags$div( 
        HTML(paste("Starting %")),
        HTML(paste("A", tags$sup(1), sep = ""))
         ), 50),
      numericInput("gen", "Number of Generations", 100),
      numericInput("pop", "Starting Population", 100),
      numericInput("AA", 
                   #withMathJax(helpText('$$A^1$$')),
                   tags$div(
                     HTML(paste("A", tags$sup(1), sep = "")), 
                     HTML(paste("A", tags$sup(1), sep = "")),
                     HTML(paste("Fitness %"))
                   ), 
                   100),
      numericInput("AB", 
                   tags$div(
                     HTML(paste("A", tags$sup(1), sep = "")), 
                     HTML(paste("A", tags$sup(2), sep = "")),
                     HTML(paste("Fitness %"))
                   )
                   , 100),
      numericInput("BB", 
                   tags$div(
                     HTML(paste("A", tags$sup(2), sep = "")), 
                     HTML(paste("A", tags$sup(2), sep = "")),
                     HTML(paste("Fitness %"))
                   ), 100),
      actionButton("begin","Click To Begin"),
      uiOutput("uiButton1"),
      uiOutput("uiButton2"),
      uiOutput("uiButton3"),
      br(), br(), br(), br(), 
      
      div("Shiny app by", a(href="https://www.linkedin.com/in/helen-hilton-39aba269",target="_blank", 
                            "Helen Hilton"),align="left", style = "font-size: 8pt"),
      
      div("Base R code by", a(href="https://www.linkedin.com/in/helen-hilton-39aba269/",target="_blank", 
                              "Helen Hilton"),align="left", style = "font-size: 8pt"),
      
      div("Shiny source files:", a(href="https://gist.github.com/calpolystat/f9dd35b71dc3e73b845160018ecacea2",
                                   target="_blank","GitHub Gist"),align="left", style = "font-size: 8pt"),
      
      div(a(href="http://www.statistics.calpoly.edu/shiny",target="_blank", 
            "Cal Poly Statistics Dept Shiny Series"),align="left", style = "font-size: 8pt")
      
    ),
    
    column(4,
           absolutePanel(
             top=125, bottom=0, left=320, right=0, fixed=TRUE,
             #              h3("Population 1"),
             imageOutput("Population1")
             
           )
    ),
    
    column(4,
           absolutePanel(
             top=425, bottom=0, left=320, right=0, fixed=TRUE,
             #              h3("Population 2"),
             imageOutput("Population2")
             
           )
    ),
    
    column(4,
           absolutePanel(  #Prints information for Population #1
             top=170, bottom=0, left=670, right=0, fixed=TRUE,
             tags$style(type="text/css",
                        ".shiny-output-error { visibility: hidden; }",
                        ".shiny-output-error:before { visibility: hidden; }"),
             uiOutput("start1"),
             textOutput("gen1"),
             textOutput("pop1"),
              textOutput("AA1"),
              textOutput("AB1"),
              textOutput("BB1")
           ), #absolutePanel
           absolutePanel(  #Prints information for Population #2
             top=470, bottom=0, left=670, right=0, fixed=TRUE,
              textOutput("start2"),
              textOutput("gen2"),
              textOutput("pop2"),
              textOutput("AA2"),
              textOutput("AB2"),
              textOutput("BB2")
           )
           
    ) #column
    
    
  ) #fluidRow
), #tabPanel

tabPanel("Simulation",
         sidebarLayout(
                  
           sidebarPanel(
             
             numericInput("drawmorepops", "Number of Simulations", 1),
             actionButton("drawpop", "Simulate"),
             br(), br(), br(), br(), 
             
             div("Shiny app by", a(href="https://www.linkedin.com/in/helen-hilton-39aba269/",target="_blank", 
                                   "Helen Hilton"),align="right", style = "font-size: 8pt"),
             
             div("Base R code by", a(href="https://www.linkedin.com/in/helen-hilton-39aba269/",target="_blank", 
                                     "Helen Hilton"),align="right", style = "font-size: 8pt"),
             
             div("Shiny source files:", a(href="https://gist.github.com/calpolystat/f9dd35b71dc3e73b845160018ecacea2",
                                          target="_blank","GitHub Gist"),align="right", style = "font-size: 8pt"),
             
             div(a(href="http://www.statistics.calpoly.edu/shiny",target="_blank", 
                   "Cal Poly Statistics Dept Shiny Series"),align="right", style = "font-size: 8pt")
             
           ),#sidebarPanel  
           mainPanel( 
             plotOutput("distPlot")
           )#mainPanel
         )#sidebarLayout
         
) #tabPanel
) #navbarPage
) #shinyUI