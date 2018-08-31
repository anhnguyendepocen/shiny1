##############################################
# App Title: Probability Distribution Viewer #
#           Author: Irvin Alcaraz            #
##############################################


if (!require("devtools"))
  install.packages("devtools")


if (!require("shinysky")) devtools::install_github("ShinySky","AnalytixWare")
library(shinysky)
library(shiny)

shinyUI(fluidPage(

  tags$head(tags$link(rel = "icon", type = "image/x-icon", 
                      href = "https://webresource.its.calpoly.edu/cpwebtemplate/5.0.1/common/images_html/favicon.ico")),

  titlePanel("Probability Viewer"),

  sidebarLayout(
    
    sidebarPanel(
          selectInput("dist",label=h4("Distribution"),
	                    choices=c("Beta" = "beta", "Cauchy" = "cauchy", "Chi-Squared" = "chisq",
						"Exponential" = "exp","F" = "f", "Gamma" = "gamma", 
						"Logistic" = "logis","Log Normal" = "lnorm", 
						"Normal"="norm","Student t" = "t","Uniform" = "unif", 
						"Weibull" = "weibull"),selected="norm"),       
						
          shinyalert("shinyalert1", TRUE, auto.close.after=5),
					shinyalert("shinyalert2", TRUE, auto.close.after=5),
					shinyalert("shinyalert3", TRUE, auto.close.after=5),
					shinyalert("shinyalert4", TRUE, auto.close.after=5),
					shinyalert("shinyalert5", TRUE, auto.close.after=5),
					shinyalert("shinyalert6", TRUE, auto.close.after=5),
					shinyalert("shinyalert7", TRUE, auto.close.after=5),
					shinyalert("shinyalert8", TRUE, auto.close.after=5),
					shinyalert("shinyalert9", TRUE, auto.close.after=5),
					shinyalert("shinyalert10", TRUE, auto.close.after=5),
					shinyalert("shinyalert11", TRUE, auto.close.after=5),
          
          conditionalPanel(condition = "input.dist=='beta'",
                          numericInput("p1.beta","First Shape",2,min=1)),
          conditionalPanel(condition = "input.dist=='beta'",
                          numericInput("p2.beta","Second Shape",2,min=1)),
          conditionalPanel(condition = "input.dist=='cauchy'",
                          numericInput("p1.cauchy","Location",0)),
          conditionalPanel(condition = "input.dist=='cauchy'",
                          numericInput("p2.cauchy","Scale",2,min=1)),
	        conditionalPanel(condition = "input.dist=='chisq'",
				                  numericInput("p1.chisq","DF",5,min=1)),    
          conditionalPanel(condition = "input.dist=='exp'",
                          numericInput("p1.exp","Rate",1,min=0)),
          conditionalPanel(condition = "input.dist=='f'",
                          numericInput("p1.f","Num DF",20,min=1)),
	        conditionalPanel(condition = "input.dist=='f'",
				                  numericInput("p2.f","Denom DF",20,min=1)),
	        conditionalPanel(condition = "input.dist=='gamma'",
				                  numericInput("p1.gamma","Shape",1,min=0)),
          conditionalPanel(condition = "input.dist=='gamma'",
				                  numericInput("p2.gamma","Scale",1,min=0)),
	        conditionalPanel(condition = "input.dist=='logis'",
				                  numericInput("p1.logis","Location",0)),
	        conditionalPanel(condition = "input.dist=='logis'",
				                  numericInput("p2.logis","Scale",1,min=0)),
	        conditionalPanel(condition = "input.dist=='lnorm'",
				                  numericInput("p1.lnorm","Log Mean",0,min=0)),
	        conditionalPanel(condition = "input.dist=='lnorm'",
				                  numericInput("p2.lnorm","Log Standard Deviation",1,min=0)),
	        conditionalPanel(condition = "input.dist=='norm'",
	    			              numericInput("p1.norm","Mean",0)),
	        conditionalPanel(condition = "input.dist=='norm'",
                          numericInput("p2.norm","Standard Deviation",1,min = 0)),
	        conditionalPanel(condition = "input.dist=='t'",
				                  numericInput("p1.t","DF",5,min=1)),
	        conditionalPanel(condition = "input.dist=='unif'",
				                  numericInput("p1.unif","Minimum",0)),
	        conditionalPanel(condition = "input.dist=='unif'",
				                  numericInput("p2.unif","Maximum",1)),
	        conditionalPanel(condition = "input.dist=='weibull'",
				                  numericInput("p1.weibull","Shape",1,min=0)),
	        conditionalPanel(condition = "input.dist=='weibull'",
				                  numericInput("p2.weibull","Scale",1,min=0)),
					
          HTML("<hr style='height: 2px; color: #F3F3F3; background-color: #F3F3F3; border: none;'>"),
	        
          radioButtons("type",label=h4("Define Shaded Area By"),
	                    choices=c("Input percentile and calculate probability"="d",
			     	                    "Input probability and calculate percentile"="p", "Nothing"="none"),selected="none"),
					
          HTML("<hr style='height: 2px; color: #F3F3F3; background-color: #F3F3F3; border: none;'>"),
            
          conditionalPanel(condition = "input.type != 'none'",
	                         selectInput("shade",label=h4("Area to shade"),
	                                     choices=c("Left Tail"="left","Right Tail"="right",
	                                               "Both Tails"="both","Middle"="middle"))),
            
# 	        conditionalPanel(condition = "input.shade == 'left' || input.shade == 'right' ||
# 	                                      input.shade == 'both' || input.shade == 'middle' ",
          conditionalPanel(condition = "input.type != 'none'",
                          numericInput("shadeval1",label=" ",0)),
            
	        conditionalPanel(condition = "(input.shade == 'both' || input.shade=='middle') &&
	        							  input.type != 'none'",
        	                numericInput("shadeval2",label=" ",value=0)),

					#HTML("<hr style='height: 2px; color: #F3F3F3; background-color: #F3F3F3; border: none;'>"),
	        actionButton("go","Submit"),
            
						div("Shiny app by", 
						    a(href="https://www.linkedin.com/in/irvinalcaraz",target="_blank", 
						      "Irvin Alcaraz"),align="right", style = "font-size: 8pt"),
						
						div("Base R code by", 
						    a(href="https://www.linkedin.com/in/irvinalcaraz",target="_blank", 
						      "Irvin Alcaraz"),align="right", style = "font-size: 8pt"),
						
						div("Shiny source files:",
						    a(href="https://gist.github.com/calpolystat/200f26d243f4f5bb7334",
						      target="_blank","GitHub Gist"),align="right", style = "font-size: 8pt"),
						
						div(a(href="http://www.statistics.calpoly.edu/shiny",target="_blank", 
						      "Cal Poly Statistics Dept Shiny Series"),align="right", style = "font-size: 8pt")
            
    ),

  mainPanel(
      plotOutput("distPlot"),
      
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
      )
      
  )
  )
))
