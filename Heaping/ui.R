# -----------------------------------------------------------------
# App Title: Heaped Distribution Estimation
#    Author: Jimmy Wong
# -----------------------------------------------------------------

##############################################
##############################################
## Libraries
##############################################
##############################################

if (!require("shinyBS")) install.packages("shinyBS")
if (!require("devtools")) install.packages("devtools")
if (!require("shinyIncubator")) devtools::install_github("rstudio/shiny-incubator")

library(devtools)
library(shinyBS)
library(shinyIncubator)

##############################################################################################################
##############################################################################################################
## Shiny user interface
##############################################################################################################
##############################################################################################################

shinyUI(fluidPage(  
  tags$head(tags$link(rel = "icon", type = "image/x-icon", 
                      href = "https://webresource.its.calpoly.edu/cpwebtemplate/5.0.1/common/images_html/favicon.ico")),
  
  progressInit(),
  
  titlePanel("Heaped Distribution Estimation"),
  
  tabsetPanel(type="tabs",
              tabPanel("Simulate Data",
                       column(12,
                              p("The goal of this study is to apply a", code("Mixture Model",style = "color:firebrick"), "to", code("Heaped",style = "color:firebrick"), "data.  
                                Heaped data is defined to be data in which irregular peaks exist in the distribution due to some form of bias.  
                                For example, a social network analysis study asked subjects to report the number of contacts on a given day.
                                Interviewers were told to instruct subjects to round to the nearest multiple of 5.  However, not all subjects
                                rounded to 5 after analyzing the distribution of number of contacts.  Therefore, there were peaks at multiples of 
                                5 and less frequencies at non-multiples of 5."),tags$hr()),
                       column(5,
                              p("Let",strong("C = actual value"), "from the underlying distribution."),
                              p("To begin this study, please select an underlying distribution to randomly generate data to be used in
                                subsequent sections.  If you choose to upload your own data, the distribution of C is unknown and will be 
                                estimated by accounting for heaping."),
                              checkboxInput("dataupload", "Upload data", FALSE),br(),
                              conditionalPanel(
                                condition="!input.dataupload",
                                uiOutput("distselect"),
                                selectInput("distdat", strong("Distribution:"),
                                            choices=list("Negative Binomial","Beta Binomial","Conway-Maxwell Poisson","Delaporte", "Yule-Simon"),
                                            selected="Negative Binomial"),
                                conditionalPanel(
                                  condition="input.distdat=='Negative Binomial'",
                                  sliderInput("size.dat", "\\(r\\):", min=0, max=100, value=3, step=1),
                                  sliderInput("mu.dat", "\\(\\mu\\):", min=0, max=100, value=10, step=1)),
                                conditionalPanel(
                                  condition="input.distdat=='Beta Binomial'",
                                  sliderInput("rbb.dat", "\\(r\\):", min=0, max=500, value=40, step=20),
                                  sliderInput("pbb.dat", "\\(Pr\\):", min=0, max=1, value=.25, step=.05),
                                  sliderInput("corbb.dat", "\\(\\rho\\):", min=0, max=1, value=.1, step=.05)),
                                conditionalPanel(
                                  condition="input.distdat=='Conway-Maxwell Poisson'",
                                  sliderInput("lam.dat", "\\(\\lambda\\):", min=1, max=100, value=7, step=.05),
                                  sliderInput("nu.dat", "\\(\\nu\\):", min=1, max=10, value=1, step=.05)),
                                conditionalPanel(
                                  condition="input.distdat=='Delaporte'",
                                  sliderInput("adel.dat", "\\(\\alpha\\):", min=0, max=100, value=1, step=.5),
                                  sliderInput("bdel.dat", "\\(\\beta\\):", min=0, max=100, value=7, step=.5),
                                  sliderInput("cdel.dat", "\\(\\lambda\\):", min=0, max=100, value=1, step=.5)),
                                conditionalPanel(
                                  condition="input.distdat=='Yule-Simon'",
                                  sliderInput("rho.dat", "\\(\\rho\\):", min=0, max=100, value=2, step=1)),
                                sliderInput("nsim.dat", "\\(sample\\,size\\):", min=100, max=5000, value=500, step=100),br(),
                                actionButton("beginstudy", strong("Simulate data"))),
                              conditionalPanel(
                                condition="input.dataupload",
                                column(3,
                                       fileInput("file", "",accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                                       checkboxInput("header", "Header", TRUE),
                                       radioButtons("sep", "Separator:", choices=c(Comma=",",Semicolon=";",Tab="\t"), selected=","),
                                       radioButtons("quote", "Quote", choices=c(None="","Double Quote"='"',"Single Quote"="'"),selected="")),br(),br())),
                       column(7,
                              plotOutput("simdatplot"))),
              
              tabPanel("Heaping Process",
                       withMathJax(), br(),
                       column(4,
                              p("Let", strong("Y = observed value"), "from the heaped (rounded) distribution."),  
                              p("Consider two rounding probabilities \\(\\alpha_1\\) and \\(\\alpha_2\\) such that:"),
                              uiOutput("prob1"),
                              bsPopover("prob1","Interpretation","&alpha;<sub>1</sub>: probability of rounding to 5, given that the actual value is either 1, 2, 3, or 4",
                                        trigger="hover",placement="right"),
                              uiOutput("prob2"),
                              bsPopover("prob2","Interpretation","&alpha;<sub>2</sub>: probability of rounding to the nearest multiple of 5, given that the actual value is greater than 5 and not a multiple of 5",
                                        trigger="hover",placement="right"),br(),
                              em("Hoover over above for more info"),
                              conditionalPanel(
                                condition="!input.dataupload",
                                tags$hr(),
                                p("To begin the rounding process, select values for the two rounding probabilities.  The result is a distribution
                                with heaping present."),
                                strong("Rounding probabilities:"),br(),
                                sliderInput("alpha1", p("\\(\\alpha_1\\):"), min=0, max=1, value=.3, step=.05),
                                sliderInput("alpha2", p("\\(\\alpha_2\\):"), min=0, max=1, value=.6, step=.05),br(),
                                actionButton("beginround", strong("Round data")),br(),br())),
                       column(8,
                              plotOutput("heapeddatplot"),
                              uiOutput("heapedpop")),
                       column(12)),
              
              tabPanel("Maximum Likelihood Estimation",
                       column(4,br(),
                              p("Based on the heaped data,", code("Maximum Likelihood Estimation",style = "color:navy"), "is used to obtain the
                                estimates of the underlying distribution's parameters and of the two rounding probabilities."),tags$hr(),
                              p("To start the optimization process, please select a range of starting values for each parameter
                                of interest."),
                              bsAlert(inputId="optimtime"),
                              conditionalPanel(
                                condition="input.dataupload",
                                selectInput("distdat1", strong("Distribution:"),
                                            choices=list("Negative Binomial","Beta Binomial","Conway-Maxwell Poisson","Delaporte", "Yule-Simon"),
                                            selected="Negative Binomial")),
                              strong("Starting values:"),br(),
                              sliderInput("alpha1.mle", "\\(\\alpha_1\\):", min=0, max=1, value=c(.2,.4), step=.1),
                              sliderInput("alpha2.mle", "\\(\\alpha_2\\):", min=0, max=1, value=c(.5,.7), step=.1),
                              conditionalPanel(
                                condition="(!input.dataupload && input.distdat=='Negative Binomial') || 
                                           (input.dataupload && input.distdat1=='Negative Binomial')",
                                sliderInput("size.mle.nb", "\\(r\\):", min=0, max=100, value=c(0,3), step=1),
                                sliderInput("mu.mle.nb", "\\(\\mu\\):", min=0, max=100, value=c(7,10), step=1)),
                              conditionalPanel(
                                condition="(!input.dataupload && input.distdat=='Beta Binomial') || 
                                           (input.dataupload && input.distdat1=='Beta Binomial')",
                                sliderInput("size.mle.bb", "\\(r\\):", min=0, max=100, value=c(39,43), step=1),
                                sliderInput("prob.mle.bb", "\\(Pr\\):", min=0, max=1, value=c(.4,.5), step=.05),
                                sliderInput("rho.mle.bb", "\\(\\rho\\):", min=0, max=1, value=c(.05,.15), step=.05)),
                              conditionalPanel(
                                condition="(!input.dataupload && input.distdat=='Conway-Maxwell Poisson') || 
                                           (input.dataupload && input.distdat1=='Conway-Maxwell Poisson')",
                                sliderInput("lam.mle.cmp", "\\(\\lambda\\):", min=1, max=100, value=c(3,10), step=1),
                                sliderInput("nu.mle.cmp", "\\(\\nu\\):", min=1, max=10, value=c(1,4), step=1)),
                              conditionalPanel(
                                condition="(!input.dataupload && input.distdat=='Delaporte') || 
                                           (input.dataupload && input.distdat1=='Delaporte')",
                                sliderInput("a.mle.del", "\\(\\alpha\\):", min=0, max=100, value=c(.5,4), step=.5),
                                sliderInput("b.mle.del", "\\(\\beta\\):", min=0, max=100, value=c(4,9), step=.5),
                                sliderInput("c.mle.del", "\\(\\lambda\\):", min=0, max=100, value=c(.5,4), step=.5)),
                              conditionalPanel(
                                condition="(!input.dataupload && input.distdat=='Yule-Simon') || 
                                           (input.dataupload && input.distdat1=='Yule-Simon')",
                                sliderInput("rho.mle.ys", "\\(\\rho\\):", min=0, max=100, value=c(1,5), step=1)),br(),
                              actionButton("startoptim", strong("Start optimization")),br(),br()),
                       column(8,
                              conditionalPanel(
                                condition="input.startoptim>0",
                                plotOutput("estdistplot"),br(),
                                tableOutput("mlevalstable"),tags$hr(),
                                plotOutput("loglikvalsplot"))),
                       column(12)),
              
              tabPanel("Confidence Intervals",
                       br(),
                       column(4,
                              p(strong("Confidence intervals based on"), code("Inverse Fisher info matrix",style = "color:navy")),
                              HTML("Using the asymptotic normality of the MLE:
                                   <ol> <li> Invert the Fisher information matrix </li> </ol>"),
                              p("Calculate CI's using the diagonal of the inverted matrix as the SE's.",style="color:navy"), br(),
                              actionButton("startci", strong("Calculate confidence intervals"))),
                       column(8,
                              tableOutput("ci"),
                              uiOutput("invert")),
                       column(1),
                       column(12,tags$hr()),
                       column(4,br(),
                              p(strong("Confidence intervals based on"), code("Bootstrapping",style = "color:navy")),
                              HTML("In each of the 500 iterations:
                                    <ol> <li> Resample from the heaped data </li>
                                    <li> Use the original MLE's as starting values to find the MLE's for the resampled data </li> </ol>"),
                              p("Calculate CI's using the 2.5 and 97.5 percentiles of the bootstrap distributions.",style="color:navy"),br(),
                              actionButton("startciboot", strong("Calculate bootstrap confidence intervals")),br(),br(),
                              bsAlert(inputId="boottime")),
                       column(8,br(),
                              tableOutput("boottable"),br(),
                              plotOutput("bootdist"),
                              uiOutput("bootdistpop"))),
                       
              tabPanel("Method Validation",
                       column(4,br(),
                              p("To verify the validity of this statistical method, a simulation study is performed.  The purpose is to ensure the correct
                                parameters are being estimated."),
                              HTML("In each iteration:
                                   <ol> <li> Randomly generate data from the specified distribution (in first tab)</li>
                                   <li> Perform the heaping process on the data based on the specified rounding probabilities (in second tab)</li> 
                                   <li> Use the specified underlying parameters as starting values to optimize the loglikelihood function </li> </ol>"),
                              p("If data were uploaded, the MLE's are used as the underlying parameters."),
                              p("The means of the MLE distributions should be close to the specified underlying parameters.",style = "color:navy"),tags$hr(),
                              sliderInput("nsim.mv", "Number of simulations:", min=10, max=500, value=10, step=10),
                              actionButton("startvalid", strong("Start simulations")),br(),br(),
                              bsAlert(inputId="simtime"),br(),br()),
                       column(8,
                              plotOutput("simplots"),
                              uiOutput("plots.sim.pop"),br(),
                              tableOutput("means.sim.tab"))))
                          
))
