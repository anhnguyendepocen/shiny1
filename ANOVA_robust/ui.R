# ------------------
# App Title: Robustness of ANOVA F-test to violation of constant variance
#    Author: Gail Potter
# ------------------

if (!require("devtools"))
  install.packages("devtools")

if (!require("shinyBS")) install.packages("shinyBS")
library(shinyBS)

if (!require("digest")) install.packages("digest")

if (!require("shinyIncubator")) devtools::install_github("rstudio/shiny-incubator")

library(shinyIncubator)

shinyUI(fluidPage(
  
  tags$title("Robustness of ANOVA"),
  
  includeCSS('styles.css'),
  
  #progressInit(),
  
  tags$head(tags$link(rel = "icon", type = "image/x-icon", href =  
                        "https://webresource.its.calpoly.edu/cpwebtemplate/5.0.1/common/images_html/favicon.ico")),  
  
  h3("How robust is the ANOVA F-test to violation of constant variance?"),
  fluidRow(
    column(3,
           wellPanel( 
             h5("Specifications for ANOVA", style="color:brown"),
             
             h5("Population standard deviations:"),
             sliderInput("sigma1", label="Group 1", value=6, min=1, max=20),
             sliderInput("sigma2", label="Group 2", value=6, min=1, max=20),
             sliderInput("sigma3", label="Group 3", value=6, min=1, max=20),
             br(),
             h5("Sample sizes"),
             sliderInput("n1", label="Group 1", value=20, min=2, max=100),
             sliderInput("n2", label="Group 2", value=20, min=2, max=100),
             sliderInput("n3", label="Group 3", value=20, min=2, max=100),
             br(),
             br(),       
             h5("Population means:"),
             sliderInput("mu1", label="Group 1", value=0, min=-5, max=5),
             sliderInput("mu2", label="Group 2", value=0, min=-5, max=5),
             sliderInput("mu3", label="Group 3", value=0, min=-5, max=5),
             
             
             div("Shiny app by", 
                 a(href="http://www.gailpotter.org",target="_blank", 
                   "Gail Potter"),align="right", style = "font-size: 8pt"),
             
             div("Base R code by", 
                 a(href="http://www.gailpotter.org",target="_blank", 
                   "Gail Potter"),align="right", style = "font-size: 8pt"),
             
             div("Shiny source files:",
                 a(href="https://gist.github.com/calpolystat/fad8ef712fc6f726640c",
                   target="_blank","GitHub Gist"),align="right", style = "font-size: 8pt"),
             
             div(a(href="http://www.statistics.calpoly.edu/shiny",target="_blank", 
                   "Cal Poly Statistics Dept Shiny Series"),align="right", style = "font-size: 8pt"))     
    ),
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    column(9, wellPanel(
      p("The ANOVA F-test is used to test for difference in means between groups, and requires
       the conditions of normality (or large sample size), independence, and constant variance in order to be valid.  This
       app evaluates robustness of the ANOVA F-test to violation of the constant variance condition. 
       At left, specify the sample sizes and standard deviations for each group.  Below left are simulated 
        data from normal distributions with the specified standard deviations and mean zero.
       In the right plot, the  F-statistic for the simulated data is added to the sampling distribution.  
      The critical value for a 0.05 significance test is shown in red."),
      
      sliderInput("nsim", label="Number of samples", value=1, min=1, max=1000),
      actionButton("go", label = "Draw samples"),
      actionButton("clear",label="Clear"),
      
      plotOutput("dotplot"),
      conditionalPanel(
        condition="input.mu1==input.mu2 & input.mu2==input.mu3",
        div("You have selected identical population means; you will analyze ", code("Type I error"))),
      conditionalPanel(
        condition="input.mu1!=input.mu2 || input.mu2!=input.mu3 || input.mu1!=input.mu3",
        div("You have selected different population means; you will analyze ", code("power"))),
      
      htmlOutput("typeI"),
      
      div(h4("Explorations"), style="color:brown"),
      
      
      p("1.  If conditions for ANOVA are satisfied, the Type I Error rate should be equal to 0.05.  Simulate data 
        that satisfy conditions and verify that this is true.  Perform several hundred simulations to get a good estimate
for the error rate."),
      p("2. Simulate samples of size 20 from populations with equal means and standard deviations 6, 6, and 6.  What is your Type I error rate?"),
      p("3. Simulate samples of size 20 from populations with equal means and standard deviations 4, 6, and 8.  Now what is your Type I error rate?"),
      p("4. Simulate samples of size 20 from populations with equal means and standard deviations 1, 6, and 11.  Now what is your Type I error rate?"),
      p("5. Do the error rates you found in 2, 3, or 4 vary by sample size, when sample sizes are equal?"),
      p("6. Next repeat your simulation study with sample sizes 10, 20, and 30.  How do results differ?"),
      p("7. Finally, repeat the above simulation studies, but specify population means to be -3, 0, and 3, so that you study the power of the test under different conditions.")
    ))
  )
)
)
