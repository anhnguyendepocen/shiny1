# ---------------------------------------
#  App Title: Random Variable Generation
#     Author: Peter Chi
# ---------------------------------------

library(shiny)
source("rvg.R")

shinyUI(navbarPage("Random Variable Generation",
    tabPanel("(1) Probability Integral Transform",
    fluidPage(withMathJax(),
	       tags$head(tags$link(rel = "icon", type = "image/x-icon", href = "https://webresource.its.calpoly.edu/cpwebtemplate/5.0.1/common/images_html/favicon.ico")),  
      h3("Random Variable Generation: Probability Integral Transform"),
      p("Suppose we would like to generate \\(X\\sim f\\), where \\(f\\) is the probability density function (pdf) of \\(X\\). If the corresponding
        cumulative distribution function (cdf) has a generalized inverse, then we can use the Probability Integral Transform. The only other requirement
        is that we have the ability to simulate \\(U\\sim Unif[0,1]\\)."),
          sidebarLayout(
       sidebarPanel(
         selectizeInput('pitEx', label=("Choose an example:"), choices=c(Exponential="Exponential",Other="Other")),
         conditionalPanel(
           condition="input.pitEx == 'Exponential'",
           sliderInput(inputId="lambda",label="Value of \\(\\lambda\\) (rate) parameter:",min=0.1,max=5,value=1,step=0.1),
           br(),br(),
           sliderInput(inputId="num.exp",label="Generate how many?",min=1,max=500,value=1,step=1),
           div(actionButton("clear.exp",label="Clear"),actionButton("go.exp",label="Generate"),align="right"),
           br(),
           uiOutput("totalcountExp")
         ),
         conditionalPanel(
           condition="input.pitEx == 'Other'",
           helpText("This example is with an arbitrary, un-named distribution (i.e.
                    one for which pre-packaged routines are unlikely to exist)."),
           br(),br(),
           sliderInput(inputId="num.linear",label="Generate how many?",min=1,max=500,value=1,step=1),
           div(actionButton("clear.linear",label="Clear"),actionButton("go.linear",label="Generate"),align="right"),
           br(),
           uiOutput("totalcountLin")
         ),
         p(),br(),br(),
         
         div("Shiny app by", a(href= "http://statweb.calpoly.edu/pchi/", target="_blank", "Peter Chi"),align="right", style = "font-size: 8pt"),
         div("Base R code by", a(href= "http://statweb.calpoly.edu/pchi/", target="_blank", "Peter Chi"),align="right", style = "font-size: 8pt"),
         div("Shiny source files:", a(href= "https://gist.github.com/calpolystat/cc55e764b757d8729963", target="_blank","GitHub Gist"),align="right", style = "font-size: 8pt"),
         div(a(href = "http://www.statistics.calpoly.edu/shiny", target="_blank", "Cal Poly Statistics Dept Shiny Series"),align="right", style = "font-size: 8pt")
       ),
       mainPanel(
         conditionalPanel(
                condition = "input.pitEx == 'Exponential'",  
                h3("Demonstration with \\(X \\sim Exp(\\lambda)\\):"),
                plotOutput("PITexpPlot",width="100%"),
 #               helpText("Most recent point is enlarged and green."),
 uiOutput("summaryExp"),
 uiOutput("invExp"),
                HTML("<hr style='height: 2px; color: #de7008; background-color: #df7109; border: none;'>"),
                p("Details:"),
                p("1) First we note that as \\(f(x)=\\lambda e^{-\\lambda x}\\) for an Exponential random variable, 
                  the cdf is thus \\(F(x) = 1-e^{-\\lambda x}\\), for \\(x \\geq 0\\)"),
                p("2) Next, the inverse of this is \\(F^{-1}(u) = \\frac{-ln(1-u)}{\\lambda}\\)"),
                p("3) Thus, we generate \\(U\\sim Unif[0,1]\\), and plug these values into \\(F^{-1}\\) to obtain generations of \\(X\\)")
          ),
          
          conditionalPanel(
            condition = "input.pitEx == 'Other'", 
            h3("Demonstration with \\(f(x) = \\frac{x}{8} \\cdot 1_{\\{0 \\leq x \\leq 4\\}}\\) "),
            plotOutput("PITlinearPlot",width="100%"),
#            helpText("Most recent point is enlarged and green."),
uiOutput("summaryLin"),
uiOutput("invLin"),
            HTML("<hr style='height: 2px; color: #de7008; background-color: #df7109; border: none;'>"),
            p("Details:"),
            p("1) First we note that the cdf is \\(F(x) = \\frac{x^2}{16}\\), for \\(0 \\leq x \\leq 4\\)"),
            p("2) Next, the inverse of this is \\(F^{-1}(u) = 4 \\sqrt{u}\\)"),
            p("3) Thus, we generate \\(U\\sim Unif[0,1]\\), and plug these values into \\(F^{-1}\\) to obtain generations of \\(X\\)")
        #    plotOutput("PITexpPlot",width="100%")
          )
#          )
)
)
)
),

    tabPanel("(2) Accept-Reject Algorithm",
             fluidPage(withMathJax(),
                       h3("Random Variable Generation: Accept-Reject Algorithm"),
                      p("Suppose we would like to generate \\(X\\sim f\\) but can neither do it directly
nor via the Probability Integral Transform (e.g. if the generalized inverse
of the cdf is unavailable). We can instead arrive at it via generating \\(Y\\sim g\\),
with only the following two necessary conditions:"),
                       p("1) \\(f\\) and \\(g\\) have the same support"),
                      p("2) We can find a constant \\(M\\) such that \\(f(x)/g(x) \\leq M \\) for all \\(x\\) "),
               sidebarLayout(
                 sidebarPanel(
                   selectInput("example", label=("Choose an example:"), choices=list("Beta"="Beta","Truncated Normal"="tnorm")),
                 conditionalPanel(
                   condition = "input.example == 'Beta'",
                  sliderInput(inputId="alpha",label="Value of \\(\\alpha\\) parameter:",min=1,max=5,value=1,step=0.1),
                   sliderInput(inputId="beta",label="Value of \\(\\beta\\) parameter:",min=1,max=5,value=1,step=0.1),
                   br(),br(),
                   sliderInput(inputId="num",label="Generate how many?",min=1,max=500,value=1,step=1),
                   div(actionButton("clear",label="Clear"),actionButton("go",label="Generate"),align="right"),
                  br(),
                  uiOutput("totalcount")
                 ),
                 conditionalPanel(
                   condition = "input.example == 'tnorm'",
                   br(),
                   sliderInput(inputId="tnormNum",label="Generate how many?",min=1,max=500,value=1,step=1),
                   div(actionButton("tnormClear",label="Clear"),actionButton("tnormGo",label="Generate"),align="right"),
                   br(),br(),
                   helpText("This example is with the standard normal distribution, truncated at 2 (i.e. allowing for values greater than or equal to 2 only)."),
                   br(),
                   helpText("It would indeed be possible to simulate a normal random variable truncated at 2 by using a pre-packaged
                            routine for a standard normal random variable (such as rnorm in R), and then discarding all values below 2."),
                  br(),
                  helpText("However, this would be extremely inefficient as we would discard more than 97% of all values that we generate.
                            With the accept-reject algorithm, we of course also discard values, but here we demonstrate that this method
                            has superior efficiency, in the sense that less than 97% of the generated values will be discarded.")
                   ),
                 
                 p(),br(),br(),
                 
                 div("Shiny app by", a(href= "http://statweb.calpoly.edu/pchi/", target="_blank", "Peter Chi"),align="right", style = "font-size: 8pt"),
                 div("Base R code by", a(href= "http://statweb.calpoly.edu/pchi/", target="_blank", "Peter Chi"),align="right", style = "font-size: 8pt"),
                 div("Shiny source files:", a(href= "https://gist.github.com/calpolystat/cc55e764b757d8729963", target="_blank","GitHub Gist"),align="right", style = "font-size: 8pt"),
                 div(a(href = "http://www.statistics.calpoly.edu/shiny", target="_blank", "Cal Poly Statistics Dept Shiny Series"),align="right", style = "font-size: 8pt")
                 
                 
                 ),
                 column(7,
                 conditionalPanel(
                   condition = "input.example == 'Beta'",                   
                   h3("Demonstration with \\(X \\sim Beta(\\alpha,\\beta)\\):"),
                   plotOutput("densityPlot",width="100%"),
                   uiOutput("summary"),
                   uiOutput("accrej"),
                   br(),
                   helpText("In the right panel: after initially being shown in red, rejected points remain in grey and stack down from the top; after initially
being shown in green, accepted
                            points remain in black and stack up from the bottom, to fill the shape of the theoretical pdf of \\(X\\)."),
                   textOutput("unifnote"),
                   br(),
                   HTML("<hr style='height: 2px; color: #de7008; background-color: #df7109; border: none;'>"),
                   p("Details:"),
                   p("1) The first step is to generate \\(Y \\sim g\\). In this example, we use \\(Y \\sim Unif[0,1]\\) 
                     (shown in the right panel, along with the true distribution that we are trying to simulate from). Notice
                     that the Unif[0,1] distribution does indeed have the same support as the Beta distribution."),
                   p("2) Next, we need to find an appropriate value of M. For the Beta example, 
                     we notice that the maximum of the Beta pdf would work."),
                   textOutput("M"),
                   br(),
                   p("3) We also generate \\(U \\sim Unif[0,1]\\) (left panel), and then accept \\(y\\) as a 
                     value of \\(X\\) if \\(U \\leq \\frac{f(y)}{Mg(y)}\\), and reject otherwise")
                 ),
                 conditionalPanel(
                   condition = "input.example == 'tnorm'",
                   h3("Demonstration with Truncated Normal"),
                   plotOutput("tnormDensityPlot",width="100%"),
                   uiOutput("tnormsummary"),
                   uiOutput("tnormaccrej"),
                   br(),
                   helpText("In the right panel: after initially being shown in red, rejected points remain in grey and stack down from the top; after initially
being shown in green, accepted
                            points remain in black and stack up from the bottom, to fill the shape of the theoretical pdf of \\(X\\)."),
                   br(),
                   uiOutput("tnormRatio"),
                   br(),
                 HTML("<hr style='height: 2px; color: #de7008; background-color: #df7109; border: none;'>"),
                 p("Details:"),
                 p("1) The first step is to generate \\(Y \\sim g\\). In this example, we will use \\(g(y) = e^{2-y} \\cdot 1_{\\{y \\geq 2\\}} \\) 
                     (shown in the right panel, along with the true distribution that we are trying to simulate from)."),
                 p("2) Next, we need to find an appropriate value of M. For this example, 
                     we notice the following:"),
                 p("$$\\frac{f(y)}{g(y)} = \\frac{\\frac{1}{\\sqrt{2 \\pi}}e^{-\\frac{1}{2}y^2}1_{\\{y \\geq 2\\}}
                    \\cdot \\left[\\frac{1}{1-\\Phi(2)}\\right] }{e^{2-y}1_{\\{y \\geq 2\\}}}$$"),
                 p("where \\(\\Phi\\) is the standard normal cdf.
                   It can be shown that this ratio is at its maximum at \\(y=2\\). Thus, \\(M=\\frac{\\phi(2)}{1-\\Phi(2)}\\) where \\(\\phi\\) is
                   the standard normal pdf."),
                 p("3) We also generate \\(U \\sim Unif[0,1]\\) (left panel), and then accept \\(y\\) as a 
                     value of \\(X\\) if \\(U \\leq \\frac{f(y)}{Mg(y)}\\), and reject otherwise")                 
                 )
                 )
               )
             )
    )
)
)
