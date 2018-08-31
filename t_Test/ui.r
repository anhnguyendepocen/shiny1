# -----------------------
# App Title: t-test
# Author: Jimmy Wong
# -----------------------

if (!require("ggplot2"))
  install.packages("ggplot2")

if (!require("shinyBS"))
  install.packages("shinyBS")

shinyUI(fluidPage(
  tags$head(tags$link(rel = "icon", type = "image/x-icon", 
                      href = "https://webresource.its.calpoly.edu/cpwebtemplate/5.0.1/common/images_html/favicon.ico")),
  
  navbarPage(title="t-test",
             
             tabPanel("Information of t-test",
                      
                      column(1),
                      column(5,br(),br(),br(),
                        withMathJax(p("This application allows users to perform either a", code("one-sample t-test",style="color:navy"), 
                        "or a", code("two-sample t-test",style="color:navy"),".  A one-sample t-test focuses on comparing the average of a 
                        single quantitative variable to a hypothesized value, while a two-sample t-test
                        focuses on comparing the difference in averages of a quantitative variable between two groups to a hypothesized value.  In
                        both scenarios, the purpose of the hypothesis test is to determine how likely are the observed results or any more extreme results, 
                        under the assumption that the null hypothesis is true. This is known as a", strong("p-value.")),
                        p("In most data analyses, the population mean(s) along with the population standard deviation(s) are unknown.  
                          Therefore, the", strong("t-test"), "is used instead of a z-test.  The", strong("t-statistic"), "can be calculated to determine the p-value,
by comparing it to the", strong("t-distribution"), "with a", strong("specified degrees of freedom."), "In this scenario, the sample standard deviation(s) replaces the population standard deviation(s) to yield
                          the", strong("standard error"), "(an estimate of the true standard deviation) of the", strong("sampling distribution.")))),
                      
                      column(5,br(),br(),br(),
                        wellPanel(
                        code("One-sample t-test:",style="color:navy"),
                        p(HTML("<ul> <li type=square> the parameter of interest is the population mean, &mu;<li type=square>"),p(),
                        p("t-statistic = \\(\\frac{\\bar x -\\mu_0}{s_{x}/\\sqrt{n}}\\)"), HTML("</ul>")),br(),
                        code("Two-sample t-test:",style="color:navy"),
                        p(HTML("<ul> <li type=square> the parameter of interest is the difference between the two population means, &mu;<sub>1</sub>-&mu;<sub>2</sub> <li type=square>"),p(),
                        p("t-statistic = \\(\\frac{(\\bar x_1 - \\bar x_2) -(\\mu_1-\\mu_2)}{\\sqrt{\\frac{s_{1}^2}{n_1} + \\frac{s_{2}^2}{n_2}}}\\)"), HTML("</ul>")))),
                      
                      column(1)),
             
             tabPanel("Data Exploration",
                      tabsetPanel(
                        tabPanel("Sample Data",
                                 fluidRow(
                                   column(3,
                                          wellPanel(
                                            selectInput("sampdat", "Choose a data set:", choices=list("One-sample"=1,"Two-sample"=2), selected=1),
                                            bsPopover(id="sampdat", title="Data set information", content="The one-sample data set is collected from the Old Faithful geyser in Yellowstone National Park, Wyoming, USA.  The variable of interest is the duration of each eruption in minutes.  The two-sample data set is associated with the 1974 Motor Trend US magazine.  The explanatory variable is the type of transmission and the response variable is horsepower.",
                                                      trigger="hover",placement="right"),
                                            checkboxInput("usedata", "Use sample data", TRUE),
                                            bsTooltip("usedata","Remember to uncheck this when not using sample data!","right"),
                                            br(),br(),br(),
                                            div("Shiny app by", 
                                                a(href="http://www.linkedin.com/in/jimmywong46/",target="_blank","Jimmy Wong"),align="right", style = "font-size: 8pt"),
                                            div("Base R code by", 
                                                a(href="http://www.linkedin.com/in/jimmywong46/",target="_blank","Jimmy Wong"),align="right", style = "font-size: 8pt"),
                                            div("Shiny source files:",
                                                a(href="https://gist.github.com/calpolystat/48dc47f3ff436aba4b19",
                                                  target="_blank","GitHub Gist"),align="right", style = "font-size: 8pt"),
                                            div(a(href="http://www.statistics.calpoly.edu/shiny",target="_blank", 
                                                  "Cal Poly Statistics Dept Shiny Series"),align="right", style = "font-size: 8pt"))),
                                   column(9,
                                          conditionalPanel(
                                            condition="input.usedata",
                                            dataTableOutput("data.tab1"))))),
                        
                        tabPanel("Upload Data",
                                 fluidRow(
                                   column(3,
                                          wellPanel(
                                            fileInput("file", "",accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                                            bsPopover("file","Note", "Remember to select the correct data format after uploading file!  Hover over the Select data format panel for more information.",
                                                      trigger="hover",placement="right"),
                                            tags$hr(),
                                            radioButtons("datformat", strong("Select data format:"), choices=c("1-sample"=1,Stacked=2,Unstacked=3), selected=1),
                                            bsPopover("datformat","Data format", "Select Stacked for 2-sample with explanatory and response variables in two columns.  Select Unstacked with explanatory variable as column names and response variable in two columns",
                                                      trigger="hover",placement="right"),
                                            tags$hr(),
                                            strong("Customize file format:"),
                                            checkboxInput("header", "Header", TRUE),
                                            radioButtons("sep", "Separator:", choices=c(Comma=",",Semicolon=";",Tab="\t"), selected=","),
                                            radioButtons("quote", "Quote", choices=c(None="","Double Quote"='"',"Single Quote"="'"),selected=""),
                                            br(),br(),br(),
                                            div("Shiny app by", 
                                                a(href="http://www.linkedin.com/in/jimmywong46/",target="_blank","Jimmy Wong"),align="right", style = "font-size: 8pt"),
                                            div("Base R code by", 
                                                a(href="http://www.linkedin.com/in/jimmywong46/",target="_blank","Jimmy Wong"),align="right", style = "font-size: 8pt"),
                                            div("Shiny source files:",
                                                a(href="https://gist.github.com/calpolystat/48dc47f3ff436aba4b19",
                                                  target="_blank","GitHub Gist"),align="right", style = "font-size: 8pt"),
                                            div(a(href="http://www.statistics.calpoly.edu/shiny",target="_blank", 
                                                  "Cal Poly Statistics Dept Shiny Series"),align="right", style = "font-size: 8pt"))),
                                   column(9,
                                          conditionalPanel(
                                            condition="input.file!='NULL'",
                                            dataTableOutput("data.tab"))))),
                        
                        tabPanel("Visualize Data",
                                 fluidRow(
                                   column(6,
                                          plotOutput("datagraph")),
                                   column(6,br(),br(),
                                          checkboxInput("displaystats", "Summary statistics", FALSE),
                                          tableOutput("summarystats")))))),
             
             tabPanel("Hypothesis Test",
                      fluidRow(
                        column(3,
                               wellPanel(
                                 conditionalPanel(
                                   condition="input.datformat==1 && input.sampdat==1",
                                   h4("Hypotheses:"),
                                   uiOutput("hypo1"),
                                   tags$hr(),
                                   numericInput("null1", label="Hypothesized value:", value=0),
                                   selectInput("alt1", "Select a direction for Ha:", choices=list("two-sided","less than","greater than"),selected="two-sided")),
                                 conditionalPanel(
                                   condition="input.datformat!=1 || input.sampdat==2",
                                   h4("Hypotheses:"),
                                   uiOutput("hypo2"),
                                   tags$hr(),
                                   numericInput("null2", label="Hypothesized value:", value=0),
                                   selectInput("alt2", label="Select a direction for Ha:", choices=list("two-sided","less than","greater than"),selected="two-sided")),
                                 sliderInput("alpha", label=HTML("Significance level &alpha;:"), value=.05, max=1, min=0, step=.01),
                                 tags$hr(),
                                 actionButton("teststart", strong("Perform t-test")),
                                 bsPopover("teststart","Note","Remember to check the normality condition before performing t-test.",trigger="hover",placement="right"),
                                 br(),br(),br(),
                                 div("Shiny app by", 
                                     a(href="http://www.linkedin.com/in/jimmywong46/",target="_blank","Jimmy Wong"),align="right", style = "font-size: 8pt"),
                                 div("Base R code by", 
                                     a(href="http://www.linkedin.com/in/jimmywong46/",target="_blank","Jimmy Wong"),align="right", style = "font-size: 8pt"),
                                 div("Shiny source files:",
                                     a(href="https://gist.github.com/calpolystat/48dc47f3ff436aba4b19",
                                       target="_blank","GitHub Gist"),align="right", style = "font-size: 8pt"),
                                 div(a(href="http://www.statistics.calpoly.edu/shiny",target="_blank", 
                                       "Cal Poly Statistics Dept Shiny Series"),align="right", style = "font-size: 8pt"))),
                        
                        column(9,
                               br(),br(),
                               conditionalPanel(
                                 condition="input.teststart>0",
                                 column(7,
                                        plotOutput("tdist"),
                                        bsPopover("tdist","p-value","The p-value is the shaded region. A large p-value indicates to fail to reject Ho and no evidence for Ha.  A small p-value indicates to reject the Ho and evidence for Ha.",
                                                  trigger="hover",placement="left"),br()),
                                 column(5,br(),
                                        strong("Test output:"),
                                        tableOutput("test"),br(),
                                        checkboxInput("showpoint","Point estimate(s):",FALSE),
                                        uiOutput("est"),
                                        checkboxInput("ci","Confidence interval:", FALSE),
                                        tableOutput("citab"),
                                        bsPopover("citab","Confidence interval sample interpretation","We are 95% confident that the true parameter is anywhere between the lower bound to the upper bound.",
                                                  trigger="hover",placement="bottom")))))),
             
             tabPanel("Normality Condition",
                      column(1),
                      column(4,br(),
                             p(strong("Normality test:"),"The sampling distribution of the sample means or differences in the sample means is 1) Normal if the population(s) is Normal or 2) approximately Normal if the 
                               sample size(s) is large enough (at least 30).  In situations with small sample size(s), the approach to access 
                               whether the sample data could have came from a Normal distribution is either through a Q-Q plot or a Normality test."),
                             actionButton("normstart", label=strong("Perform normality test")),br(),br(),
                             conditionalPanel(
                               condition="input.normstart>0",
                               wellPanel(
                               strong("Ho: data is from a Normal distribution"),br(),
                               strong("Ha: data is not from a Normal distribution"),br(),br(),
                               em("Shapiro-Wilk Normality Test:"),
                               tableOutput("sw"),
                               bsPopover("sw","Normality test","In a normality test, a large p-value does not provide evidence that the sample data is not from a Normal distribution.",
                                         trigger="hover",placement="bottom")))),
                      column(6,
                             conditionalPanel(
                               condition="input.normstart>0",br(),br(),
                               plotOutput("qqplot"),
                               bsPopover("qqplot","Q-Q plot","In a Q-Q plot, points that resemble a diagonal line with no curvature implies that the sample data could have came from a Normal distribution.",
                                         trigger="hover",placement="left"))),
                      column(1))
             
)))

