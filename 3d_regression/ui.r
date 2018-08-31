# ------------------------------------------------
#  App Title: 3D Model Viewer
#     Author: Irvin Alcaraz
# ------------------------------------------------
options(rgl.useNULL=TRUE)
if (!require("devtools")){
  install.packages("devtools")
  library("devtools")
  }
if (!require("shiny")){
  install.packages("shiny")
  library("shiny")
  }
if (!require("rgl")){
  install.packages("rgl")
  library("rgl")
  }
if (!require("shinyRGL")){
  install.packages("shinyRGL")
  library("shinyRGL")
  }
if (!require("reshape2")){
  install.packages("reshape2")
  library("reshape2")
  }
if (!require("RColorBrewer")){
  install.packages("RColorBrewer")
  library("RColorBrewer")
  }

if (!require("rglwidget")){
  install.packages("rglwidget")
  library("rglwidget")
}

shinyUI(navbarPage("Multiple Regression Visualization",
  tabPanel("3D Visualizer",

  tags$head(tags$link(rel = "icon", type = "image/x-icon",
                      href = "https://webresource.its.calpoly.edu/cpwebtemplate/5.0.1/common/images_html/favicon.ico")),

  p("When creating a model, it can be very helpful to visualize both the data and the model.
    Often we wish to create a prediction model for a response variable on more than one predictors.
    In the case of a single response and two predictors, we must use a third dimension to visualize the
    the data and model."),
  p("In this app, you will be able to  visualize the data and explore the effectiveness of different models
    for a numerical response variable. "),

  sidebarLayout(


    sidebarPanel(

      tags$title("3D Visualizer"),

      selectInput("dataset",label = "Select a dataset", choices = c("Iris"= "iris",
                                                                    "Cars" = "mtcars",
                                                                    "U.S." = "state.x77"
                                                                    ##,"Custom" = "upload"
                                                                    )),

      ##To do the 2D this you need to uncomment the end and add the third option
      conditionalPanel(condition = "input.dataset == 'iris'",
                       radioButtons("expTypes1", label = "Available Models: Sepal Area = ",
                                    choices = list("Sepal Length + Sepal Width" = 1,
                                                   "Sepal Length * sepal Width" = 2,
                                                   "Sepal Length + Sepal Width + Species" = 4,
                                                   "Sepal Length * Species + Sepal Width * Species" = 6,
                                                   "None" = 5),

                                    selected = 5)),
      conditionalPanel(condition = "input.dataset == 'mtcars'",
                       radioButtons("expTypes2", label = "Available Models: MPG =",
                                    choices = list("Horsepower + Weight" = 1,
                                                   "Horsepower * Weight" = 2,
                                                   "Horsepower + Weight + Transmission" = 4,
                                                   "Horsepower * Transmission + Weight * Transmission" = 6,
                                                   "None" = 5),
                                    selected = 5)),
      conditionalPanel(condition = "input.dataset == 'state.x77'",
                       radioButtons("expTypes3", label = "Available Models: Life Expectancy =",
                                    choices = list("Murder Rate + HS Graduation Rate" = 1,
                                                   "Murder * HSGrad" = 2,
                                                   "Murder + HSGrad + Region" = 4,
                                                   "Murder * Region + HSGrad * Region" = 6,
                                                   "None" = 5),
                                    selected = 5)),

      ####For file upload
#       conditionalPanel(condition = "input.dataset === 'upload'",
#                        fileInput("file", "Browse for a file",
#                                  accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
#                        strong("Please use numerical data formated as N,N,N"),
# #                        radioButtons("colTypes","C = Categorical, N = Numerical",
# #                                     c("N,N,N"=1,"N,N,N,C"=2,"N,N,C,N"=3,"N,C,N,N"=4),1),
#                        strong("Customize file format:"),
#                        checkboxInput("header", "Header", TRUE),
#                        radioButtons("sep", "Separator:",
#                                     c(Comma=",",Semicolon=";",Tab="\t"), ","),
#                        radioButtons("quote", "Quote",
#                                     c(None="","Double Quote"='"',"Single Quote"="'"), ""),
#                        strong("Check box to include interaction"),
#                        checkboxInput("interaction","", FALSE)
#                        ),

      div("Shiny app by",
          a(href="https://www.linkedin.com/in/irvinalcaraz",target="_blank",
            "Irvin Alcaraz"),align="right", style = "font-size: 8pt"),

      div("Base R code by",
          a(href="https://www.linkedin.com/in/irvinalcaraz",target="_blank",
            "Irvin Alcaraz"),align="right", style = "font-size: 8pt"),

      div("Shiny source files:",
          a(href="https://gist.github.com/calpolystat/bd0400c7ce3aacfa4973",
            target="_blank","GitHub Gist"),align="right", style = "font-size: 8pt"),

      div(a(href="http://www.statistics.calpoly.edu/shiny",target="_blank",
            "Cal Poly Statistics Dept Shiny Series"),align="right", style = "font-size: 8pt")


    ),
    mainPanel(

      tabsetPanel(
#         conditionalPanel(condition = "(!is.na(input.expTypes1) && input.expTypes1 != 3) || input.expTypes2 != 3 || input.expTypes3 != 3",
                         tabPanel("Plot",rglwidgetOutput("troisPlot",width="600px",height="600px")),
#                          ),
#         conditionalPanel(condition = "(!is.na(input.expTypes1) && input.expTypes1 == 3) || input.expTypes2 == 3 || input.expTypes3 == 3",
#                          tabPanel("Plot (2)",plotOutput("cat2d"))),
        tabPanel("Model Info",
                 withMathJax(),
                 helpText("The Response variable is:"),
                 verbatimTextOutput("responseVar"),
                 br(),
                 helpText("The current model is:"),
                 verbatimTextOutput("modelEQ"),
                 br(),
                 helpText("The corresponding \\(R^2-adjusted\\) is:"),
                 verbatimTextOutput("modelRsq")
        )
#         ,
#         tabPanel("Model Info (2)",
#                  withMathJax(),
#                  helpText("The Response variable is:"),
#                  verbatimTextOutput("catResp"),
#                  br(),
#                  helpText("The current model is:"),
#                  verbatimTextOutput("catModel"),
#                  br(),
#                  helpText("The corresponding \\(R^2-adjusted\\) is:"),
#                  verbatimTextOutput("catRsq")
#         )

        )

#       conditionalPanel(condition = "input.expTypes1 != 3 && !is.na(input.expTypes1)  || input.expTypes2 != 3 || input.expTypes3 != 3",
#                        tabsetPanel(
#                          tabPanel("Plot",webGLOutput("troisPlot",width="600px",height="600px")),
#                          tabPanel("Model Info",
#                                   withMathJax(),
#                                   helpText("The Response variable is:"),
#                                   verbatimTextOutput("responseVar"),
#                                   br(),
#                                   helpText("The current model is:"),
#                                   verbatimTextOutput("modelEQ"),
#                                   br(),
#                                   helpText("The corresponding \\(R^2-adjusted\\) is:"),
#                                   verbatimTextOutput("modelRsq")
#                          )
#
#                        )
#       ),
#       conditionalPanel(condition = "input.expTypes1 == 3 || input.expTypes2 == 3 || input.expTypes3 == 3",
#                        tabsetPanel(
#                          tabPanel("Plot",plotOutput("cat2d")),
#                          tabPanel("Model Info",
#                                   withMathJax(),
#                                   helpText("The Response variable is:"),
#                                   verbatimTextOutput("catResp"),
#                                   br(),
#                                   helpText("The current model is:"),
#                                   verbatimTextOutput("catModel"),
#                                   br(),
#                                   helpText("The corresponding \\(R^2-adjusted\\) is:"),
#                                   verbatimTextOutput("catRsq")
#                          )
#                        )
#
#       )


    )



  )
),
tabPanel("2D Help",
         p("When visualizing a categorical explanatory variable, we can utilize 2D plots instead. This is useful
           because it enables us to understand why the regression surfaces are seperate and gives us an expectation
           for what the regression surfaces will look like. Furthermore, 2D plot are by far, much easier to interpret."),
         sidebarLayout(
           sidebarPanel(selectInput("dataset1",label = "Select a dataset", choices = c("Iris"= "iris",
                                                                       "Cars" = "mtcars",
                                                                       "U.S." = "state.x77")),
#                         radioButtons("interact2D",label = "",
#                                      choices = c("No Interaction" = "no", "Interaction" = "yes")),
                        radioButtons("interact2D",label = "",
                                     choices = c("Simple Regression" = "simple",
                                                 "Categorical Predictor" = "no",
                                                 "Interaction" = "yes")),
                        div("Shiny app by",
                            a(href="https://www.linkedin.com/in/irvinalcaraz",target="_blank",
                              "Irvin Alcaraz"),align="right", style = "font-size: 8pt"),

                        div("Base R code by",
                            a(href="https://www.linkedin.com/in/irvinalcaraz",target="_blank",
                              "Irvin Alcaraz"),align="right", style = "font-size: 8pt"),

                        div("Shiny source files:",
                            a(href="https://gist.github.com/calpolystat/bd0400c7ce3aacfa4973",
                              target="_blank","GitHub Gist"),align="right", style = "font-size: 8pt"),

                        div(a(href="http://www.statistics.calpoly.edu/shiny",target="_blank",
                              "Cal Poly Statistics Dept Shiny Series"),align="right", style = "font-size: 8pt")

           ),
           mainPanel(
                    tabsetPanel(
                                tabPanel("Plot",plotOutput("cat2d")),
                                tabPanel("Model Info",
                                         withMathJax(),
                                         helpText("The Response variable is:"),
                                         verbatimTextOutput("catResp"),
                                         br(),
                                         helpText("The current model is:"),
                                         verbatimTextOutput("catModel"),
                                         br(),
                                         helpText("The corresponding \\(R^2-adjusted\\) is:"),
                                         verbatimTextOutput("catRsq"))
                    )
             )
         )
        )
  ))
