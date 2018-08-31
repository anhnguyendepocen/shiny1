# ----------------------------------------
#  App Title: Chaos Game -- 2 Dimensions
#     Author: Jimmy Doi
# ----------------------------------------

if (!require("shiny")) install.packages("shiny")
if (!require("shinyBS")) install.packages("shinyBS")

library(shiny)
library(shinyBS)

shinyUI(fluidPage(

   tags$head(tags$link(rel = "icon", type = "image/x-icon", href =
   "https://webresource.its.calpoly.edu/cpwebtemplate/5.0.1/common/images_html/favicon.ico")),

   tags$head(
     tags$style(HTML("hr {border-top: 1px solid #000000;}"))
   ),
   
     tags$title("Chaos Game -- 2 Dimensions"),
  h3("Chaos Game: Two Dimensions"),

  div("Note: Please adjust width of browser if only one column is visible.",br(),
  span(HTML("<a href='http://shiny.stat.calpoly.edu/ChaosGame3D' style='color:    #DC143C'
            target='_blank'>[Click here for another Shiny app on the Chaos Game]</a>")),
    style = "font-size: 9pt;color:teal"),br(),


  p("In the two dimensional version of the ", HTML("<a href='http://mathworld.wolfram.com/ChaosGame.html'>Chaos Game</a>"),
    "we start with a regular polygon and mark selected
    points which will typically be the vertices. These points will be called",
    tags$i("endpoints"), "and will be marked in red. The game begins by randomly choosing a
    starting point and one of the endpoints.
    Mark a new point at a fixed distance ratio from the starting point to the endpoint (e.g.,
    halfway to the endpoint). Select another endpoint at random and,
    with the most recently created point, repeat the process to generate the next point
    and continue. By applying the right distance ratio the resulting set of points can converge
    to a beautiful image known as a", HTML("<i>fractal</i>"),". For each polygon the required
    distance ratio to yield a fractal will be provided,
    but try different settings to see what other patterns may arise!"
    ),
  br(),

  fluidRow(
    column(4,
    wellPanel(

    selectizeInput('shape', h5(tags$b('Shape')), choices = list(
    "Two Dimensions" = c(`Triangle` = 'tri',
                     `Square` = 'sqr',
                     `Pentagon` = 'pent',
                     'Hexagon' = 'hex')
    ), selected = 'tri'),
    #hr(),

    conditionalPanel(
      condition = "input.shape=='tri'",
        sliderInput("dist.tri",
                    label = h5(tags$b("Distance ratio to endpoint:")),
                    min = 0.01, max = .99, value = .50, step=.01),
      div("For", tags$b("Triangle"), "default value is 0.50",
          style = "font-size: 9.5pt;color:teal",align="left")
      ),

    conditionalPanel(
      condition = "input.shape=='sqr'",
        sliderInput("dist.sqr",
                    label = h5(tags$b("Distance ratio to endpoint:")),
                    min = 0.01, max = .99, value = .67, step=.01),
      div("For", tags$b("Square"), "default value is 0.67 (2/3)",
          style = "font-size: 9.5pt;color:teal",align="left")
      ),

    conditionalPanel(
      condition = "input.shape=='pent'",
        sliderInput("dist.pent",
                    label = h5(tags$b("Distance ratio to endpoint:")),
                    min = 0.01, max = .99, value = .63, step=.01),
      div("For", tags$b("Pentagon"), "default value is 0.63 (5/8)",
          style = "font-size: 9.5pt;color:teal",align="left")
      ),

    conditionalPanel(
      condition = "input.shape=='hex'",
        sliderInput("dist.hex",
                    label = h5(tags$b("Distance ratio to endpoint:")),
                    min = 0.01, max = .99, value = .67, step=.01),
      div("For", tags$b("Hexagon"), "default value is 0.67 (2/3)",
          style = "font-size: 9.5pt;color:teal",align="left")
      ),

    conditionalPanel(
      condition = "input.shape=='tetra'",
        sliderInput("dist.tetra",
                    label = h5(tags$b("Distance ratio to endpoint:")),
                    min = 0.01, max = .99, value = .5, step=.01),
      div("For", tags$b("Tetrahedron"), "default value is 0.50",
          style = "font-size: 9.5pt;color:teal",align="left")
      ),

    conditionalPanel(
      condition = "input.shape=='cube'",
        sliderInput("dist.cube",
                    label = h5(tags$b("Distance ratio to endpoint:")),
                    min = 0.01, max = .99, value = .33, step=.01),
      div("For", tags$b("Cube"), "default value is 0.33",
          style = "font-size: 9.5pt;color:teal",align="left")
      ),

    conditionalPanel(
      condition = "input.shape=='dodec'",
        sliderInput("dist.dodec",
                    label = h5(tags$b("Distance ratio to endpoint:")),
                    min = 0.01, max = .99, value = .38, step=.01),
      div("For", tags$b("Dodecahedron"), "default value is 0.38",
          style = "font-size: 9.5pt;color:teal",align="left")
      ) # NO COMMA AFTER RIGHT PARENT OF LAST CONDITIONALPANEL

    ,
    #hr(),
    br(),
    
    conditionalPanel(condition="input.tabselected==1",
                            div(uiOutput("my.init")),
                            div("Press the",
                                span(HTML("&#9654"),style=
                                       "font-size:10pt;color:#999999;"), "button above to animate.",br(),
                                span("Advance slider manually at anytime."),br(),
                                span("Click",tags$b("Randomize"),"to re-randomize at current n."),
                                align="center",style="font-size:8.5pt;color:teal")
    ),
    
    conditionalPanel(condition="input.tabselected==2",
                     div(uiOutput("my.extend")),
                     div("Press the",
                         span(HTML("&#9654"),style=
                                "font-size:10pt;color:#999999;"), "button above to animate.",br(),
                         span("Advance slider manually at anytime."),br(),
                         span("Click",tags$b("Randomize"),"to re-randomize at current n."),
                         align="center",style="font-size:8.5pt;color:teal")
    ),

    conditionalPanel(condition="input.tabselected==3",
                     div(uiOutput("my.pts")),
                     div("Press the",
                         span(HTML("&#9654"),style=
                                "font-size:10pt;color:#999999;"), "button above to animate.",br(),
                         span("Advance slider manually at anytime."),br(),
                         span("Click",tags$b("Randomize"),"to re-randomize at current n."),
                         align="center",style="font-size:8.5pt;color:teal")
    ),
    
    #hr(),
    br(),
    
#         div(bsActionButton("gen", label="Randomize"),align="right"),
        div(bsButton("gen", label="Randomize"),align="right"),
        div("Click", tags$b("Randomize")," to re-randomize outcomes based on current settings.",
            style = "font-size: 9.5pt;color:teal",align="right"),

        br(),

        div("Shiny app by",
            a(href="http://statweb.calpoly.edu/jdoi/",target="_blank",
              "Jimmy Doi"),align="right", style = "font-size: 8pt"),

        div("Base R code by",
            a(href="http://statweb.calpoly.edu/jdoi/",target="_blank",
              "Jimmy Doi"),align="right", style = "font-size: 8pt"),

        div("Shiny source files:",
            a(href="https://gist.github.com/calpolystat/d40a02fa87508ac5ac4b",
              target="_blank","GitHub Gist"),align="right", style = "font-size: 8pt"),

        div(a(href="http://www.statistics.calpoly.edu/shiny",target="_blank",
              "Cal Poly Statistics Dept Shiny Series"),align="right", style = "font-size: 8pt")

    ) #sidebarPanel
    ), #column-4

    # Show a tabset that includes a plot, summary, and table view
    # of the generated distribution
    column(8,
      tabsetPanel(type = "tabs",id = "tabselected",
##############
# tabPanel 1 #
##############
        tabPanel("Initial Sequence",value=1,
          fluidRow(
            column(12,
                   div(
                   div(
                 plotOutput("initPlot"),style="width:500px",inline="TRUE"),align="center"),
             HTML("<hr style='height: 2px; color: #BDBDBD; background-color: #D9D9D9; border: none;'>")
            ), # column-12
            fluidRow(
              column(10, offset=1,
              p("
                This plot shows a step-by-step progression of the chaos game.
                At each step the randomly chosen red endpoint will be marked.
                Use the 'Number of points' slider and press the",
                span(HTML("&#9654"),style=
                         "font-size:10pt;color:#999999;"),"button to animate the plot.
                Advance the slider to n=100 before moving to the 'Extended Sequence' tab.",
                style="color:#0066CC"
                )
              ) # column-10
            ) # fluidRow
        ) # fluidRow
      ), # tabPanel

##############
# tabPanel 2 #
##############
        tabPanel("Extended Sequence",value=2,
          fluidRow(
            column(12,
                   div(
                   div(
                 plotOutput("extendPlot"),style="width:500px",inline="TRUE"),align="center"),
              HTML("<hr style='height: 2px; color: #BDBDBD; background-color: #D9D9D9; border: none;'>")
            ), # column-12
            fluidRow(
              column(10, offset=1,
                     p("
                Compare the plot above when n=100 to the plot when n=100 from the
                'Initial Sequence' tab",HTML("&ndash;"), "they should be identical. This shows the plot
                given above is simply a continuation of the chaos game.
                Advance the 'Number of points' slider to n=1000 before moving to the 'Complete Sequence' tab.",
                style="color:#0066CC"
                )
              ) # column-10
            ) # fluidRow
        ) # fluidRow
      ), # tabPanel

##############
# tabPanel 3 #
##############
        tabPanel("Complete Sequence",value=3,
          fluidRow(
            column(12,
                   div(
                   div(
                 plotOutput("compPlot"),style="width:500px",inline="TRUE"),align="center"),
             HTML("<hr style='height: 2px; color: #BDBDBD; background-color: #D9D9D9; border: none;'>")

                 ), # column-12
              fluidRow(
                column(10, offset=1,
                       p("
                Compare the plot above when n=1000 to the plot when n=1000 from the
                'Extended Sequence' tab",HTML("&ndash;"), "they should be identical.
                This again shows the plot
                given above is a continuation of the chaos game. Smaller plotting points are used
                to reveal the finer details of the completed plot.",
                style="color:#0066CC"
                )
                ) # column-10
              ) #fluidRow
          ) #fluidRow
        )  # close tabPanel-Complete
      )# tabsetPanel
    )# column-8
  ) # fluidRow
 )# fluidPage
)# shinyUI
