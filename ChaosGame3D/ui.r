# ----------------------------------------
#  App Title: Chaos Game -- 3 Dimensions
#     Author: Jimmy Doi
# ----------------------------------------

options(rgl.useNULL=TRUE)

if (!require("shiny")) install.packages("shiny")
if (!require("shinyBS")) install.packages("shinyBS")
if (!require("shinyRGL")) install.packages("shinyRGL")
if (!require("devtools")) install.packages("devtools")
if (!require("RColorBrewer")) install.packages("RColorBrewer")
if (!require("rgl")) install.packages("rgl")
if (!require("rglwidget")) install.packages("rglwidget")

library(shiny)
library(shinyBS)
library(shinyRGL)
library(rgl)
library(rglwidget)

shinyUI(fluidPage(


  h3("Chaos Game: Three Dimensions"),

  div("Note: Please adjust width of browser if only one column is visible.",br(),
  span(HTML("<a href='http://shiny.stat.calpoly.edu/ChaosGame2D' style='color:    #DC143C'
            target='_blank'>[Click here for another Shiny app on the Chaos Game]</a>")),
    style = "font-size: 9pt;color:teal"),br(),


  p("In the 3 dimensional version of the ", HTML("<a href='http://mathworld.wolfram.com/ChaosGame.html'>
                                                 Chaos Game</a>"),
    "we start with a regular polyhedron and mark selected
    points which will typically be the vertices. These points will be called",
    tags$i("endpoints"), "and will be marked with red squares. The game begins by randomly choosing a
    starting point and one of the endpoints.
    Mark a new point at a fixed distance ratio from the starting point to the endpoint (e.g.,
    halfway to the endpoint). Select another endpoint at random and,
    with the most recently created point, repeat the process to generate the next point
    and continue. By applying the right distance ratio the resulting set of points can converge
    to a beautiful image known as a", HTML("<i>fractal</i>."),"For each polyhedron the required
    distance ratio to yield a fractal will be provided,
    but try different settings to see what other patterns may arise!"
    ),
  br(),


  # Sidebar with a slider input for number of points
  sidebarPanel(
       tags$head(tags$link(rel = "icon", type = "image/x-icon", href =
   "https://webresource.its.calpoly.edu/cpwebtemplate/5.0.1/common/images_html/favicon.ico")),

  tags$title("Chaos Game -- 3 Dimensions"),
    selectizeInput('shape', h5('Shape'), choices = list(
    "Three Dimensions" = c(`Tetrahedron` = 'tetra',
                     `Cube` = 'cube',
                     `Dodecahedron` = 'dodec')
    ), selected = 'tetra'),br(),

    conditionalPanel(
      condition = "input.shape=='tetra'",
        sliderInput("dist.tetra",
                    label = h5("Distance ratio to endpoint:"),
                    min = 0.01, max = .99, value = .50, step=.01),
      div("For", tags$b("Tetrahedron"), "default value is 0.50",
          style = "font-size: 9.5pt;color:teal",align="left")
      ),
    conditionalPanel(
      condition = "input.shape=='cube'",
        sliderInput("dist.cube",
                    label = h5("Distance ratio to endpoint:"),
                    min = 0.01, max = .99, value = .67, step=.01),
      div("For", tags$b("Cube"), "default value is 0.67 (2/3)",
          style = "font-size: 9.5pt;color:teal",align="left")
      ),
    conditionalPanel(
      condition = "input.shape=='dodec'",
        sliderInput("dist.dodec",
                    label = h5("Distance ratio to endpoint:"),
                    min = 0.01, max = .99, value = .72, step=.01),
      div("For", tags$b("Dodecahedron"), "default value is 0.72",
          style = "font-size: 9.5pt;color:teal",align="left")
      ),


    br(),br(),

    sliderInput("pts",
      label = h5("Percentage of sequence:"),
      min = 10,
      max = 100,
      step = 10,
      value = 100),br(),

#    div(bsActionButton("gen", label = "Randomize"),align="right"),
    div(bsButton("gen", label = "Randomize"),align="right"),
        div("Click", tags$b("Randomize")," to re-randomize outcomes based on current settings.",
            style = "font-size: 9.5pt;color:teal",align="right"),br(),
           br(), br(), br(), br(),

        div("Shiny app by",
            a(href="http://statweb.calpoly.edu/jdoi/",target="_blank",
              "Jimmy Doi"),align="right", style = "font-size: 8pt"),

        div("Base R code by",
            a(href="http://statweb.calpoly.edu/jdoi/",target="_blank",
              "Jimmy Doi"),align="right", style = "font-size: 8pt"),

        div("Shiny source files:",
            a(href="https://gist.github.com/calpolystat/1d63ae1c5c5e3a4a5969",
              target="_blank","GitHub Gist"),align="right", style = "font-size: 8pt"),

        div(a(href="http://www.statistics.calpoly.edu/shiny",target="_blank",
              "Cal Poly Statistics Dept Shiny Series"),align="right", style = "font-size: 8pt")
        ),

  # Show the generated 3d scatterplot
  mainPanel(
    rglwidgetOutput("sctPlot"),
  div(HTML("<hr style='height: 2px; color: #BDBDBD; background-color: #D9D9D9; border: none; align:center'>"),
  p(tags$b("NOTE:"), "In the image above all points nearest a particular vertex/endpoint are shown in a common color.
Rotate the image by clicking your mouse within the plot and dragging the cursor. Also, use
    the scroll wheel of the mouse to zoom in/out of the image. Set the sequence percentage to 100%
            and slightly zoom out to improve image resolution.",align="center"),align="center"),br(),br(),
  p("For a better understanding of the chaos game, please",
     HTML("<a href='https://calpolystat.shinyapps.io/ChaosGame2D'
      target='_blank'>click here</a>"),"to access the 2 dimensional version of the app. Use the animation in the 'Initial Sequence'
           plot to see a step-by-step progression of the game."
               ,align="center")
    )
) #fluidPage
) #shinyUI