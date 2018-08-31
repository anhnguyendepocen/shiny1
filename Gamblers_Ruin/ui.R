# --------------------------
#  App Title: Gambler's Ruin
#     Author: Peter Chi
# --------------------------


library(shiny)
library(shinyIncubator)

shinyUI(fluidPage(
   tags$head(tags$link(rel = "icon", type = "image/x-icon", href = 
   "https://webresource.its.calpoly.edu/cpwebtemplate/5.0.1/common/images_html/favicon.ico")),  
  progressInit(),
    titlePanel("Gambler's Ruin"),

    sidebarLayout(
       sidebarPanel(
         p("On each 'turn,' Player A wins $1 from Player B, with the probability specified 
           below. Otherwise, Player B wins $1 from Player A."),
         p("A 'game' consists of a series of turns, which continues until one player has all of the money."),
         p(),br(),br(),
         sliderInput("num",
                     label="Number of games to run",
                     min=1,max=100,value=1,step=1),
         
         sliderInput("fortuneA",
                      label="Initial fortune of Player A",
                      min=1,max=100,value=10,step=1),
         sliderInput("fortuneB",
                      label="Initial fortune of Player B",
                      min=1,max=100,value=10,step=1),
         p(),br(),
         sliderInput("probA",
                      label="Probability that Player A wins on any given turn:",
                      min=0,max=1,value=0.5,step=0.005),
         br(),
         div(actionButton("go",label="Run"),align="right"),
         p(),br(),br(),
         
         div("Shiny app by", a(href= "http://statweb.calpoly.edu/pchi/", target="_blank", "Peter Chi"),align="right", style = "font-size: 8pt"),
         div("Base R code by", a(href= "http://statweb.calpoly.edu/pchi/", target="_blank", "Peter Chi"),align="right", style = "font-size: 8pt"),
         div("Shiny source files:", a(href= "https://gist.github.com/calpolystat/96f9adc5c37f4414fbd1", target="_blank","GitHub Gist"),align="right", style = "font-size: 8pt"),
         div(a(href = "http://www.statistics.calpoly.edu/shiny", target="_blank", "Cal Poly Statistics Dept Shiny Series"),align="right", style = "font-size: 8pt")
         
      ),
      mainPanel(plotOutput("ruinPlot",width="100%"),
      verbatimTextOutput("summary"), HTML("<hr style='height: 2px; color: #de7008; background-color: #df7109; border: none;'>"),
      p("An application:"),
      p("In the casino game of blackjack, the house edge dictates that a player will win any given hand against the house
        with an approximate probability of 0.495 (assuming that the player is playing perfect `basic strategy,' and
        is not employing any card-counting techniques)."),
      p("Suppose
        that a player sits at a blackjack table and plans to bet $1 on every hand, and is willing to lose X dollars overall, but will quit if he is
        up by Y dollars from his initial fortune, at any moment. Using the inputs above, we can find a simulated probability that he will leave a winner, by putting in X as the initial fortune
        of Player A, Y as the initial fortune of Player B (the amount that the player wishes to win from the casino), and 0.495 as the probability that Player A wins
        on any given turn, and then running a large number of games (e.g. 100)."),
      p("It is rare to find a casino that will actually allow you to bet only $1 on a hand of blackjack; to consider larger bet sizes, we simply need
        to divide X and Y by the bet size before inputting the initial fortunes.")
       )
    )
))
