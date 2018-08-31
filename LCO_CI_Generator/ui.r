# ----------------------------
#  App Title: LCO CI Generator
#     Author: Jimmy Doi
# ----------------------------

library(shiny)

shinyUI(fluidPage(
    tags$head(tags$link(rel = "icon", type = "image/x-icon", href =
    "https://webresource.its.calpoly.edu/cpwebtemplate/5.0.1/common/images_html/favicon.ico")),

  tags$title("LCO Confidence Interval Generator"),
  titlePanel("LCO Confidence Interval Generator"),

  div("Note: Please adjust width of browser if only one column is visible.",
    style = "font-size: 9pt;color:teal"),br(),br(),

  sidebarPanel(

      sliderInput("level",
                label = h5("Confidence Level %:"),
                min = 80, max = 99, value = 95, step=1),br(),

      sliderInput("size",
                  label = h5("Sample Size:"),
                  min = 1, max = 200, value = 25),br(),

      selectInput("dpsize",
                  label = h5("Decimal Place Accuracy:"),
                  choices = c("2nd", "3rd", "4th"),
                  selected = "2nd"),

      div("At 4th decimal place accuracy computation time may require up to
          25 seconds.", style = "font-size: 9.5pt;color:teal",align="right"),
      br(), br(),

      div(submitButton("Submit"),align="right"), br(), br(), br(), br(), br(),

      div("Shiny app by",
          a(href="http://statweb.calpoly.edu/jdoi/",target="_blank",
            "Jimmy Doi"),align="right", style = "font-size: 8pt"),

      div("Base R code by",
          a(href="http://statweb.calpoly.edu/jdoi/",target="_blank",
            "Jimmy Doi"),align="right", style = "font-size: 8pt"),

      div("Shiny source files:",
          a(href="https://gist.github.com/calpolystat/d896c5848934484181be",
            target="_blank","GitHub Gist"),align="right", style = "font-size: 8pt"),

      div(a(href="http://www.statistics.calpoly.edu/shiny",target="_blank",
            "Cal Poly Statistics Dept Shiny Series"),align="right", style = "font-size: 8pt")
    ),

  mainPanel(
    p("Details on the Length/Coverage Optimal (LCO) confidence interval for
       the binomial proportion can be found in the following journal
       article:"),

    #     tags$blockquote("Schilling, M., and Doi, J. (2014) 'A Coverage Probability
    #        Approach to Finding an Optimal Binomial Confidence Procedure'",
    #        em("The American Statistician"),", 68, 133-145. ",
    #        a(href="http://amstat.tandfonline.com/doi/abs/10.1080/00031305.2014.899274#.UyNr7oWuomg",
    #        target="_blank", "(Online access)")),

    div("Schilling, M., and Doi, J. (2014) 'A Coverage Probability Approach to Finding an Optimal
        Binomial Confidence Procedure'",
        em("The American Statistician"),", 68(3), 133--145",
        a(href="http://amstat.tandfonline.com/doi/abs/10.1080/00031305.2014.899274#.UyNr7oWuomg",
          target="_blank", "(Online access)"),style="padding-left: 20px;
        display:block; border-left: 5px solid #faebbc;margin-left:0px"),br(),

    p("The ", a(href="http://www.calpoly.edu/~jdoi/LCO/", target="_blank", "LCO website"),
      "contains the R code for the CI algorithm and a full listing of LCO CIs
      (at 5 decimal places), for", em("n"), " = 1, 2, ..., 100 at the
      90, 95, and 99% levels."),

    HTML("<hr style='height: 2px; color: #de7008; background-color: #df7109; border: none;'>"),

    textOutput("textlevel"),
    textOutput("textsize"),
    textOutput("textdp"),
    br(),
    verbatimTextOutput("LCOresults")
    )
  )
)
