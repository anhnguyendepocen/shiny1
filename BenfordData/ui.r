# -------------------------------------------
#  App Title: Benford's Law and Data Examples
#     Author: Jimmy Doi
# -------------------------------------------

library(RColorBrewer)
library(rvest)
library(xml2)

shinyUI(navbarPage("Benford's Law: Data Examples",

  #open tabPanel#1
  #############################################################################
  ## tabPanel: Census Data I                                                 ##
  #############################################################################
  tabPanel("(1) Census Data I",
    fluidPage(
     tags$head(tags$link(rel = "icon", type = "image/x-icon", href = "https://webresource.its.calpoly.edu/cpwebtemplate/5.0.1/common/images_html/favicon.ico")),

    # Give the page a title
    h3("(1) Benford's Law: US Census Data I",HTML("&ndash;"),"Population Estimates"),

    div("Note: Please adjust width of browser if only one column is visible.",br(),
        HTML("<a href='http://shiny.stat.calpoly.edu/BenfordSeq' style='color:    #DC143C'
        target='_blank'>[Click here for another Shiny app on Benford's Law]</a>"),
        style = "font-size: 9pt;color:teal"),br(),

    p("The first-digit distribution of many US Census variables is known to closely follow",
      HTML("<a href='http://mathworld.wolfram.com/BenfordsLaw.html', target='_blank'>Benford's Law</a>."),
    "We will consider several census variables available from",
     HTML("<a href='https://www.census.gov/data/datasets/2016/demo/popest/counties-total.html',
                 target='_blank'>
          County Totals Dataset: Population, Population Change and Estimated
          Components of Population Change</a>."),
     "The app will apply a goodness of fit test of the observed frequencies of first-digits
     for the selected variable. The variables under consideration are: Annual Resident Total Population
     Estimate (2013 to 2016), Annual Births (2013 to 2016), Annual Deaths (2013 to 2016)."),

    p("Note: It may be the case that some variables do not sufficiently adhere
    to Benford's Law according to the goodness of fit test.
    However bear in mind that relatively small deviations from what is expected
    can lead to a small P-value for the test due to a large sample size. Nevertheless,
    it is still interesting to see that, for any census variable from this app, the observed
    proportions of first-digits are not uniformly equal to 1/9 as one might expect and that Benford's
    Law can at least serve as a rough approximation."),

    HTML("<hr style='height: 2px; color: #BDBDBD; background-color: #D9D9D9; border: none;'>"),

    # Generate a row with a sidebar
    fluidRow(
      column(4,wellPanel(

        selectInput("sel.var.cens", label = h5("Select Census Variable:"),
                    choices = list("Population Est. 2016" = 16,
                                   "Population Est. 2015" = 15,
                                   "Population Est. 2014" = 14,
                                   "Population Est. 2013" = 13,
                                   "Births, 2016" = 30,
                                   "Births, 2015" = 29,
                                   "Births, 2014" = 28,
                                   "Births, 2013" = 27,
                                   "Deaths, 2016" = 37,
                                   "Deaths, 2015" = 36,
                                   "Deaths, 2014" = 35,
                                   "Deaths, 2013" = 34
                                   ),
                    selected = 16),

        br(), br(), br(), br(),

        div("Shiny app by", a(href="http://statweb.calpoly.edu/jdoi/",target="_blank",
        "Jimmy Doi"),align="right", style = "font-size: 8pt"),

        div("Base R code by", a(href="http://statweb.calpoly.edu/jdoi/",target="_blank",
        "Jimmy Doi"),align="right", style = "font-size: 8pt"),

        div("Shiny source files:", a(href="https://gist.github.com/calpolystat/94fe941ab0d8a4f36d8b",
        target="_blank","GitHub Gist"),align="right", style = "font-size: 8pt"),

        div(a(href="http://www.statistics.calpoly.edu/shiny",target="_blank",
        "Cal Poly Statistics Dept Shiny Series"),align="right", style = "font-size: 8pt")
          ) # Close wellPanel
        ), # Close column-4

      column(8,
        div(span("Data from:",style="text-align:
            right;font-size:11pt;display:inline-block"),
        HTML("<a href='https://www.census.gov/data/datasets.html',
                 target='_blank'>
          County Totals Dataset</a>"),
        style="font-size:11pt;"),

        div(span("Total Number of Counties in Data Set: ",style="text-align:
                  right;font-size:11pt;display:inline-block"),
            span(textOutput("comp.rows.cens"),style="text-align: right;font-size:11pt;
                 display:inline-block;color:#5C8AE6;font-weight:normal")),

        div(span("Census Variable: ",style="text-align:
            right;font-size:11pt;display:inline-block"),
            span(textOutput("out.var.cens"),style="text-align: right;font-size:11pt;
            display:inline-block;color:#5C8AE6;font-weight:normal")
            ),


        HTML("<hr style='height: 2px; color: #de7008; background-color: #df7109; border: none;'>"),

        fluidRow(
          column(4,
           p(tags$b("Goodness of Fit Test:")),
           verbatimTextOutput("goodness.cens")
           ),
          column(8,
            plotOutput("cens.pmf")
            )#closes column-8
           ),#closes fluidRow
          HTML("<hr style='height: 2px; color: #de7008; background-color: #df7109; border: none;'>")
     ) #closes column-8
    ) #closes fluidRow
   ) #closes fluidPage
  ), #close tabPanel#1

  #open tabPanel#2
  #############################################################################
  ## tabPanel: Census Data II                                                ##
  #############################################################################
  tabPanel("(2) Census Data II",
    fluidPage(

    # Give the page a title
    h3("(2) Benford's Law: US Census Data II",HTML("&ndash;"),"QuickFacts"),

    div("Note: Please adjust width of browser if only one column is visible.",br(),
        HTML("<a href='https://calpolystat.shinyapps.io/BenfordSeq',
        target='_blank'>[Click here for another Shiny app on Benford's Law]</a>"),
        style = "font-size: 9pt;color:teal"),br(),

    p("The first-digit distribution of many US Census variables is known to closely follow",
      HTML("<a href='http://mathworld.wolfram.com/BenfordsLaw.html', target='_blank'>Benford's Law</a>."),
    "We will consider several census variables available from",
     HTML("<a href='http://quickfacts.census.gov/qfd/download_data.html',
                 target='_blank'>US Census State & County QuickFacts</a>."),
     "The app will apply a goodness of fit test of the observed frequencies of first-digits
     for the selected variable. The variables under consideration are:
      Housing Units (2013),
      Households (2008-12),
      Veterans (2008-12),
      Nonemployer Establishments (2012),
      *Private Nonfarm Establishments (2012),
      *Private Nonfarm Employment (2012),
      *Retail Sales (2007)."),

    p("*A small fraction (less than 2%) of the 3143 counties had entries of
        zero for the variables listed with an asterisk. For these cases, only the non-zero
        values were used for the goodness of fit test.",style="font-style:normal;font-size:9pt"),

    p("Note: It may be the case that some variables do not sufficiently adhere
    to Benford's Law according to the goodness of fit test.
    However bear in mind that relatively small deviations from what is expected
    can lead to a small P-value for the test due to a large sample size. Nevertheless,
    it is still interesting to see that, for any census variable from this app, the observed
    proportions of first-digits are not uniformly equal to 1/9 as one might expect and that Benford's
    Law can at least serve as a rough approximation."),

    HTML("<hr style='height: 2px; color: #BDBDBD; background-color: #D9D9D9; border: none;'>"),

    # Generate a row with a sidebar
    fluidRow(
      column(4,wellPanel(

        selectInput("sel.var.quick", label = h5("Select Census Variable:"),
                    choices = list("Housing Units, 2013" = 25,
                                   "Households, 2008-12" = 29,
                                   "Veterans, 2008-12" = 23,
                                   "Nonemployer Est., 2012" = 37,
                                   "*Private Nonfarm Est., 2012" = 34,
                                   "*Private Nonfarm Emp., 2012" = 35,
                                   "*Retail Sales, 2007 ($1000)" = 47),
                    selected = 25),

        br(), br(), br(), br(),

        div("Shiny app by", a(href="http://statweb.calpoly.edu/jdoi/",target="_blank",
        "Jimmy Doi"),align="right", style = "font-size: 8pt"),

        div("Base R code by", a(href="http://statweb.calpoly.edu/jdoi/",target="_blank",
        "Jimmy Doi"),align="right", style = "font-size: 8pt"),

        div("Shiny source files:", a(href="https://gist.github.com/calpolystat/94fe941ab0d8a4f36d8b",
        target="_blank","GitHub Gist"),align="right", style = "font-size: 8pt"),

        div(a(href="http://www.statistics.calpoly.edu/shiny",target="_blank",
        "Cal Poly Statistics Dept Shiny Series"),align="right", style = "font-size: 8pt")
          ) # Close wellPanel
        ), # Close column-4

      column(8,
        div(span("Data from:",style="text-align:
            right;font-size:11pt;display:inline-block"),
            HTML("<a href='http://quickfacts.census.gov/qfd/download_data.html',
                 target='_blank'>US Census State & County QuickFacts</a>"),
            style="font-size:11pt;"),

        div(span("Total Number of Counties in Data Set: ",style="text-align:
                  right;font-size:11pt;display:inline-block"),
            span(textOutput("comp.rows.quick"),style="text-align: right;font-size:11pt;
                 display:inline-block;color:#5C8AE6;font-weight:normal")),

        div(span("Census Variable: ",style="text-align:
            right;font-size:11pt;display:inline-block"),
            span(textOutput("out.var.quick"),style="text-align: right;font-size:11pt;
            display:inline-block;color:#5C8AE6;font-weight:normal")
            ),


        HTML("<hr style='height: 2px; color: #de7008; background-color: #df7109; border: none;'>"),

        fluidRow(
          column(4,
           p(tags$b("Goodness of Fit Test:")),
           verbatimTextOutput("goodness.quick")
           ),
          column(8,
            plotOutput("quick.pmf")
            )#closes column-8
           ),#closes fluidRow
          HTML("<hr style='height: 2px; color: #de7008; background-color: #df7109; border: none;'>")
     ) #closes column-8
    ) #closes fluidRow
   ) #closes fluidPage
  ), #close tabPanel#2

  #open tabPanel#3
  #############################################################################
  ## tabPanel: Stock Exchange                                                ##
  #############################################################################
  tabPanel("(3) US Stock Markets",
           fluidPage(

             # Give the page a title
             h3("(3) Benford's Law: US Stock Markets"),

             div("Note: Please adjust width of browser if only one column is visible.",
                 style = "font-size: 9pt;color:teal"),br(),

             p("After a given day of trading, if we consider the first-digit of the
               volume of shares traded for each listed company in the New York Stock Exchange,
               the corresponding distribution will closely follow",
               HTML("<a href='http://mathworld.wolfram.com/BenfordsLaw.html', target='_blank'>Benford's Law</a>."),
               "Does the first-digit distribution for other variables such as a stock's closing cost
               closely follow Benford's Law? And does the specific stock market have any influence on the
               first-digit distribution of market variables?"),

             p("This app will download information from the",
               span(HTML("<a href='http://online.wsj.com/public/resources/documents/stocksdaily.htm',
                         target='_blank'>Wall Street Journal website</a>")),
               "from the",span("most recent end of day market data.",style="font-weight:bold"),
               "The data will be based on various
               market variables for all
               companies listed in one of four stock markets.
               The app will apply a goodness of fit test of the observed frequencies of first-digits
               for the selected variable in the specified stock market."),

             p("Note: It may be the case that some stock price variables do not sufficiently adhere
               to Benford's Law according to the goodness of fit test.
               However bear in mind that relatively small deviations from what is expected
               can lead to a small P-value for the test due to a large sample size. Nevertheless,
               it is still interesting to see that, for any variable in this app, the observed
               proportions of first-digits are not uniformly equal to 1/9 as one might expect and that Benford's
               Law can at least serve as a rough approximation."),

             HTML("<hr style='height: 2px; color: #BDBDBD; background-color: #D9D9D9; border: none;'>"),

             # Generate a row with a sidebar
             fluidRow(
               column(4,wellPanel(

                 selectInput("stock", label = h5("Select Stock Market:"),
                             choices = c("New York Stock Exchange",
                                         "Nasdaq Stock Market",
                                         "Nasdaq Capital Market",
                                         "NYSE MKT Stock Exchange"),
                             selected = "New York Stock Exchange"), br(),

                 selectInput("sel.var", label = h5("Select Variable"),
                             choices = list("Volume of Shares Traded" = 9,
                                            "Opening Price" = 3,
                                            "High Price" = 4,
                                            "Low Price" = 5,
                                            "Closing Price" = 6),
                             selected = 9),

                 br(), br(), br(), br(),

                 div("Shiny app by", a(href="http://statweb.calpoly.edu/jdoi/",target="_blank",
                                       "Jimmy Doi"),align="right", style = "font-size: 8pt"),

                 div("Base R code by", a(href="http://statweb.calpoly.edu/jdoi/",target="_blank",
                                         "Jimmy Doi"),align="right", style = "font-size: 8pt"),

                 div("Shiny source files:", a(href="https://gist.github.com/calpolystat/f4475cbfe4cc77cef168",
                                              target="_blank","GitHub Gist"),align="right", style = "font-size: 8pt"),

                 div(a(href="http://www.statistics.calpoly.edu/shiny",target="_blank",
                       "Cal Poly Statistics Dept Shiny Series"),align="right", style = "font-size: 8pt")
               ) # Close wellPanel
               ), # Close column-4

               column(8,

                      div(span(imageOutput("USA.icon.stock",height=24),style="vertical-align: middle;display:inline-block"),
                          span(HTML("&nbsp;Currency:"),style="text-align:left;
                               font-size:11pt;vertical-align: middle;display:inline-block"),
                          span(HTML("&nbsp;"),style="text-align:left;font-size:11pt;vertical-align: middle;display:inline-block"),
                          span(textOutput("USA.curr.stock"),style="text-align:left;font-size:11pt;color:#5C8AE6;
                               display:inline-block;vertical-align: middle"),
                          span(HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"),style="text-align: right;vertical-align: middle;font-size:11pt"),
                      span("End of Day Market Data from: ",style="text-align:
                      right;font-size:11pt;vertical-align: middle;display:inline-block"),
                          span(HTML("&nbsp;"),style="text-align: right;vertical-align: middle;font-size:11pt"),
                          span(textOutput("comp.date"),style="text-align: right;vertical-align: middle;font-size:11pt;
                      display:inline-block;color:#5C8AE6;font-weight:normal"),
                          style="text-align: right;font-size:0pt;display:inline-block;"),br(),

                      div(span("Market Source: ",style="text-align:
                      right;font-size:11pt;display:inline-block"),
                          span(textOutput("short.stock"),style="text-align: right;font-size:11pt;
                     display:inline-block;color:#5C8AE6;font-weight:normal"),
                          span(HTML("&nbsp;&nbsp;&nbsp;&nbsp;"),style="text-align: right;font-size:11pt"),
                          span("Total Number of Stocks in Data Set: ",style="text-align:
                      right;font-size:11pt;display:inline-block"),
                          span(textOutput("comp.rows"),style="text-align: right;font-size:11pt;
                     display:inline-block;color:#5C8AE6;font-weight:normal")),

                      HTML("<hr style='height: 2px; color: #de7008; background-color: #df7109; border: none;'>"),

                      fluidRow(
                        column(4,
                               p(tags$b("Goodness of Fit Test:")),
                               div(span(textOutput("out.var"),style="text-align: right;font-size:11pt;
                      display:inline-block;color:#5C8AE6;font-weight:normal;text-align: center")),
                               br(),
                               verbatimTextOutput("goodness")
                        ),
                        column(8,
                               plotOutput("stock.pmf")
                        )#closes column-8
                      ),#closes fluidRow
                      HTML("<hr style='height: 2px; color: #de7008; background-color: #df7109; border: none;'>")
               ) #closes column-8
             ) #closes fluidRow
             ) #closes fluidPage
               ), #close tabPanel#3

  #open tabPanel#4
  #############################################################################
  ## tabPanel: World Stock Exchange                                          ##
  #############################################################################
  tabPanel("(4) World Stock Markets",
           fluidPage(

             # Give the page a title
             h3("(4) Benford's Law: World Stock Markets"),

             div("Note: Please adjust width of browser if only one column is visible.",br(),
                 HTML("<a href='https://calpolystat.shinyapps.io/BenfordSeq',
                      target='_blank'>[Click here for another Shiny app on Benford's Law]</a>"),
                 style = "font-size: 9pt;color:teal"),br(),

             p("For the New York Stock Exchange (see previous tab),
              we found that the first-digit distribution of stock prices
              closely follows",
               HTML("<a href='http://mathworld.wolfram.com/BenfordsLaw.html', target='_blank'>Benford's Law</a>."),
               "Does Benford's Law also apply to the first-digit distribution for stock prices from other world markets?
               Does currency have any influence on the
               first-digit distribution of market variables?"),

             p("This app will download market data from the",
               span(HTML("<a href='https://www.investing.com/indices',
                         target='_blank'>Investing.com website</a>. ")),
               "For the selected stock market, if trading is ", span("active ",style="font-weight:bold"),
               "at the point of data access,
                the results will be based on the ", span("most current market data.",style="font-weight:bold"),
               "If the market is closed at point of access, then all information will be based on the most recent
                end of day market data. The app will apply a goodness of fit test of the observed frequencies of first-digits
               for the selected variable in the specified stock market."),

             p("Note: It may be the case that some stock price variables do not sufficiently adhere
               to Benford's Law according to the goodness of fit test.
               However bear in mind that relatively small deviations from what is expected
               can lead to a small P-value for the test due to a large sample size. Nevertheless,
               it is still interesting to see that, for any variable in this app, the observed
               proportions of first-digits are not uniformly equal to 1/9 as one might expect and that Benford's
               Law can at least serve as a rough approximation."),

             HTML("<hr style='height: 2px; color: #BDBDBD; background-color: #D9D9D9; border: none;'>"),

             # Generate a row with a sidebar
             fluidRow(
               column(4,wellPanel(

                 selectInput("w.stock", label = h5("Select World Market:"),
                             choices = c("Australia",
                                         "Canada",
                                         "China",
                                         "Denmark",
                                         "England",
                                         "France",
                                         "Germany",
                                         "India",
                                         "Japan",
                                         "Korea",
                                         "Netherlands",
                                         "Poland",
                                         "Sri Lanka",
                                         "Switzerland",
                                         "Turkey"),
                             selected = "Australia"), br(),

                 selectInput("w.sel.var", label = h5("Select Variable:"),
                             choices = list("Most Recent Price" = 3,
                                            "High Price" = 4,
                                            "Low Price" = 5),
                             selected = 3),

                 br(), br(), br(), br(),

                 div("Shiny app by", a(href="http://statweb.calpoly.edu/jdoi/",target="_blank",
                                       "Jimmy Doi"),align="right", style = "font-size: 8pt"),

                 div("Base R code by", a(href="http://statweb.calpoly.edu/jdoi/",target="_blank",
                                         "Jimmy Doi"),align="right", style = "font-size: 8pt"),

                 div("Shiny source files:", a(href="https://gist.github.com/calpolystat/94fe941ab0d8a4f36d8b",
                                              target="_blank","GitHub Gist"),align="right", style = "font-size: 8pt"),

                 div(a(href="http://www.statistics.calpoly.edu/shiny",target="_blank",
                       "Cal Poly Statistics Dept Shiny Series"),align="right", style = "font-size: 8pt")
               ) # Close wellPanel
               ), # Close column-4

               column(8,

                      div(span(imageOutput("w.icon.stock",height=24),style="vertical-align: middle;display:inline-block"),
                          span(HTML("&nbsp;Currency:"),style="text-align:left;
                               font-size:11pt;vertical-align: middle;display:inline-block"),
                          span(HTML("&nbsp;"),style="text-align:left;font-size:11pt;vertical-align: middle;display:inline-block"),
                          span(textOutput("w.curr.stock"),style="text-align:left;font-size:11pt;color:#5C8AE6;
                               display:inline-block;vertical-align: middle"),
                          span(HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"),
                               style="text-align: left;font-size:11pt;vertical-align: middle"),
                          span("Access Point: ",style="text-align:
                               left;font-size:11pt;display:inline-block;vertical-align: middle"),
                          span(HTML("&nbsp;"),style="text-align: left;font-size:11pt"),
                          span(textOutput("w.time.stock"),style="text-align: left;font-size:11pt;
                               display:inline-block;color:#5C8AE6;font-weight:normal;vertical-align: middle"),
                          style="text-align: right;font-size:0pt;display:inline-block;"),br(),

                      div(span("Market Status:",style="text-align:
                               left;font-size:11pt;display:inline-block;vertical-align: middle"),
                          span(textOutput("w.mkt.status"),style="text-align: left;font-size:11pt;
                               display:inline-block;color:red;font-weight:normal;vertical-align: middle"),
                          span(HTML("&nbsp;&nbsp;&nbsp;&nbsp;"),style="text-align: left;font-size:11pt;vertical-align: middle"),
                          span("Source: ",style="text-align:
                               left;font-size:11pt;display:inline-block;vertical-align: middle"),
                          span(textOutput("w.short.stock"),style="text-align: left;font-size:11pt;
                               display:inline-block;color:#5C8AE6;font-weight:normal;vertical-align: middle"),
                          span(HTML("&nbsp;&nbsp;&nbsp;&nbsp;"),style="text-align: left;font-size:11pt;vertical-align: middle"),
                          span("Total Stocks: ",style="text-align:
                               left;font-size:11pt;display:inline-block;vertical-align: middle"),
                          span(textOutput("w.comp.rows"),style="text-align: left;font-size:11pt;
                          display:inline-block;color:#5C8AE6;font-weight:normal;vertical-align: middle")
                          ),

                      HTML("<hr style='height: 2px; color: #de7008; background-color: #df7109; border: none;'>"),

                      fluidRow(
                        column(4,
                               p(tags$b("Goodness of Fit Test:")),
                               div(span(textOutput("w.out.var"),style="text-align: right;font-size:11pt;
                                        display:inline-block;color:#5C8AE6;font-weight:normal;text-align: center")),
                               br(),
                               verbatimTextOutput("w.goodness")
                               ),
                        column(8,
                               plotOutput("w.stock.pmf")
                        )#closes column-8
                      ),#closes fluidRow
                      HTML("<hr style='height: 2px; color: #de7008; background-color: #df7109; border: none;'>")
                          ) #closes column-8
                      ) #closes fluidRow
             ) #closes fluidPage
               ) #close tabPanel#4

  ) #closes navbarPage
) #closes shinyUI
