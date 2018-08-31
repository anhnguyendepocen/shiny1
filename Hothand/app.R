# Shiny app to accompany
# Ross, K.J. (2017) "Classroom Investigations of Recent 
# Research Concerning the Hot Hand Phenomenon," 
# Journal of Statistics Education, 25(3)

if (!require("shiny")) install.packages("shiny")
if (!require("shinyBS")) install.packages("shinyBS")
if (!require("plyr")) install.packages("plyr")
if (!require("ggplot2")) install.packages("ggplot2")

library(shiny)
library(shinyBS)
library(plyr)
library(ggplot2)

# function that returns basic streak statistics given a sequence
source("streak_stats.r")

# List of descriptions of streak statistics
streak_stat_options = as.list(c("Proportion of S after streaks of S" = "phat_after_Sstreak",
                                "Difference in proportion of S (after streaks of S - other trials)" = "phat_Sstreak_vs_others",
                                "Difference in proportion of S (after streaks of S - after streaks of F)" = "phat_Sstreak_vs_Fstreak",
                                "Frequency of S streaks" = "Sstreak_frequency",
                                "Longest run of S" = "longest_run_success",
                                "Total number of runs" = "n_runs"
                                # "Number of runs of S" = "n_runs_success",
                                # "Proportion of S after streaks of F" = "phat_after_Fstreak",
                                # "Number of runs of F" = "n_runs_failure",
                                # "Longest run of F" = "longest_run_failure"
))

# global constants
max_n_repetitions = 20000  # maximum number of repetitions
max_n_dots = 113  # maximum number of dots to show before histogram
max_streak_length = 7  # maximum alloable streak length
observed_color = "#ffa500"  # orange
pvalue_color = "#00bfff"   # skyblue
`%then%` <- shiny:::`%OR%` # for Shiny validate (see validate help)


ui <- fluidPage(
      tags$head(tags$link(rel = "icon", type = "image/x-icon", href =
      "https://webresource.its.calpoly.edu/cpwebtemplate/5.0.1/common/images_html/favicon.ico")),
  theme = "bootswatch-cerulean.css",
  titlePanel("Randomization-based Analysis of the Hot Hand Phenomenon"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "statistic",
                  label = "Choose a streak statistic:",
                  choices = streak_stat_options,
                  selected = "phat_after_Sstreak"
      ),
      numericInput(inputId = "streak_length",
                   label = "Define streak length:",
                   value = 1,
                   min = 1, max = max_streak_length, step = 1),
      radioButtons(inputId = "input_type",
                   label = "Select method to input observed data:",
                   choices = c("Input results sequence"= "input_data",
                               "Input summary statistics"="input_stat"),
                   selected = "input_data"),
      conditionalPanel(
        condition = "input.input_type == 'input_stat'",
        textInput(inputId = "n_trials",
                  label = "Number of trials:",
                  value = ""),
        textInput(inputId = "n_success",
                  label = "Total number of successes:", value = ""),
        textInput(inputId = "observed_stat",
                  label = "Observed value of streak statistic:",
                  value = "")
      ),
      conditionalPanel(
        condition = "input.input_type == 'input_data'",
        textInput(inputId = "observed_data",
                  label = "Enter observed results sequence",
                  value = "")
      ),
      actionButton(inputId = "run",
                   label="Accept inputs and initialize simulation"),
      tags$hr(style = "border-color: #000000;"),
      numericInput(inputId = "n_repetitions",
                   label = "Number of shuffles to simulate:",
                   value = 1,
                   min = 1, max = max_n_repetitions),
      actionButton(inputId = "more_repetitions",
                   label="Shuffle"),
      checkboxInput(inputId = "compute_pvalue",
                    label = "Compute p-value",
                    value = FALSE),
      checkboxInput(inputId = "recent_shuffle",
                    label = "Show most recent shuffle",
                    value = FALSE),
      actionButton(inputId = "reset",
                   label = "Clear plot"),
      div("Shiny app by",
          a(href="mailto:kjross@calpoly.edu",target="_blank",
            "Kevin Ross"),align="right", style = "font-size: 8pt"),
      div("Base R code by",
          a(href="mailto:kjross@calpoly.edu",target="_blank",
            "Kevin Ross"),align="right", style = "font-size: 8pt"),
      div("Shiny source files:",
          a(href="https://gist.github.com/calpolystat/d40a02fa87508ac5ac4b",
            target="_blank","GitHub Gist"), 
          align="right", style = "font-size: 8pt"),
      div(a(href="http://www.statistics.calpoly.edu/shiny",target="_blank",
            "Cal Poly Statistics Dept Shiny Series"),
          align="right", style = "font-size: 8pt")
    ),
    mainPanel(
      tabsetPanel(tabPanel(title = "Simulation app",
                           tags$b(tags$h4("Observed data",
                                          style = paste("color: ",
                                                        observed_color, ";"))),
                           tableOutput(outputId = "summary_stats"),
                           # bsAlert(anchorId = "alert_input_type"),
                           # bsAlert(anchorId = "alert_input_data"),
                           # bsAlert(anchorId = "alert_input_streak"),
                           bsAlert(anchorId = "alert_update_stat"),
                           tags$hr(style = "border-color: #000000;"),
                           bsAlert(anchorId = "alert_nrep"),
                           # bsAlert(anchorId = "alert_click_button"),
                           tags$b(tags$h4(textOutput(outputId = "plot_title"),
                                          style = "text-align: center;")),
                           textOutput(outputId = "recent_shuffle"),
                           textOutput(outputId = "recent_stat"),
                           plotOutput(outputId = "null_plot"),
                           tags$b(tags$h4(textOutput(outputId = "observed_stat"),
                                          style = paste("color: ",
                                                        observed_color, ";"))),
                           tags$b(tags$h4(textOutput(outputId = "pvalue"),
                                          style = paste("color: ",
                                                        pvalue_color, ";")))
      ),
      tabPanel(title = "Instructions and notes",
               includeHTML("instructions.html")),
      tabPanel(title = "References",
               includeHTML("references.html")),
      id = "current_tab",
      selected = "Simulation app"
      )
    )
  )
)

server <- function(input, output, session){
  
  ##### Validate inputs
  
  # choose statistic - just read input
  streak_stat_name = reactive({
    return(names(streak_stat_options)[which(streak_stat_options == input$statistic)])
  })
  
  observeEvent(c(input$statistic, input$streak_length), {
    if (input$input_type == 'input_stat' && input$observed_stat != ""){
      createAlert(session, anchorId = "alert_update_stat",
                  content = "Be sure to update the value of the observed statistic.")
    }
  })
  
  # length of streak - check integer between 1 and 7
  streak_length = reactive({
    validate(
      need(try(input$streak_length%%1 == 0),
           "Streak length must be a whole number")%then%
        need(try(input$streak_length >= 1),
             "Streak length must be at least 1")%then%
        need(try(input$streak_length <= max_streak_length),
             paste("Streak length cannot be more than ",max_streak_length))
    )
    return(input$streak_length)
  })
  
  # input data method - check that sequence is valid (just 1s and 0s)
  observed_results = reactive({
    if (input$input_type == 'input_data'){
      x = unlist(strsplit(input$observed_data, split=","))
      validate(
        need(setequal(x, c("0","1")),
             "Please enter observed results (1 for success, 0 for failure)
                  in sequence separated by commas
             (e.g. 0,1,1,1,0,0,1,0,1,1)")
      )
      return(as.numeric(x))
    }
  })
  
  # number of trials - check positive integer greater than streak length
  n_trials = reactive({
    if (input$input_type == 'input_stat'){
      validate(
        need(input$n_trials != "",
             "Please input number of trials")%then%
          need(try(as.numeric(input$n_trials) > streak_length()),
               "Number of trials must be greater than streak length")%then%
          need(try(as.numeric(input$n_trials)%%1 == 0),
               "Number of trials must be a whole number")
      )
      return(as.numeric(input$n_trials))
    }else{
      return(length(observed_results()))
    }
  })
  
  # number of successes - check positive integer less than number of trials
  n_success = reactive({
    if (input$input_type == 'input_stat'){
      validate(
        need(input$n_success != "",
             "Please input observed number of successes")%then%
          need(try(as.numeric(input$n_success) < n_trials()),
               "Number of successes must be less than number of trials")%then%
          need(try(as.numeric(input$n_success) >= streak_length()),
               "Number of successes must be greater than streak length")%then%
          need(try(as.numeric(input$n_success)%%1 == 0),
               "Number of successes must be a whole number")
      )
      return(as.numeric(input$n_success))
    }else{
      return(sum(observed_results()))
    }
  })
  
  
  #### Calculations
  
  # input data method - compute observed stats
  observed_streak_stats <- reactive({
    if (input$input_type == 'input_data'){
      return(streak_stats(observed_results(), streak_length()))
    }
  })
  
  # compute observed value of selected streak stat
  observed_stat <- reactive({
    if (input$input_type == 'input_data'){
      x = observed_streak_stats()[[input$statistic]]
    }else{
      x = as.numeric(input$observed_stat)
    }
    return(x)
  })
  
  # invalidate the simulation when any of the inputs change
  is_sim_valid <- reactiveValues(yes = TRUE)
  
  observeEvent(input$input_type, {
    is_sim_valid$yes = FALSE
    createAlert(session, anchorId = "alert_input_type",
                content = "Enter data and click Accept Inputs.")
  })
  
  observeEvent(c(input$n_trials, input$n_success, input$observed_data), {
    is_sim_valid$yes = FALSE
    createAlert(session, anchorId = "alert_input_data",
                content = "Observed data has changed.  Click Accept Inputs.")
  })
  
  observeEvent(input$streak_length, {
    is_sim_valid$yes = FALSE
    createAlert(session, anchorId = "alert_input_streak",
                content = "Streak length has changed.  Click Accept Inputs.")
  })
  
  # Simulate random permutations
  # Given # of trials and # of successes
  random_permutations = eventReactive(input$run, {
    is_sim_valid$yes = TRUE
    x = c(rep(1, n_success()), rep(0, n_trials() - n_success()))
    y = matrix(
      replicate(max_n_repetitions,
                sample(x, size = n_trials(), replace = F)),
      byrow=TRUE, nrow=max_n_repetitions)
    return(y)
  })
  
  # compute streak statistics for the random permutations
  null_dist = eventReactive(input$run, {
    withProgress(message = "Initializing simulation, please wait.",
                 detail = "When this message disappears you can use the simulate shuffles button.",
                 {
                   x = as.data.frame(
                     adply(random_permutations(), 
                           1, streak_stats, streak_length = streak_length()))
                 })
    return(x)
  })
  
  # update counter for producing the plot
  total_nrep <- reactiveValues(current = 0)
  observeEvent(input$more_repetitions, {
    if (is_sim_valid$yes == FALSE){
      createAlert(session, anchorId = "alert_click_button",
                  content = "You must click the Accept Inputs
                  button before simulating values.")
      total_nrep$current = 0
    }else{
      total_nrep$current = isolate(input$n_repetitions) + 
        isolate(total_nrep$current)
      if (total_nrep$current > max_n_repetitions){
        total_nrep$current = max_n_repetitions
        createAlert(session, anchorId = "alert_nrep",
                    content = paste("Note: ", max_n_repetitions,
                                    "is the maximum number of repetitions
                                  the app will run"))
      }
    }
  })
  
  # Reset the simulation when inputs change
  observe({
    input$run
    input$reset
    input$streak_length
    input$n_trials
    input$n_success
    input$observed_data
    total_nrep$current = 0
  })
  
  # all simulated values of the current statistic
  all_values <- reactive({
    y = null_dist()[[input$statistic]]
    return(y)
  })
  
  # values to plot; plot is updated incrementally
  plot_values <- reactive({
    if (total_nrep$current > 0){
      y = all_values()[1:total_nrep$current]
      return(y[!is.na(y)])
    }
  })
  
  # compute p-value (and numerator and denominator)
  pvalue <- reactive({
    if (input$statistic == "n_runs"){
      x = sum(plot_values() <= observed_stat())
    }else{
      x = sum(plot_values() >= observed_stat())
    }
    return(c("nrep" = length(plot_values()),
             "count" = x,
             "approx" = round(x / length(plot_values()), 4)))
  })
  
  #### Outputs
  
  # Table of summary stats
  output$summary_stats <- renderTable({
    x = data.frame(
      c("Number of trials", "Number of successes", streak_stat_name()),
      c(n_trials(), n_success(), observed_stat())
    )
    colnames(x) = c("Statistic", "Observed value")
    return(x)
  },
  include.rownames = FALSE, display = c("d","s","f") #, digits = 2
  )
  
  # Title of the plot of the null distribution (and pvalue calc)
  output$plot_title <- renderText({
    if (total_nrep$current>0){
      paste("Null distribution of streak statistic: ",
            streak_stat_name())}
  })
  
  # Most recent shuffle
  output$recent_shuffle <- renderText({
    if (input$recent_shuffle && total_nrep$current>0){
      paste("Most recent shuffle: ",
            toString(random_permutations()[total_nrep$current, ]))
    }
  })
  
  # Most recent value of streak statistic
  output$recent_stat <- renderText({
    if (input$recent_shuffle && total_nrep$current>0){
      if (!is.na(all_values()[total_nrep$current])){
        paste("Value of streak statistic for this shuffle: ",
              round(all_values()[total_nrep$current],4))
      }else{
        paste("Value of streak statistic cannot be computed for this shuffle\n
              since there are no streaks of length", streak_length())
      }
    }
  })
  
  # The plot of the null distribution and p-value
  output$null_plot <- renderPlot({
    x_axis_limits = range(c(all_values(),observed_stat()), na.rm=TRUE)*c(0.95,1.05)  ### Need c(.95, 1.05)???
    if (total_nrep$current>0){
      max_y_dots = all_values()[!is.na(all_values())]
      max_y_dots = max_y_dots[1:max_n_dots]
      if (input$statistic == "n_runs"){
        color_value = plot_values() <= observed_stat()
      }else{
        color_value = plot_values() >= observed_stat()
      }
      plot_data = data.frame(plot_value = plot_values(), color_value)
      null_plot = ggplot(data=plot_data, aes(x=plot_value, fill=color_value))
      if (pvalue()[["nrep"]] < max_n_dots){
        null_plot = null_plot +
          geom_dotplot(dotsize = .4,
                       na.rm = TRUE,
                       binwidth = (x_axis_limits[2]-x_axis_limits[1])/30) +
          scale_y_continuous(expand = c(0.01, 0.01),limits=c(0,max_n_dots/2), name="", breaks = NULL)
      }else{
        null_plot = null_plot +
          geom_histogram(colour="black",
                         binwidth = (x_axis_limits[2]-x_axis_limits[1])/50) +
          scale_y_continuous(expand = c(0.01, 0.01))
      }
      if (input$compute_pvalue && !is.na(pvalue()[["approx"]]) && !is.na(observed_stat())){
        if (input$statistic == "n_runs"){
          shade_min = -Inf
          shade_max = observed_stat()
        }else{
          shade_min = observed_stat() 
          shade_max = Inf
        }
        pvalue_fill_scale <- scale_fill_manual(
          name = "pvalue_color", 
          values = as.character(c("FALSE"="white", "TRUE"=pvalue_color)),
          limits = c("FALSE","TRUE")
        )
        null_plot = null_plot +
          pvalue_fill_scale +
          annotate("rect", xmin = shade_min, xmax = shade_max, ymin = -Inf, ymax = 0, fill=pvalue_color, color=pvalue_color) + 
          geom_vline(xintercept = observed_stat(), color = observed_color, linetype="dashed", size = 1.5)
      }else{
        null_plot = null_plot + scale_fill_manual(values=c("white","white"))
      }
      null_plot = null_plot +
        annotate("text",  x=Inf, y = Inf,
                 label = paste("Median = ", round(median(plot_values()),3),
                               "\n Mean = ", round(mean(plot_values()),3),
                               "\n SD = ", round(sd(plot_values()),3)),
                 vjust=1, hjust=1) +
        coord_cartesian(xlim=c(x_axis_limits)) + 
        ggtitle(paste("Based on ",length(plot_values())," simulated values
                      \n (resulting from  ", total_nrep$current," repetitions of the simulation)")) +
        theme(plot.title = element_text(hjust = 0.5)) +
        labs(x="Streak statistic",y="Frequency") +
        theme(legend.position = "none",
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.line.x = element_line(color="black", size = 0.5),
              axis.line.y = element_line(color="black", size = 0.5),
              plot.title = element_text(face="bold", size=14),
              axis.title = element_text(face="bold", size=14))
      print(null_plot)
    }
  })
  
  # Output value of observed statistic for plot
  output$observed_stat <- renderText({
    if (input$compute_pvalue){ 
      if (is.na(observed_stat()) & input$input_type == 'input_data'){
        paste("Warning: observed value of the streak statistic cannot be computed
              because there are no trials following a streak of length ",
              streak_length(), " in the observed data")
      }else if (is.na(observed_stat()) & input$input_type == 'input_stat'){
        paste("Please enter the observed value of the streak statistic")
      }else{ ####
        paste("Observed value of streak statistic = ", round(observed_stat(),4))
      }
    }
  })
  
  # Output the p-value calculation
  output$pvalue <- renderText({
    if (input$compute_pvalue){ 
      if (is.na(observed_stat())){
        paste("The p-value cannot be computed because the observed statistic cannot be computed")
      }else if (pvalue()[["nrep"]] == 0){
        paste("There must be at least one simulated value of the streak statistic in order to compute an approximate p-value")
      }else{
        paste("Simulated p-value = ", pvalue()[["count"]], "/",
              pvalue()[["nrep"]], " = ",
              pvalue()[["approx"]])
      }
    }
  })
}

shinyApp(ui = ui, server = server)