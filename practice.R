library(shiny)
library(ggplot2)
library(reshape)
library(scales)

source("queplot.R")

ui <- fluidPage(
  titlePanel("Plots"),
  sidebarPanel(
    h3("Constraints (Between 2012 and 2016)"),
    dateInput("date1", "Start Date", value = "2012-01-28"),
    dateInput("date2", "End Date", value = "2012-01-28"),
    h3("Parameters:"),
    checkboxInput('valtempr', 'Temperature', FALSE),
    checkboxInput('valrh', 'Humidity', FALSE),
    checkboxInput('valws', 'Wind Speed', FALSE),
    checkboxInput('valap', 'Air Pressure', FALSE),
    actionButton("submit","Submit")
  ),
  mainPanel(
    h2("Time Series Plot\n"),
    plotOutput("view"),
    verbatimTextOutput("d")
  )
)

server <- function(input, output) {
  mytable = iig_bharati
  observeEvent(input$submit, {
    output$d = renderText({
      paste(input$date1, " to ", input$date2)
    })
    output$view = renderPlot({
      #queplot(input$date1, input$date2, input$valtempr, input$valrh, input$valws, input$valap)
      queplotuser(input$date1, input$date2, input$valtempr, input$valrh, input$valws, input$valap)
    })
  })
}

shinyApp(ui, server)
deployApp()

runApp(host = "172.0.0.1", port = 7200)

#############################################################################

library(shiny)
library(ggplot2)
library(reshape)
library(scales)
library(reshape2)

source("queplot.R")

ui <- fluidPage(
  titlePanel("Plots"),
  sidebarPanel(
    h3("Constraints (Between 2005 and 2015)"),
    dateInput("date1", "Start Date", value = "2007-03-01"),
    dateInput("date2", "End Date", value = "2007-03-01"),
    h3("Parameters:"),
    checkboxInput('valtempr', 'Temperature', FALSE),
    checkboxInput('valrh', 'Humidity', FALSE),
    checkboxInput('valws', 'Wind Speed', FALSE),
    checkboxInput('valap', 'Air Pressure', FALSE),
    actionButton("submit","Submit")
  ),
  mainPanel(
    h2("Your Plot\n"),
    plotOutput("view"),
    verbatimTextOutput("d")
  )
)

#server = function(input, output) {}

server <- function(input, output) {
  observeEvent(input$submit, {
    output$d = renderText({
      paste(input$date1, " to ", input$date2)
    })
    #subse = queplot(input$date1, input$date2)
    subs = subset(mytable, as.character(as.Date(mytable$obstime, "%d/%m/%Y")) >= input$date1 & as.character(as.Date(mytable$obstime, "%d/%m/%Y")) <= input$date2)
    subs$time_only = strptime(subs$obstime, "%d/%m/%Y %H:%M")
    format(subs$time_only, "%H:%M:%S")
    output$view = renderPlot({
      qplot(subs$time_only, subs$tempr, geom = "line", xlab = "Time", ylab = "Temperature", 
            main = "Temperature vs Time")
    })
  })
}

shinyApp(ui, server)

#########################################################################

output$view = renderPlot({
  queplot(input$date1, input$date2)
})

radioButtons("choice", "Type of data?", 
             c("Daily"="day",
               "Monthly"="month",
               "yearly"="year"))
conditionalPanel(condition = "choice==day",
                 numericInput("date","Date",1))

#########################################################################

ui = fluidPage(
  titlePanel("Plots"),
  sidebarPanel(
    radioButtons("choice", "Choice", 
                 c("Fixed Intervals"="ch1", "User Defined Range"="ch2"), selected = NULL),
    conditionalPanel(
      condition = "input.choice == ch2",
      h3("Constraints (Between 2005 and 2015)"),
      dateInput("date1", "Start Date", value = "2007-03-01"),
      dateInput("date2", "End Date", value = "2007-03-01")
    ),
    conditionalPanel(
      condition = "input.choice == ch1",
      radioButtons("choice1", "Type of data?", 
                   c("Daily"="day",
                     "Monthly"="month",
                     "yearly"="year"), selected = NULL),
      conditionalPanel(condition = "input.choice1 == year",
                       numericInput("yea", "Enter Year:", 2007,2015,2007)
      ),
      conditionalPanel(condition = "input.choice1 == month",
                       numericInput("yea", "Enter Year:", 2007,2015,2007),
                       numericInput("mon", "Enter Month:", 01,12,01)
      ),
      conditionalPanel(condition = "input.choice1 == day",
                       numericInput("yea", "Enter Year:", 2007,2015,2007),
                       numericInput("mon", "Enter Month:", 01,12,01),
                       numericInput("dat", "Enter Date:", 01,31,01)
      )
    ),
    h3("Parameters:"),
    checkboxInput('valtempr', 'Temperature', FALSE),
    checkboxInput('valrh', 'Humidity', FALSE),
    checkboxInput('valws', 'Wind Speed', FALSE),
    checkboxInput('valap', 'Air Pressure', FALSE),
    actionButton("submit","Submit")
  ),
  mainPanel(
    h2("Time Series Plot\n"),
    plotOutput("view")
  )
)

server = function(input, output) {
  observeEvent(input$submit, {
    output$view = renderPlot({
      if(input$choice == ch2) {
        queplotuser(input$date1, input$date2, input$valtempr, input$valrh, input$valws, input$valap)
      }
      if(input$choice == ch1) {
          queplotfix(input$valtempr, input$valrh, input$valws, input$valap, input$choice1, input$yea, input$mon, input$dat)
      }
    })
  })
}
