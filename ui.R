library(highcharter)
library(shiny)
library(shinydashboard)
library(lubridate)

shinyUI(dashboardPage(
  skin = "yellow",

  dashboardHeader(title = "6.004 Lab metrics"),

  dashboardSidebar(disable = TRUE),

  dashboardBody(
    fluidRow(
      box(title = NULL,
          width = 2,
          status = "info",
          dateRangeInput("date_range", "Date Range",
                         startview = "week")),
      box(title = NULL,
          solidHeader = F,
          status = "info",
          width = 2,
          actionButton("refresh", "Refresh", icon = icon("refresh")),
          br(),
          paste("Fake last updated:", format(now(), format = "%H:%M", tz = "America/New_York"))),
      infoBoxOutput("wait",       width = 2),
      infoBoxOutput("n_students", width = 2),
      infoBoxOutput("n_helping",  width = 2),
      infoBoxOutput("n_staff",    width = 2)
    ),

    fluidRow(
      box(title = "Various",    highchartOutput("plot_all")),
      box(title = "Staff view",      plotOutput("plot_staff_view"))
    ),
    fluidRow(
      box(title = "Wait time",              plotOutput("plot_wait_time")),
      box(title = "Staff Utilization", highchartOutput("plot_staff_utilization"))
    )
  )
))
