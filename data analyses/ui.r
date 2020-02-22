library(shiny)
library(shinydashboard)

#ui = fluidPage(
#  titlePanel(title = "FYP"),
#  sidebarLayout(position = "right",
#                sidebarPanel(h3("sidebar panel")),
#                mainPanel(h4("main panel text")))
#)


ui = shinyUI(fluidPage(
  dashboardPage(
    dashboardHeader(title = "Final Year Project - Victory Obot"),
    dashboardSidebar(
      menuItem("Dashboard"),
      menuItem("Detailed Analysis"),
      menuSubItem("3rd Mainland Bridge"),
      menuSubItem("Ikorodu Road"),
      menuItem("Raw Data")
    ),
    dashboardBody()
  )
)
)
