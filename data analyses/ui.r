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
    dashboardHeader(title = "Level of Service Prediction"),
    dashboardSidebar(
      
      sliderInput("bins","Number of Breaks",1,100,50),
      selectInput("ï..Description", "Direction of Traffic", list("Northbound", "Southbound"), 
                  selected = NULL, multiple = FALSE,
                  selectize = TRUE, width = NULL, size = NULL),
      sliderInput("LengthofDetectionkm","Length of Detection (km)",3.0,4.0,3.5),
      sliderInput("Month","Month",1,12,1),
      selectInput("DayofWeek", "Day of Week", list("Sunday", "Monday", "Tuesday", "Wednesday",
                                                   "Thursday", "Friday", "Saturday"), 
                  selected = NULL, multiple = FALSE,
                  selectize = TRUE, width = NULL, size = NULL),
      sliderInput("Hour","Hour",0,23,0),
      #Calculate TimeofDay with hour value
      sliderInput("DetectedDevices","Number of Vehicles",0,1500,50),
      sliderInput("Speedkmh","Average Speed of Vehicles",10.0,120.0,10.0),
      #Calculate Travel time with speed and distance values.
      sliderInput("Temperature","Temperature",20,40,30),
      selectInput("Weather", "Weather Condition", list("clear sky", "few clouds",
                                                       "scattered clouds", "broken clouds", 
                                                       "overcast clouds", "light intensity drizzle", 
                                                       "drizzle", "light rain", "moderate rain",
                                                       "heavy intensity rain", "light thunderstorm",
                                                       "thunderstorm", "thunderstorm with light rain",
                                                       "thunderstorm with rain", "thunderstorm with heavy rain",
                                                       "mist", "haze", "fog", "smoke"), 
                  selected = NULL, multiple = FALSE,
                  selectize = TRUE, width = NULL, size = NULL)
      
      #actionButton("submit", "submit")
    ),
    dashboardBody(
      fluidRow(
        box(plotOutput("histogram")),
        box(tableOutput("prediction"))

      )
    )
  )
)
)
