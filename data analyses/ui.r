library(shiny)
library(shinydashboard)
library(DT)


ui = shinyUI(fluidPage(
  dashboardPage(
    dashboardHeader(title = "LOS Prediction"),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Project Overview", tabName = "overview", icon = icon("book", lib = "font-awesome")),
        menuItem("Test The Model", tabName = "testModel", icon = icon("vial", lib = "font-awesome")),
        menuItem("Make A Specific Prediction", tabName = "makePrediction", icon = icon("vial", lib = "font-awesome")),
        menuItem("Raw Data", tabName = "rawData", icon = icon("database", lib = "font-awesome"))
      )
    ),
    
    dashboardBody(
      tabItems(
      tabItem(tabName = "overview",
              h1("Project Overview"),
              fluidRow(box(
                plotOutput("histogram")
              ))),
      tabItem(tabName = "testModel",
              h1("Test The Model"),
              sliderInput("ratio", "Training set ratio", 0.10, 0.95, 0.75),
              h4("The higher the ratio, the longer it takes to train..."),
              actionButton("submit", "Submit"),
              
              fluidRow(box(
                h3("Overall Accuracy: ", h4(textOutput("accuracy")))
              ),box(
                h3("Precision: ", h4(tableOutput("precision"))),
                h3("Recall: ", h4(tableOutput("recall"))),
                h3("Specificity: ", h4(tableOutput("specificity")))
              ),
                plotOutput("plot")
              )
      ),
      tabItem(tabName = "makePrediction",
              fluidRow(
                box(
                  selectInput(
                    "ï..Description",
                    "Direction of Traffic",
                    list("Northbound", "Southbound"),
                    selected = NULL,
                    multiple = FALSE,
                    selectize = TRUE,
                    width = NULL,
                    size = NULL
                  ),
                  sliderInput("LengthofDetectionkm", "Length of Detection (km)", 3.0, 4.0, 3.5),
                  selectInput(
                    "Month",
                    "Month",
                    list(
                      "June",
                      "July",
                      "August",
                      "September",
                      "October",
                      "November",
                      "December",
                      "January"
                    ),
                    selected = NULL,
                    multiple = FALSE,
                    selectize = TRUE,
                    width = NULL,
                    size = NULL
                  ),
                  selectInput(
                    "DayofWeek",
                    "Day of Week",
                    list(
                      "Sunday",
                      "Monday",
                      "Tuesday",
                      "Wednesday",
                      "Thursday",
                      "Friday",
                      "Saturday"
                    ),
                    selected = NULL,
                    multiple = FALSE,
                    selectize = TRUE,
                    width = NULL,
                    size = NULL
                  ),
                  sliderInput("Hour", "Hour", 0, 23, 0),
                  #Calculate TimeofDay with hour value
                  sliderInput("DetectedDevices", "Number of Vehicles", 0, 1500, 50),
                  sliderInput("Speedkmh", "Average Speed of Vehicles", 10.0, 120.0, 10.0),
                  #Calculate Travel time with speed and distance values.
                  sliderInput("Temperature", "Temperature", 20, 40, 30),
                  selectInput(
                    "Weather",
                    "Weather Condition",
                    list(
                      "clear sky",
                      "few clouds",
                      "scattered clouds",
                      "broken clouds",
                      "overcast clouds",
                      "light intensity drizzle",
                      "drizzle",
                      "light rain",
                      "moderate rain",
                      "heavy intensity rain",
                      "light thunderstorm",
                      "thunderstorm",
                      "thunderstorm with light rain",
                      "thunderstorm with rain",
                      "thunderstorm with heavy rain",
                      
                      "mist",
                      "haze",
                      "fog",
                      "smoke"
                    ),
                    selected = NULL,
                    multiple = FALSE,
                    selectize = TRUE,
                    width =
                      NULL,
                    size = NULL
                  ),
                  
                  actionButton("submit2", "submit")
                ),
                box(
                  h3("The level of service is:"),
                  h3(textOutput("prediction")))
              )),
      tabItem(tabName = "rawData",
              h1("Raw Traffic Data of 3rd Mainland Bridge from June '19 to Jan '20"),
              fluidRow(
                column(
                  DT::dataTableOutput("trafficData"), width = 6)
                ))
      
      )
      
    )
  )
)
)