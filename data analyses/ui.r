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
                h3("Among the 36 states that make up Nigeria, Lagos is the smallest in terms of land mass. Despite that, it I the second most populous state in Nigeria with a population of 12.5 million as of 2016. 
                Due to this very dense population, Lagos is plagued by an absurd level of traffic gridlock. 
                   A Lagosian that works 40 hour a week spends an average of 30 hours commuting to and from work each week."),
                h3("The need for an improvement to the traffic management system is evident. 
                   For the Lagos State government to make better informed decisions regarding improvements, this project proposes the use of artificial intelligence that uses numerous traffic characteristics to predict congestion levels. 
                   Using this will enables the planners to have consistency in the reasoning behind decisions made as the predictions will be accurate, systematic and can also provide the most important factors leading to the prediction."),
                h3("This project therefore set out to build a model that predicts the occurrence and level of traffic congestion using various traffic variables."),
                h3("Furthermore, this web app was then created to allow users interact with the model and the original dataset."),                
                
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