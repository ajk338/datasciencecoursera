install.packages("shiny")
library(shiny)
shinyUI(pageWithSidebar(
    
  headerPanel("Google Analytics"),
  
  sidebarPanel(
    
      dateRangeInput(inputID = "dateRange",
                     label = "Date range",
                     start = "2013-04-01",
                     max = Sys.Date()
                    
      ),
      sliderInput(inputID = "minimumTime",
                  label = "Hours of interest- minimum",
                  min = 0,
                  max = 23,
                  value = 0,
                  step = 1),
      
      sliderInput(inputID = "maximumTime",
                  label = "Hours of interest - maximum",
                  min = 0,
                  max = 23,
                  value = 23,
                  step = 1),
      
      checkboxInput(inputID = "smoother",
                    label = "Add smoother?",
                    value = FALSE),
      
      checkboxGroupInput(inputID = "domainShow",
                         label = "Show NHS and other domain (defaults to all)?",
                         choices = list("NHS users" = "NHS",
                                        "Other" = "Other")
                         ),
      
      radioButtons(inputID = "outputType",
                   label = "Output required",
                   choices = list("Visitors" = "visitors",
                                  "Bounce rate" = "bounceRate",
                                  "Time on site" = "timeOnSite"))
      
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Summary", textOutput("textDisplay")),
          tabPanel("Monthly figures", plotOutput("monthGraph")),
          tabPanel("Hourly figures", plotOutput("hourGraph"))
          )
        )
  ))
