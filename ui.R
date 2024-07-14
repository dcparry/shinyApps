# DATA423 assignment 1
# Dianne Parry
# ui file

library(shiny)
source("global.R")
library(DT)
library(shinyWidgets)

fluidPage(
  titlePanel("Assignment 1 - Dianne Parry"),
  
  sidebarLayout(
    sidebarPanel(
      width = 2,
      
      conditionalPanel(
        condition = "input.tabs == 3",
        
        checkboxGroupInput("columns",
                           "Select data type:",
                           choices = names(missing_groups),
                           selected = names(missing_groups))
      ),

      
      conditionalPanel(
        condition = "input.tabs == 6",
        
        checkboxGroupInput("variableGroups", 
                           label = "Select variables:", 
                           choices = names(numeric_groups), 
                           selected = names(numeric_groups)),
        
        checkboxGroupInput("transformSelect2",
                           label = "",
                           choices = c("Centre", "Scale"))
      ),
      
      
      conditionalPanel(
        condition = "input.tabs == 5",
        
        sliderInput("range", 
                    label = "IQR Multiplier", 
                    min = 0, 
                    max = 5, 
                    step = 0.1, 
                    value = 1.5),
        
        checkboxGroupInput("transformSelect",
                           label = "",
                           choices = c("Centre", "Scale"),
                           selected = FALSE),
        
        checkboxInput("outliers", 
                      label = "Show outliers", 
                      value = TRUE)
      ),
      
      
      conditionalPanel(
        condition = "input.tabs == 12",
        
        selectInput("categorySelect", 
                    label = "Select variable:", 
                    choices = choices_categorical, 
                    selected = NULL)
      ),
      
      
      conditionalPanel(
        condition = "input.tabs == 1", 
        selectInput("var",
                    label = "Select variable:",
                    choices = choices_categorical,
                    selected = "Operator")
        ),

      
      conditionalPanel(
        condition = "input.tabs == 4",
        
        sliderInput("dateSelect",
                    label = "Select date range:",
                    min = min(dat$Date),
                    max = max(dat$Date),
                    value = c(min(dat$Date), max(dat$Date))),

        checkboxGroupInput("sensorSelect",
                           label = "Select sensors:",
                           choices = choices_sensors,
                           selected = choices_sensors[1:30]),
        
        ),
        
      
      conditionalPanel(
        condition = "input.tabs == 7",

        selectizeInput("variableSelect", 
                       label = "Select variables:", 
                       choices = choices_categorical, 
                       multiple = TRUE, 
                       selected = choices_categorical[1:2])
      ),
      
      
      conditionalPanel(
        condition = "input.tabs == 8",

        selectInput("groupSelect",
                    label = "Select variables:",
                    choices = names(pairs_groups),
                    selected = "Y + Categoricals 1")
      ),

      
      conditionalPanel(
        condition = "input.tabs == 9",
        
        checkboxInput("showOutliers", 
                      label = "Include outliers", 
                      value = TRUE),
        
        checkboxInput("abs", 
                      label = "Use absolute correlation", 
                      value = TRUE),
        
        selectInput("corrMeth", 
                    label = "Correlation method", 
                    choices = c("pearson","spearman","kendall"), 
                    selected = "pearson"),
        
        selectInput("group", 
                    label = "Grouping method", 
                    choices = list("none" = FALSE,"OLO" = "OLO","GW" = "GW","HC" = "HC"), 
                    selected = "OLO")),
      

      conditionalPanel(
        condition = "input.tabs == 11",

        checkboxGroupInput("group_selector",
                           label = "Select variables:",
                           choices = names(tabPlot_groups),
                           selected = names(tabPlot_groups)),

       selectInput("sort",
                   label = "Sort by:",
                   choices = choices_sort,
                   selected = NULL))
      
      ),
    
    mainPanel(
      tabsetPanel(
        id = "tabs",
        
        tabPanel("Missing Data", 
                 value = 3,
                 plotOutput("visMiss"), 
                 plotOutput("visMiss2")),
        
        tabPanel("Rising Value Chart", 
                 value = 6,
                 plotOutput("risingValues"), height = "800px"),
        
        tabPanel("Box Plots", 
                 value = 5,
                 plotOutput("boxPlot")),
        
        tabPanel("Box Plots by Category", 
                 value = 12,
                 plotOutput("boxPlot2"),
                 textOutput("caption")),
        
        tabPanel("Categorical Data", 
                 value = 1,
                 plotOutput("barChart"),
                 plotOutput("boxPlots")),
        
        tabPanel("Sensor Plot", 
                 value = 4,
                 plotOutput("sensorPlot")),
        
        tabPanel("Correlation", 
                 value = 9,
                 withSpinner(
                   plotOutput("corrgramPlot"))),
        
        tabPanel("Pairs Plot", 
                 value = 8,
                 withSpinner(
                   plotOutput("pairsPlot"))),
        
        tabPanel("Mosaic Plot", 
                 value = 7, 
                 plotOutput("mosaicPlot"),
                 uiOutput("mosaicText")),
        
        tabPanel("Table Plot", 
                 value = 11,
                 withSpinner(
                   plotOutput("tabPlot"))),
        
        tabPanel("Data Table", 
                 value = 2,
                 DTOutput("dataTable")),
        
        tabPanel("Summary", 
                 value = 10,
                 htmlOutput("summaryOutput"))
        
        )
    )
  )
)
