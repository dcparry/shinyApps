# DATA423 assignment 2
# Dianne Parry
# ui file

source("global.R")

navbarPage(title = div(HTML('<strong>DATA423 Assignment 2 - Dianne Parry</strong>')), 
             selected = "EDA", 
             collapsible = TRUE, 
             inverse = TRUE, 
             fluid = TRUE, 
             theme = shinytheme("yeti"),
             
             
             tabPanel("EDA",
                      fluidPage(
                        tabsetPanel(
                          tabPanel("Missingness", br(),
                                   sidebarLayout(
                                     sidebarPanel(width = 2,
                                       
                                       selectInput("plotSelect",
                                                   label = "Select plot:",
                                                   choices = c("vis_miss", "gg_miss_var", "Upset Plot", "Cummulative Sum"),
                                                   selected = "vis_miss"),
                                       
                                       checkboxGroupInput("varSelect", 
                                                          label = "Select variables:", 
                                                          choices = choices_sort, 
                                                          selected = choices_sort),
                                       br(),
                                       
                                       checkboxInput("cluster", 
                                                     label = "Cluster missingness (vis_miss only)", 
                                                     value = FALSE),
                                       br(),
                                       
                                       sliderInput("nsets", 
                                                   label = "Number of sets (Upset only)", 
                                                   min = 1, 
                                                   max = 10, 
                                                   step = 1, 
                                                   value = 5)
                                       
                                       ), # close sidebarPanel
                                     
                                     mainPanel(
                                       plotOutput("selectedPlot", height = "600px")
                                       ) # close mainPanel
                                     
                                     ) # close sibebarLayout
                                   ), # close tabPanel
                          
                          tabPanel("Rising Value", 
                                   sidebarLayout(
                                     sidebarPanel(width = 2,
                                                  
                                                  checkboxGroupInput("numericSelect", 
                                                                     label = "Select variables:", 
                                                                     choices = names(numerics), 
                                                                     selected = names(numerics)),
                                                  
                                                  checkboxGroupInput("transformSelect",
                                                                     label = "",
                                                                     choices = c("Centre", "Scale"))
                                                  ), # close sidebarPanel
                                     
                                     mainPanel(
                                       plotOutput("risingValue", height = "600px")
                                     ) # close mainPanel
                                     
                                     ) # close sibebarLayout
                                   ), # close tabPanel
                          
                          tabPanel("Box Plots",
                                   sidebarLayout(
                                     sidebarPanel(width = 2,
                                                  
                                                  sliderInput("range", 
                                                              label = "IQR Multiplier", 
                                                              min = 0, 
                                                              max = 5, 
                                                              step = 0.1, 
                                                              value = 1.5),
                                                  
                                                  checkboxGroupInput("transformSelect2",
                                                                     label = "",
                                                                     choices = c("Centre", "Scale"),
                                                                     selected = FALSE),
                                                  
                                                  checkboxInput("outliers", 
                                                                label = "Show outliers", 
                                                                value = TRUE)
                                                  ), # close sidebarPanel
                                                  
                                     mainPanel(
                                       plotOutput("boxPlot", height = "600px")
                                     ) # close mainPanel
                                     
                                     
                                     ) # close sibebarLayout
                                   ), # close tabPanel
                          
                          tabPanel("Box Plots 2",
                                   sidebarLayout(
                                     sidebarPanel(width = 2,
                                                  
                                                  selectInput("var",
                                                              label = "Select variable:",
                                                              choices = choices_numerics,
                                                              selected = "POPULATION")
                                                  ), #close sidebarPanel

                                     mainPanel(
                                       
                                       plotOutput("boxPlots"),
                                       plotOutput("boxPlots2")
                                     ) # close mainPanel
                                     
                                     
                                   ) # close sibebarLayout
                          ),# close tabPanel
                          
                          tabPanel("Novelty",
                                   sidebarLayout(
                                     sidebarPanel(width = 2,
                                                  
                                                  sliderInput("range2", 
                                                              label = "IQR Multiplier", 
                                                              min = 0, 
                                                              max = 5, 
                                                              step = 0.1, 
                                                              value = 1.5),
                                                  
                                                  
                                                  selectInput("var2",
                                                              label = "Select variable:",
                                                              choices = choices_numerics,
                                                              selected = "POPULATION")
                                     ),
                                                  

                                     mainPanel(
                                       
                                       plotOutput("boxPlot3", height = "600px"),
                                       htmlOutput("outlierText")
                                     ) # close mainPanel
                                     
                                   ) # close sibebarLayout
                          ),# close tabPanel
                          
                          tabPanel("Scatter Plot",
                                   sidebarLayout(
                                     sidebarPanel(width = 2,
                                                  
                                                  selectInput("xVar",
                                                              label = "Select x variable:",
                                                              choices = choices_numerics,
                                                              selected = "POPULATION"),
                                                  
                                                  selectInput("yVar",
                                                              label = "Select y variable:",
                                                              choices = choices_numerics,
                                                              selected = "DEATH_RATE")
                                                  
                                     ), # close sidebarPanel
                                     
                                     mainPanel(
                                       
                                       plotOutput("scatterPlot", height = "600px")
                                     ) # close mainPanel
                                     
                                   ) # close sibebarLayout
                          ),# close tabPanel
                          
                          tabPanel("Correlation",
                                   
                                   sidebarLayout(
                                     sidebarPanel(width = 2,
                                                  
                                                  checkboxInput("abs", 
                                                                label = "Use absolute correlation", 
                                                                value = TRUE),
                                                  
                                                  selectInput("corrMeth", 
                                                              label = "Correlation method", 
                                                              choices = c("pearson","spearman","kendall"), 
                                                              selected = "spearman"),
                                                  
                                                  selectInput("group", 
                                                              label = "Grouping method", 
                                                              choices = list("none" = FALSE,"OLO" = "OLO","GW" = "GW","HC" = "HC"), 
                                                              selected = "OLO")
                                                  ), # close sidebarPanel
                                     
                                     mainPanel(
                                       withSpinner(
                                         plotOutput("corrgramPlot", height = "600px"))
                                       ) # close mainPanel
                                     
                                     ) # close sidebarLayout
                                   ), # close tabPanel
    
                          tabPanel("Pairs Plot",
                                   
                                   sidebarLayout(
                                     sidebarPanel(width = 2,

                                                  checkboxGroupInput("variableSelect2", 
                                                                     label = "Select variables:", 
                                                                     choices = choices_pairs,
                                                                     selected = choices_pairs[1:5]),
                                                  
                                                  selectInput("colourVar", 
                                                              label = "Variable to colour:", 
                                                              choices = choices_categorical, 
                                                              selected = choices_categorical[1]),
                                                  
                                                  actionButton("Plot", 
                                                               label = "Plot", 
                                                               icon = icon("play"))

                                     ), # close sidebarPanel
                                     
                                     mainPanel(
                                       withSpinner(
                                         plotOutput("pairsPlot", height = "600px"))
                                       ) # close mainPanel
                                     
                                     ) # close sidebarLayout
                                   ), # close tabPanel
                          
                          tabPanel("Mosaic",
                                   sidebarLayout(
                                     sidebarPanel(width = 2,
                                                  
                                                  selectInput("variableSelect1",
                                                              label = "Select variables:",
                                                              choices = choices_categorical,
                                                              multiple = TRUE,
                                                              selected = choices_categorical[1:2])
                                                  
                                                  ), # close sidebarPanel
                                     
                                     mainPanel(
                                         plotOutput("mosaicPlot", height = "600px"),
                                         uiOutput("mosaicText")
                                         )
                                     ) # close sibebarLayout
                                   ),# close tabPanel
                          
                          tabPanel("Table Plot",
                                   sidebarLayout(
                                     sidebarPanel(width = 2,
                                                  

                                                  selectInput("sort",
                                                  label = "Sort by:",
                                                  choices = choices_sort,
                                                  selected = "index")
                                                  
                                                  ), # close sidebarPanel
                                   
                                   mainPanel(
                                     plotOutput("tabPlot")
                                     )
                                   
                                   ) # close sidebarLayout
                                   ), # close tabPanel
                          
                          tabPanel("rpart tree", br(),
                                    
                                    mainPanel(
                                        plotOutput("predictMissing", height = "200px"),
                                        uiOutput("predMissText")
                                    ) # close mainPanel
                                    
                          ), # close tabPanel
                          
                          tabPanel("Feature Importance", br(),
                                    
                                    sidebarLayout(
                                      sidebarPanel(width = 2,
                                                   
                                                   radioButtons("selectNA",
                                                                label = "",
                                                                choices = c("NAs imputed", "NAs omitted"),
                                                                selected = "NAs imputed")
                                                   
                                      ), # close sidebarPanel
                                      
                                      mainPanel(
                                        
                                        withSpinner(
                                          DTOutput("featureImportance"))
                                        
                                      ) # close mainPanel
                                    ) # close sidebarLayout
                                    
                          ), # close tabPanel

                          tabPanel("Data Table",
                                   mainPanel(
                                     DTOutput(outputId = "dataTable")
                                     ) # close mainPanel
                                   ), # close tabPanel
                          
                          tabPanel("Summary",
                                   mainPanel(
                                     htmlOutput("summaryOutput")
                                     ) # close mainPanel
                                   ) # close tabPanel
                     
                        )# close tabsetPanel
                        ) # close fluidPage
                      ), # close tabPanel
           
           tabPanel("Model",
                    fluidPage(
                      tabsetPanel(
                        tabPanel("Train", br(),
                                 sidebarLayout(
                                   sidebarPanel(width = 2,
                                                
                                                tags$fieldset(
                                                  tags$legend("Missingness"),
                                                  
                                                sliderInput("varThreshold", 
                                                            label = "Threshold of variable missingness:", 
                                                            min = 0, 
                                                            max = 100, 
                                                            value = 50, 
                                                            post = "%"),
                                                
                                                sliderInput("obsThreshold", 
                                                            label = "Threshold of observation missingness:", 
                                                            min = 0, 
                                                            max = 100, 
                                                            value = 40, 
                                                            post = "%"),
                                                
                                                uiOutput("varSelectInput")
                                                
                                                ),
                                                
                                                tags$fieldset(
                                                  tags$legend("Outliers"),

                                                  selectizeInput("removeErrors", 
                                                                 label = "Select variables for potential error removal:", 
                                                                 choices = choices_errors, 
                                                                 multiple = TRUE), 
                                                  
                                                  selectizeInput("removeOutliers", 
                                                                 label = "Select variables for outlier removal:", 
                                                                 choices = choices_outliers, 
                                                                 multiple = TRUE)
                                                ),
                                                  
                                                tags$fieldset(
                                                  tags$legend("Imputation"),
                                                  
                                                  checkboxInput("nominalUnknown",
                                                                label = "Set missing nominal variables to 'unknown'",
                                                                value = FALSE),
                                                  
                                                  selectInput("impMethod",
                                                            label = "Select method:",
                                                            choices = c("KNN", "Mean & Mode","Median & Mode"),
                                                            selected = "KNN"),
                                                  numericInput("knnSelect",
                                                             label = "KNN value:",
                                                             value = 5, 
                                                             min = 1)
                                                ),
                                                
                                                tags$fieldset(
                                                  tags$legend("Transformations"),
                                                  
                                                  selectInput("transform",
                                                              label = "Select transformation type:",
                                                              choices = c("None", "Yeo Johnson", "Square root"),
                                                              selected = "Yeo Johnson"),
                                                  
                                                  selectInput("centreScale",
                                                              label = "Select normalisation:",
                                                              choices = c("None", "Centre", "Scale", "Centre & Scale"),
                                                              selected = "Centre & Scale")
                                                  ),


                                                tags$fieldset(
                                                  tags$legend("Resampling"),
                                                  
                                                selectInput("methodSelect",
                                                            label = "Select method:",
                                                            choices = c("cv", "repeatedcv", "LOOCV"),
                                                            selected = "repeatedcv"),
                                                
                                                numericInput("numberSelect",
                                                             label = "Number of folds (cv/repeatedcv):",
                                                             value = 10,
                                                             min = 0),
                                                
                                                numericInput("repeatsSelect",
                                                             label = "Number of repeats (repeatedcv):",
                                                             value = 5, 
                                                             min = 0)
                                                ),
                                                
                                                tags$fieldset(
                                                  tags$legend("Regularisation"),
                                                  
                                                sliderInput("alphaLength",
                                                            label = "Number of alpha values to try:",
                                                            min = 2,
                                                            max = 20,
                                                            value = 10),
                                                
                                                sliderInput("lambdaMinMax", 
                                                            label = "Set minimum and maximum lambda values:", 
                                                            min = 0, 
                                                            max = 1, 
                                                            value = c(0, 1),
                                                            round = FALSE),
                                                
                                                
                                                sliderInput("lambdaLength", 
                                                            label = "Number of lambda values to try:", 
                                                            min = 10, 
                                                            max = 20, 
                                                            value = 10)
                                                ),

                                                actionButton("Go", 
                                                             label = "Train", 
                                                             icon = icon("play"))
                    
                                                
                                   ), # close sidebarPanel
                                   
                                   mainPanel(
                                     withSpinner(
                                       plotOutput("missingPlot", height = "600px")),

                                     withSpinner(
                                       verbatimTextOutput("modelSummary")),
                                     
                                     withSpinner(
                                       plotOutput("plotModel"))
                                     
                                   ) # close mainPanel
                                   
                                 ) # close sibebarLayout
                        ) # close tabPanel
                        
                        ,
                        
                        tabPanel("Test", br(),
                                 sidebarLayout(
                                   sidebarPanel(width = 2,
                                                
                                                actionButton("Go2", 
                                                             label = "Test", 
                                                             icon = icon("play"))
                                                
                                   ), # close sidebarPanel
                                   
                                   mainPanel(
                                     
                                     withSpinner(
                                       plotlyOutput("visPreds"))
                                     
                                     ) # close mainPanel
                                   
                                 ) # close sibebarLayout
                        ) # close tabPanel
                        
                        ,tabPanel("Residual Box Plot", br(),
                                  
                                  sidebarLayout(
                                    sidebarPanel(width = 2,
                                                 
                                                 selectInput("residualType",
                                                             label = "Select residual type:",
                                                             choices = c("Test Residuals", "Train Residuals", "Test + Train Residuals"),
                                                             selected = "Test Residuals"),
                                                 
                                                 sliderInput("range3", 
                                                             label = "IQR Multiplier", 
                                                             min = 0, 
                                                             max = 5, 
                                                             step = 0.1, 
                                                             value = 1.5)
                                    ), # close sidebarPanel
                                  
                                  mainPanel(
                                      plotOutput("visResiduals", height = "700px"),
                                      
                                      htmlOutput("outlierText2")
                                      
                                      ) # close mainPanel
                                  ) # close sidebarLayout
                                  
                        ) # close tabPanel
                        
                        ,tabPanel("Residual Plots", br(),
                                  
                                    mainPanel(
                                      plotOutput("visResiduals2", height = "700px")
                                      
                                    ) # close mainPanel
                                
                        ) # close tabPanel
  
                        ) # close tabsetPanel
                    ) # close fluidPage
           ) # close tabPanel
    
  ) # close NavbarPage