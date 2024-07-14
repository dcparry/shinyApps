shinyUI(fluidPage(
  
  # Application title
  titlePanel("Assignment 3 - Dianne Parry"),
  tabsetPanel(
    tabPanel("Data",
             verbatimTextOutput(outputId = "DataSummary"),
             fluidRow(
               column(width = 4,
                      sliderInput(inputId = "Multiplier", label = "IQR multiplier", min = 0, max = 10, step = 0.1, value = 1.5)
               ),
               column(width = 3,
                      checkboxInput(inputId = "Normalise", label = "Standardise chart", value = TRUE)
               )
             ),
             plotOutput(outputId = "BoxPlots"),
             plotOutput(outputId = "Missing"),
             plotOutput(outputId = "Corr"),
             DT::dataTableOutput(outputId = "Table")
    ), 
    tabPanel("Split",
             sliderInput(inputId = "Split", label = "Train proportion", min = 0, max = 1, value = 0.8),
             verbatimTextOutput(outputId = "SplitSummary")
    ),
    tabPanel("Available methods",
             h3("Regression methods in caret"),
             shinycssloaders::withSpinner(DT::dataTableOutput(outputId = "Available"))
    ),
    tabPanel("Methods",
             checkboxInput(inputId = "Parallel", label = "Use parallel processing", value = FALSE),
             bsTooltip(id = "Parallel", title = "Turn off parallel processing to view any training errors in the console"),
             "The preprocessing steps and their order are important.",
             HTML("See function <code>dynamicSteps</code> in global.R for interpretation of preprocessing options. "),
             "Documentation", tags$a("here", href = "https://www.rdocumentation.org/packages/recipes/versions/0.1.16", target = "_blank"),
             
             tabsetPanel(type = "pills",
                         tabPanel("NULL Model",
                                  br(),
                                  fluidRow(
                                    column(width = 4),
                                    column(width = 1,
                                           actionButton(inputId = "null_Go", label = "Train", icon = icon("play")),
                                           bsTooltip(id = "null_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "null_Load", label = "Load", icon = icon("file-arrow-up")),
                                           bsTooltip(id = "null_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "null_Delete", label = "Forget", icon = icon("trash-can")),
                                           bsTooltip(id = "null_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "null_Metrics"),
                                  hr(),
                                  verbatimTextOutput(outputId = "null_Recipe")
                         ),
                         tabPanel("GLMnet Model",
                                  verbatimTextOutput(outputId = "glmnet_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "glmnet_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(glmnet_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = glmnet_initial),  # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "glmnet_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")

                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "glmnet_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "glmnet_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "glmnet_Load", label = "Load", icon = icon("file-arrow-up")),
                                           bsTooltip(id = "glmnet_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "glmnet_Delete", label = "Forget", icon = icon("trash-can")),
                                           bsTooltip(id = "glmnet_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "glmnet_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "glmnet_ModelPlots"),
                                  verbatimTextOutput(outputId = "glmnet_Recipe"),
                                  verbatimTextOutput(outputId = "glmnet_ModelSummary2"),
                                  wellPanel(
                                    h3("Coefficients"),
                                    tableOutput(outputId = "glmnet_Coef")  #  <- typically this is specific to OLS
                                  )
                         ),
                         tabPanel("PLS Model",
                                  verbatimTextOutput(outputId = "pls_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "pls_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(pls_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = pls_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "pls_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "pls_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "pls_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "pls_Load", label = "Load", icon = icon("file-arrow-up")),
                                           bsTooltip(id = "pls_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "pls_Delete", label = "Forget", icon = icon("trash-can")),
                                           bsTooltip(id = "pls_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "pls_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "pls_ModelPlots"),
                                  verbatimTextOutput(outputId = "pls_Recipe"),
                                  verbatimTextOutput(outputId = "pls_ModelSummary2"),
                                  wellPanel(
                                    h3("Coefficients"),
                                    tableOutput(outputId = "pls_Coef")  #  <- typically this is specific to OLS
                                  )
                         ),
                         tabPanel("Rpart Model",
                                  verbatimTextOutput(outputId = "rpart_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models                                 selectizeInput(inputId = "rpart_Preprocess",
                                           selectizeInput(inputId = "rpart_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(rpart_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = rpart_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "rpart_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rpart_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "rpart_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rpart_Load", label = "Load", icon = icon("file-arrow-up")),
                                           bsTooltip(id = "rpart_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rpart_Delete", label = "Forget", icon = icon("trash-can")),
                                           bsTooltip(id = "rpart_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "rpart_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "rpart_ModelPlots"),
                                  verbatimTextOutput(outputId = "rpart_Recipe"),
                                  plotOutput(outputId = "rpart_ModelTree")   #  <- this tree-plot is unique to the rpart method
                         ),
                         
                         
                         # maintenance point ------------------------------------------------------------------------------
                         # add further tabs (with controls) here
                         
                         # tabPanel("pcr Model",
                         #          verbatimTextOutput(outputId = "pcr_ModelSummary0"),
                         #          fluidRow(
                         #            column(width = 4,
                         #                   # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models                                 selectizeInput(inputId = "rpart_Preprocess",
                         #                   selectizeInput(inputId = "pcr_Preprocess",
                         #                                  label = "Pre-processing",
                         #                                  choices = unique(c(pcr_initial, ppchoices)),
                         #                                  multiple = TRUE,
                         #                                  selected = pcr_initial), # <-- These are suggested starting values. Set these to your best recommendation
                         #                   bsTooltip(id = "pcr_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                         #            ),
                         #            column(width = 1,
                         #                   actionButton(inputId = "pcr_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                         #                   bsTooltip(id = "pcr_Go", title = "This will train or retrain your model (and save it)")
                         #            ),
                         #            column(width = 1,
                         #                   actionButton(inputId = "pcr_Load", label = "Load", icon = icon("file-arrow-up")),
                         #                   bsTooltip(id = "pcr_Load", title = "This will reload your saved model")
                         #            ),
                         #            column(width = 1,
                         #                   actionButton(inputId = "pcr_Delete", label = "Forget", icon = icon("trash-can")),
                         #                   bsTooltip(id = "pcr_Delete", title = "This will remove your model from memory")
                         #            )
                         #          ),
                         #          hr(),
                         #          h3("Resampled performance:"),
                         #          tableOutput(outputId = "pcr_Metrics"),
                         #          hr(),
                         #          plotOutput(outputId = "pcr_ModelPlots", height = "600px"),
                         #          verbatimTextOutput(outputId = "pcr_Recipe"),
                         #          verbatimTextOutput(outputId = "pcr_ModelSummary2"),
                         #          wellPanel(
                         #            h3("Coefficients"),
                         #            tableOutput(outputId = "pcr_Coef")  #  <- typically this is specific to OLS
                         #          )
                         # ),
                         
                         tabPanel("lmStepAIC Model",
                                  verbatimTextOutput(outputId = "lmStepAIC_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models                                 selectizeInput(inputId = "rpart_Preprocess",
                                           selectizeInput(inputId = "lmStepAIC_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(lmStepAIC_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = lmStepAIC_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "lmStepAIC_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "lmStepAIC_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "lmStepAIC_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "lmStepAIC_Load", label = "Load", icon = icon("file-arrow-up")),
                                           bsTooltip(id = "lmStepAIC_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "lmStepAIC_Delete", label = "Forget", icon = icon("trash-can")),
                                           bsTooltip(id = "lm_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "lmStepAIC_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "lmStepAIC_ModelPlots", height = "600px"),
                                  verbatimTextOutput(outputId = "lmStepAIC_Recipe"),
                                  verbatimTextOutput(outputId = "lmStepAIC_ModelSummary2"),
                                  wellPanel(
                                    h3("Coefficients"),
                                    tableOutput(outputId = "lmStepAIC_Coef")  #  <- typically this is specific to OLS
                                  )
                         ),
                         # 
                         # tabPanel("bstTree Model",
                         #          verbatimTextOutput(outputId = "bstTree_ModelSummary0"),
                         #          fluidRow(
                         #            column(width = 4,
                         #                   # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models                                 selectizeInput(inputId = "rpart_Preprocess",
                         #                   selectizeInput(inputId = "bstTree_Preprocess",
                         #                                  label = "Pre-processing",
                         #                                  choices = unique(c(bstTree_initial, ppchoices)),
                         #                                  multiple = TRUE,
                         #                                  selected = bstTree_initial), # <-- These are suggested starting values. Set these to your best recommendation
                         #                   bsTooltip(id = "bstTree_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                         #            ),
                         #            column(width = 1,
                         #                   actionButton(inputId = "bstTree_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                         #                   bsTooltip(id = "bstTree_Go", title = "This will train or retrain your model (and save it)")
                         #            ),
                         #            column(width = 1,
                         #                   actionButton(inputId = "bstTree_Load", label = "Load", icon = icon("file-arrow-up")),
                         #                   bsTooltip(id = "bstTree_Load", title = "This will reload your saved model")
                         #            ),
                         #            column(width = 1,
                         #                   actionButton(inputId = "bstTree_Delete", label = "Forget", icon = icon("trash-can")),
                         #                   bsTooltip(id = "bstTree_Delete", title = "This will remove your model from memory")
                         #            )
                         #          ),
                         #          hr(),
                         #          h3("Resampled performance:"),
                         #          tableOutput(outputId = "bstTree_Metrics"),
                         #          hr(),
                         #          plotOutput(outputId = "bstTree_ModelPlots"),
                         #          verbatimTextOutput(outputId = "bstTree_Recipe"),
                         #          verbatimTextOutput(outputId = "bstTree_ModelSummary2")
                         # ),
                         # 
                         # tabPanel("rf Model",
                         #          verbatimTextOutput(outputId = "rf_ModelSummary0"),
                         #          fluidRow(
                         #            column(width = 4,
                         #                   # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models                                 selectizeInput(inputId = "rpart_Preprocess",
                         #                   selectizeInput(inputId = "rf_Preprocess",
                         #                                  label = "Pre-processing",
                         #                                  choices = unique(c(rf_initial, ppchoices)),
                         #                                  multiple = TRUE,
                         #                                  selected = rf_initial),
                         #                   bsTooltip(id = "rf_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                         #            ),
                         #            column(width = 1,
                         #                   actionButton(inputId = "rf_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                         #                   bsTooltip(id = "rf_Go", title = "This will train or retrain your model (and save it)")
                         #            ),
                         #            column(width = 1,
                         #                   actionButton(inputId = "rf_Load", label = "Load", icon = icon("file-arrow-up")),
                         #                   bsTooltip(id = "rf_Load", title = "This will reload your saved model")
                         #            ),
                         #            column(width = 1,
                         #                   actionButton(inputId = "rf_Delete", label = "Forget", icon = icon("trash-can")),
                         #                   bsTooltip(id = "rf_Delete", title = "This will remove your model from memory")
                         #            )
                         #          ),
                         #          hr(),
                         #          h3("Resampled performance:"),
                         #          tableOutput(outputId = "rf_Metrics"),
                         #          hr(),
                         #          # plotOutput(outputId = "rf_ModelPlots"),
                         #          verbatimTextOutput(outputId = "rf_Recipe"),
                         #          verbatimTextOutput(outputId = "rf_ModelSummary2")
                         # ),
                         # 
                         # tabPanel("svmRadialCost Model",
                         #          verbatimTextOutput(outputId = "svmRadialCost_ModelSummary0"),
                         #          fluidRow(
                         #            column(width = 4,
                         #                   # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models                                 selectizeInput(inputId = "rpart_Preprocess",
                         #                   selectizeInput(inputId = "svmRadialCost_Preprocess",
                         #                                  label = "Pre-processing",
                         #                                  choices = unique(c(svmRadialCost_initial, ppchoices)),
                         #                                  multiple = TRUE,
                         #                                  selected = svmRadialCost_initial),
                         #                   bsTooltip(id = "svmRadialCost_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                         #            ),
                         #            column(width = 1,
                         #                   actionButton(inputId = "svmRadialCost_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                         #                   bsTooltip(id = "svmRadialCost_Go", title = "This will train or retrain your model (and save it)")
                         #            ),
                         #            column(width = 1,
                         #                   actionButton(inputId = "svmRadialCost_Load", label = "Load", icon = icon("file-arrow-up")),
                         #                   bsTooltip(id = "svmRadialCost_Load", title = "This will reload your saved model")
                         #            ),
                         #            column(width = 1,
                         #                   actionButton(inputId = "svmRadialCost_Delete", label = "Forget", icon = icon("trash-can")),
                         #                   bsTooltip(id = "svmRadialCost_Delete", title = "This will remove your model from memory")
                         #            )
                         #          ),
                         #          hr(),
                         #          h3("Resampled performance:"),
                         #          tableOutput(outputId = "svmRadialCost_Metrics"),
                         #          hr(),
                         #          # plotOutput(outputId = "svmRadialCost_ModelPlots"),
                         #          verbatimTextOutput(outputId = "svmRadialCost_Recipe"),
                         #          verbatimTextOutput(outputId = "svmRadialCost_ModelSummary2")
                         # ),

                         tabPanel("svmLinear Model",
                                  verbatimTextOutput(outputId = "svmLinear_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models                                 selectizeInput(inputId = "rpart_Preprocess",
                                           selectizeInput(inputId = "svmLinear_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(svmLinear_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = svmLinear_initial),
                                           bsTooltip(id = "svmLinear_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "svmLinear_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "svmLinear_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "svmLinear_Load", label = "Load", icon = icon("file-arrow-up")),
                                           bsTooltip(id = "svmLinear_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "svmLinear_Delete", label = "Forget", icon = icon("trash-can")),
                                           bsTooltip(id = "svmLinear_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "svmLinear_Metrics"),
                                  hr(),
                                  # plotOutput(outputId = "svmLinear_ModelPlots"),
                                  verbatimTextOutput(outputId = "svmLinear_Recipe"),
                                  verbatimTextOutput(outputId = "svmLinear_ModelSummary2")
                         ),
                         # 
                         # tabPanel("gaussprRadial Model",
                         #          verbatimTextOutput(outputId = "gaussprRadial_ModelSummary0"),
                         #          fluidRow(
                         #            column(width = 4,
                         #                   # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models                                 selectizeInput(inputId = "rpart_Preprocess",
                         #                   selectizeInput(inputId = "gaussprRadial_Preprocess",
                         #                                  label = "Pre-processing",
                         #                                  choices = unique(c(gaussprRadial_initial, ppchoices)),
                         #                                  multiple = TRUE,
                         #                                  selected = gaussprRadial_initial),
                         #                   bsTooltip(id = "gaussprRadial_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                         #            ),
                         #            column(width = 1,
                         #                   actionButton(inputId = "gaussprRadial_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                         #                   bsTooltip(id = "gaussprRadial_Go", title = "This will train or retrain your model (and save it)")
                         #            ),
                         #            column(width = 1,
                         #                   actionButton(inputId = "gaussprRadial_Load", label = "Load", icon = icon("file-arrow-up")),
                         #                   bsTooltip(id = "gaussprRadial_Load", title = "This will reload your saved model")
                         #            ),
                         #            column(width = 1,
                         #                   actionButton(inputId = "gaussprRadial_Delete", label = "Forget", icon = icon("trash-can")),
                         #                   bsTooltip(id = "gaussprRadial_Delete", title = "This will remove your model from memory")
                         #            )
                         #          ),
                         #          hr(),
                         #          h3("Resampled performance:"),
                         #          tableOutput(outputId = "gaussprRadial_Metrics"),
                         #          hr(),
                         #          # plotOutput(outputId = "gaussprRadial_ModelPlots"),
                         #          verbatimTextOutput(outputId = "gaussprRadial_Recipe"),
                         #          verbatimTextOutput(outputId = "gaussprRadial_ModelSummary2")
                         # ),
                         
                         tabPanel("gaussprLinear Model",
                                  verbatimTextOutput(outputId = "gaussprLinear_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models                                 selectizeInput(inputId = "rpart_Preprocess",
                                           selectizeInput(inputId = "gaussprLinear_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(gaussprLinear_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = gaussprLinear_initial),
                                           bsTooltip(id = "gaussprLinear_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "gaussprLinear_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "gaussprLinear_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "gaussprLinear_Load", label = "Load", icon = icon("file-arrow-up")),
                                           bsTooltip(id = "gaussprLinear_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "gaussprLinear_Delete", label = "Forget", icon = icon("trash-can")),
                                           bsTooltip(id = "gaussprLinear_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "gaussprLinear_Metrics"),
                                  hr(),
                                  # plotOutput(outputId = "gaussprLinear_ModelPlots"),
                                  verbatimTextOutput(outputId = "gaussprLinear_Recipe"),
                                  verbatimTextOutput(outputId = "gaussprLinear_ModelSummary2")
                         ),
                         
                         tabPanel("monmlp Model",
                                  verbatimTextOutput(outputId = "monmlp_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models                                 selectizeInput(inputId = "rpart_Preprocess",
                                           selectizeInput(inputId = "monmlp_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(monmlp_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = monmlp_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "monmlp_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "monmlp_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "monmlp_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "monmlp_Load", label = "Load", icon = icon("file-arrow-up")),
                                           bsTooltip(id = "monmlp_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "monmlp_Delete", label = "Forget", icon = icon("trash-can")),
                                           bsTooltip(id = "monmlp_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "monmlp_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "monmlp_ModelPlots"),
                                  verbatimTextOutput(outputId = "monmlp_Recipe"),
                                  verbatimTextOutput(outputId = "monmlp_ModelSummary2")
                         )
                         
                         # ,
                         # tabPanel("mlpML Model",
                         #          verbatimTextOutput(outputId = "mlpML_ModelSummary0"),
                         #          fluidRow(
                         #            column(width = 4,
                         #                   # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models                                 selectizeInput(inputId = "rpart_Preprocess",
                         #                   selectizeInput(inputId = "mlpML_Preprocess",
                         #                                  label = "Pre-processing",
                         #                                  choices = unique(c(mlpML_initial, ppchoices)),
                         #                                  multiple = TRUE,
                         #                                  selected = mlpML_initial),
                         #                   bsTooltip(id = "mlpML_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                         #            ),
                         #            column(width = 1,
                         #                   actionButton(inputId = "mlpML_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                         #                   bsTooltip(id = "mlpML_Go", title = "This will train or retrain your model (and save it)")
                         #            ),
                         #            column(width = 1,
                         #                   actionButton(inputId = "mlpML_Load", label = "Load", icon = icon("file-arrow-up")),
                         #                   bsTooltip(id = "mlpML_Load", title = "This will reload your saved model")
                         #            ),
                         #            column(width = 1,
                         #                   actionButton(inputId = "mlpML_Delete", label = "Forget", icon = icon("trash-can")),
                         #                   bsTooltip(id = "mlpML_Delete", title = "This will remove your model from memory")
                         #            )
                         #          ),
                         #          hr(),
                         #          h3("Resampled performance:"),
                         #          tableOutput(outputId = "mlpML_Metrics"),
                         #          hr(),
                         #          # plotOutput(outputId = "mlpML_ModelPlots"),
                         #          verbatimTextOutput(outputId = "mlpML_Recipe"),
                         #          verbatimTextOutput(outputId = "mlpML_ModelSummary2")
                         # )

                         
                         
                         
                         
                         
                         # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
             )
    ),
    tabPanel("Model Selection",
             tags$h5("Cross validation results:"),
             checkboxInput(inputId = "Notch", label = "Show notch", value = FALSE),
             checkboxInput(inputId = "NullNormalise", label = "Normalise", value = TRUE),
             checkboxInput(inputId = "HideWorse", label = "Hide models worse than null model", value = TRUE),
             plotOutput(outputId = "SelectionBoxPlot"),
             radioButtons(inputId = "Choice", label = "Model choice", choices = c(""), inline = TRUE )
    ),
    tabPanel("Performance",
             htmlOutput(outputId = "Title"),
             verbatimTextOutput(outputId = "TestSummary"),
             fluidRow(
               column(offset = 2, width = 4,
                      plotOutput(outputId = "TestPlot")
               ),
               column(width = 2,
                      plotOutput(outputId = "TestResiduals")
               ),
               column(width = 2,
                      plotOutput(outputId = "TrainResiduals"),
               )
             ),
             sliderInput(inputId = "IqrM", label = "IQR multiplier", min = 0, max = 5, value = 1.5, step = 0.1),
    )
  )
))
