# DATA423 assignment 2
# Dianne Parry
# server file 

source("global.R")

# server
function(input, output, session) {
  
  onSessionEnded(function() {
    stopApp()
  })

  # ----------------------------------------------------------------------------
  # EDA
  
  # missing data charts
  selected_columns <- reactive({
    selected_groups <- input$varSelect
    selected_vars <- data[selected_groups]
    #data[, selected_vars, drop = FALSE]
  })

  output$selectedPlot <- renderPlot({
    req(input$plotSelect)  # Ensure that a plot is selected

    # You can add more plots to this switch as needed
    selected_plot <- switch(input$plotSelect,
                            
                            "vis_miss" = {
                              visdat::vis_miss(selected_columns(), cluster = input$cluster) +
                                ggtitle("Missing Value Distribution") +
                                theme_bw() +
                                theme(
                                  plot.title = element_text(size = rel(1.8), face = "bold", hjust = 0.5),
                                  axis.title = element_text(size = rel(1.5), face = "bold"),
                                  axis.text.x = element_text(angle = 45, hjust = -0.1),
                                  axis.text = element_text(size = rel(1.2)),
                                  legend.title = element_text(size = rel(1.5), face = "bold"),
                                  legend.text = element_text(size = rel(1.2), face = "bold"),
                                  legend.key.size = unit(1.5, "lines")
                                )
                            },
                            
                            "gg_miss_var" = {
                              gg_miss_var(selected_columns()) + 
                                ggtitle("Number of Missing Values per Variable") +
                                theme(
                                  plot.title = element_text(size = rel(1.8), face = "bold", hjust = 0.5),
                                  axis.title = element_text(size = rel(1.5), face = "bold"),
                                  axis.text.x = element_text(angle = 45, hjust = -0.1),
                                  axis.text = element_text(size = rel(1.2)),
                                  legend.text = element_text(size = rel(1.2), face = "bold"),
                                  legend.key.size = unit(1.5, "lines"))
                            },
                            
                            "Upset Plot" = {
                              gg_miss_upset(selected_columns(), nsets = input$nsets, 
                                            text.scale = 1.8, matrix.color = "#255E91", 
                                            main.bar.color = "#255E91", sets.bar.color = "#255E91",
                                            point.size = 5, line.size = 1.5)
                            },
                            
                            "Cummulative Sum" = {
                              gg_miss_var_cumsum(selected_columns()) +
                                labs(title = "Cumulative Sum of Missing Values",
                                     x = "Variable",
                                     y = "Cumulative sum") +
                                theme_shiny() +
                                theme(axis.text.x = element_text(angle = 90))
                            }
    )
    print(selected_plot)  
    
  })
  
  
  # rising value chart
  output$risingValue <- renderPlot({
    selected_vars <- input$numericSelect
    transform_options <- input$transformSelect
    
    if (length(selected_vars) == 0) {
      return(NULL)
    }
    
    selected_columns <- numerics[selected_vars]
    
    if ("Centre" %in% transform_options) {
      selected_columns <- scale(x = selected_columns, center = TRUE, scale = FALSE)
    }
    
    if ("Scale" %in% transform_options) {
      selected_columns <- scale(x = selected_columns, center = FALSE, scale = TRUE)
    }
    
    for (col in 1:ncol(selected_columns)) {
      selected_columns[, col] <- selected_columns[order(selected_columns[, col]), col]
    }
    
    mypalette <- rainbow(ncol(selected_columns))
   
    layout(matrix(c(1, 2), nrow = 1), widths = c(2, 1))
    par(mar = c(4, 4, 2, 2) + 0.1)
    
    matplot(
      x = seq(1, 100, length.out = nrow(selected_columns)),
      y = selected_columns,
      type = "l",
      xlab = "Percentile",
      ylab = "Values",
      lty = 1,
      lwd = 1,
      col = mypalette,
      main = "Rising Value Chart for Numeric Data"
    )
    
    par(mar = c(2, 2, 2, 0) + 0.1)
    
    # Empty plot in the second column
    plot(1, 1, type = 'n', xlim = c(0, 1), ylim = c(0, 1), axes = FALSE) # only way I could get the legend outside the plot
    
    legend("topleft", legend = colnames(selected_columns), col = mypalette, lty = 1, lwd = 1, ncol = 1)
  })
 
  
  # box plot of numeric variables
  output$boxPlot <- renderPlot({

    if ("Centre" %in% input$transformSelect2) {
      numerics <- scale(numerics, center = TRUE, scale = FALSE)
    }

    if ("Scale" %in% input$transformSelect2) {
      numerics <- scale(numerics, center = FALSE, scale = TRUE)
    }
    
    par(mar=c(10, 4, 4, 4))
    

    car::Boxplot(
      y = numerics,
      ylab = NA,
      use.cols = TRUE,
      notch = TRUE,
      varwidth = FALSE,
      col = "#5B9BD5",
      main = "Boxplots of Numeric Data",
      outline = input$outliers,
      range = input$range,
      las = 2,
      id = ifelse(input$outliers, list(n = Inf, location = "avoid"), FALSE))

  })
  

  # box plots of values by categorical variables - GOVERN_TYPE
  output$boxPlots <- renderPlot({
    
    data$temp_var <- data[[input$var]]  
    
    ggplot(data, aes(x = GOVERN_TYPE, y = temp_var, fill = GOVERN_TYPE)) +
      geom_boxplot(notch = TRUE) +
      theme_shiny() + 
      labs(title = paste("Distribution of", input$var, "by Government Type"),
           y = input$var,
           x = "",
           fill = "Government Type") + 
      scale_fill_manual(values = plot_colours)
    
  })
  
  
  # box plots of values by categorical variables - HEALTHCARE_BASIS
  output$boxPlots2 <- renderPlot({
    
    data$temp_var <- data[[input$var]]  
    
    ggplot(data, aes(x = HEALTHCARE_BASIS, y = temp_var, fill = HEALTHCARE_BASIS)) +
      geom_boxplot(notch = TRUE) +
      theme_shiny() + 
      labs(title = paste("Distribution of", input$var, "by Healthcare System"),
           y = input$var,
           x = "",
           fill = "Heathcare System") + 
      scale_fill_manual(values = plot_colours)
    
  })
  
  
  # individual boxplots for outlier exploration
  output$boxPlot3 <- renderPlot ({

      coef <- input$range2

      selected_var <- numerics[[input$var2]]
      
      clean_numerics <- numerics[!is.na(selected_var), ]
      
      limits <- boxplot.stats(x = clean_numerics[[input$var2]], coef = coef)$stats

      clean_numerics$label <- ifelse(clean_numerics[[input$var2]] < limits[1] |
                                       clean_numerics[[input$var2]] > limits[5], 
                                     as.character(rownames(clean_numerics)), NA)
      
      ggplot(data = clean_numerics, aes(x = "", y = .data[[input$var2]], label = label, fill = "Variable")) +
        geom_boxplot(coef = coef, outlier.colour = "darkorange") +
        geom_text_repel(aes(label = ifelse(!is.na(label), as.character(label), NA)),
                        na.rm = TRUE,
                        box.padding = 0.9, 
                        point.padding = 0.5, 
                        size = 5, 
                        min.segment.length = 0, 
                        max.overlaps = 20) +
        theme_shiny() +
        labs(title = paste("Uni-variable boxplots of", input$var2, "at IQR multiplier of", coef), x = "") +
        theme(legend.position = "none")+ 
        scale_fill_manual(values = "#5B9BD5")
        
    })
  

    output$outlierText <- renderUI({  
      coef <- input$range2
      
      selected_col <- numerics[[input$var2]]
      clean_numerics <- numerics[!is.na(selected_col), ]
      limits <- boxplot.stats(x = clean_numerics[[input$var2]], coef = coef)$stats
      
      lower_outliers <- clean_numerics[clean_numerics[[input$var2]] < limits[1], ]
      upper_outliers <- clean_numerics[clean_numerics[[input$var2]] > limits[5], ]
      
      if(nrow(lower_outliers) > 0) {
        lower_outlier_labels <- paste("Lower outliers (", nrow(lower_outliers), "): ", paste(rownames(lower_outliers), collapse=", "), "<br>")
      } else {
        lower_outlier_labels <- "Lower outliers: None<br>"
      }
      
      if(nrow(upper_outliers) > 0) {
        upper_outlier_labels <- paste("Upper outliers (", nrow(upper_outliers), "): ", paste(rownames(upper_outliers), collapse=", "))
      } else {
        upper_outlier_labels <- "Upper outliers: None"
      }
      
      outlier_summary <- paste(lower_outlier_labels, upper_outlier_labels, sep="")
      
      HTML(outlier_summary)
    })
  
  
  # scatter plot
  output$scatterPlot <- renderPlot({
    
    ggplot(numerics, aes(x = .data[[input$xVar]], y = .data[[input$yVar]])) +
      geom_point() +
      geom_smooth() +
      labs(title = paste(input$xVar, "vs", input$yVar),
           x = input$xVar,
           y = input$yVar) +
      theme_shiny()
    
  })
  
 
  # correlation plot
  output$corrgramPlot <- renderPlot({
    
    corrgram::corrgram(numerics,
             order = input$group,
             abs = input$abs, # doesn't seem to work?
             cor.method = input$corrMeth,
             main = paste("Correlation of Numeric Data using", input$corrMeth, "method"),
             lower.panel= panel.shade,
             upper.panel=panel.cor,
             cex.labels = 1.2)
  })
  
  
  # pairs plot select variables
  plot_data_reactive <- eventReactive(input$Plot, {

    selected_vars <- c(input$variableSelect2, input$colourVar)

    data[selected_vars]

  })

  # 
  # # plot pairs plot
  # output$pairsPlot <- renderPlot({
  # 
  #   req(plot_data_reactive())
  # 
  #   ggpairs(plot_data_reactive(), aes(color = .data[[input$colourVar]])) +
  #       theme_bw()
  #   })

  
  # plot pairs plot
  output$pairsPlot <- renderPlot({
 
    validate(
      need(plot_data_reactive(), "Data is required for plotting."),
      need(input$colourVar %in% input$variableSelect2, "The variable selected for colouring must be included in the variables selected for plotting.\nPlease select the appropriate variable and re-click 'Plot' if required")
    )
    
    ggpairs(plot_data_reactive(), aes_string(color = input$colourVar)) +
      theme_bw()
  })
  
  
  # mosaic plot
  output$mosaicPlot <- renderPlot({
    selected_cat <- input$variableSelect1
    
    if (length(selected_cat) > 0) { # so if no variables selected, doesn't have ugly error message
      formula <- as.formula(paste("~", paste(selected_cat, collapse = " + ")))
      vcd::mosaic(formula, data = data,
                  main = "Mosaic Chart of Categorical Variables",
                  shade = TRUE,
                  legend = TRUE)
    }
  })
  
  
  # caption for mosaic
  output$mosaicText <- renderUI({
    HTML('<div style="text-align: center;">Dark Blue indicates unusually common.<br>
    Light blue indicates common.<br>
    Light red indicates uncommon.<br>
    Dark red indicates unusually uncommon.</div>')
  })
  
  
  # table plot
  output$tabPlot <- renderPlot({
    
    tabplot::tableplot(data[1:16], sortCol = input$sort, scales = "lin")
  })
  
  
  # predicting missingness
  output$predictMissing <- renderPlot ({
    
    data <- data[order(data$CODE),]
    data$index <- 1:nrow(data)
    
    data$missingness <- apply(X = is.na(data), MARGIN = 1, FUN = sum)
    
    tree <- caret::train(missingness ~ . -CODE -OBS_TYPE - HEALTHCARE_COST_SHADOW, 
                         data = data, 
                         method = "rpart", 
                         na.action = na.rpart) # na.rpart means "rpart will deal with missing predictors intrinsically"
    
    rpart.plot(tree$finalModel, 
               main = "Predicting the number of missing variables in an observation",
               roundint = TRUE, 
               clip.facs = TRUE,
               tweak = 1.5)
    
    
  })
  
  
  # caption for predict missingness
  output$predMissText <- renderUI({
    HTML('<div style="text-align: center;
    "><br>The rpart tree shows missingness can be partially predicted by POP_DENSITY.<br>
    Where POP_DENSITY is 500 or greater, there is an average of 3.9 missing values per <br>observation, otherwise the average is 1.</div>')
  })
  
  
  #feature importance 
  output$featureImportance <- renderDT({
    
    req(input$selectNA)
    
    imputed_data <- missRanger(data, num.trees=100, verbose=FALSE)
    
    imputed_data <- imputed_data %>% 
      dplyr::select(-CODE, -OBS_TYPE, -HEALTHCARE_COST_SHADOW, -index)
    
    data_no_NAs <- data_no_NAs %>% 
      select(-CODE, -OBS_TYPE, -HEALTHCARE_COST_SHADOW, -index)
    
    if (input$selectNA == "NAs imputed") {
      data = imputed_data
    } else {
      data = data_no_NAs
    }
    
    rf_model <- randomForest(x = data[, -which(names(data) == "DEATH_RATE")], 
                             y = data$DEATH_RATE,
                             ntree = 500,  
                             importance = TRUE)
    
    importance_results <- importance(rf_model)
    sorted_importance <- importance_results[order(importance_results[,'%IncMSE'], decreasing = TRUE), ]
    
    importance_df <- as.data.frame(sorted_importance)
    
    importance_df <- round(importance_df, 3)
    
    rownames_to_column(importance_df, "Feature")
  }, 
  options = list(pageLength = 12, orderClasses = TRUE, scrollX = TRUE)
  )
  
  
  # data table
  output$dataTable <- renderDT(
    data[1:16], filter = 'top', options = list(
      pageLength = 10)
  )
  
  
  # summary
  output$summaryOutput <- renderUI({
    HTML({
      summary_text <- capture.output({
        data[1:15] %>%
          summarytools::dfSummary(col.widths = c(10, 80, 150, 120, 120, 180, 220)) %>%
          summarytools::view(method = "render")
      })
      paste(summary_text, collapse = "\n")
    })
  })

  
  # ----------------------------------------------------------------------------
  # Processing + model
  
  
  output$missingPlot <- renderPlot({
    data <- getCleanTrainData()
    
    obs <- nrow(data)
    vars <- ncol(data)
    
    visdat::vis_dat(data) +
      labs(title = paste("Thresholds: VarMiss =", input$varThreshold,"% ObsMiss =", input$obsThreshold,"% \n Remaining:", vars, "variables and", obs, "observations")) +
      theme(
        plot.title = element_text(size = rel(1.8), face = "bold", hjust = 0.5),
        axis.title = element_text(size = rel(1.5), face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = -0.1),
        axis.text = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1.2), face = "bold"),
        legend.key.size = unit(1.5, "lines"))
  })
  
  
  trainData <- reactiveVal()  # Placeholder for training data

  # Example of initializing with sample data
  observe({
    trainData(train)
  })
  

  # general data cleaning function for missingness
  cleanData <- function(data, varThreshold, obsThreshold) {
    vRatio <- apply(X = data, MARGIN = 2, FUN = pMiss)
    data <- data[, vRatio < varThreshold]
    oRatio <- apply(X = data, MARGIN = 1, FUN = pMiss)
    data <- data[oRatio < obsThreshold, ]
    return(data)
  }
  
  
  # to create a reactive list of variable names for UI
  cleanedData <- reactive({
    req(trainData(), input$varThreshold, input$obsThreshold)
    cleanData(trainData(), input$varThreshold, input$obsThreshold)
  })
  
  
  # reactive output for variable names
  output$varSelectInput <- renderUI({
    data <- cleanedData()  # Get cleaned data
    req(ncol(data) > 0)  # Ensure there are columns to choose from
    selectizeInput("selectedVars", 
                   "Select variables for shadow creation:",
                   choices = names(data),
                   multiple = TRUE)
  })

  # pre-processing for potential errors and outliers
  getCleanTrainData <- reactive({
    data <- cleanData(trainData(), input$varThreshold, input$obsThreshold)
    
    selected_vars <- input$removeErrors
    if (length(selected_vars) > 0) {
      for (var in selected_vars) {
        if (var %in% names(error_indices)) {
          data <- data %>%
            dplyr::filter(!(index %in% error_indices[[var]]))
        }
      }
    }

    selected_vars <- input$removeOutliers
    if (length(selected_vars) > 0) {
      for (var in selected_vars) {
        if (var %in% names(outlier_indices)) {
          data <- data %>%
            dplyr::filter(!(index %in% outlier_indices[[var]]))
        }
      }
    }
    
    data

  })
  
  # the recipe pipeline
  getRecipe <- reactive({

    rec <- recipes::recipe(DEATH_RATE ~., data = getCleanTrainData()) %>%  # define target and dataframe
      update_role("CODE", new_role = "id") %>% # assign CODE variable to id role (i.e. not a predictor)
      update_role("index", new_role = "id") %>% # assign index variable to id role (i.e. not a predictor)
      update_role("OBS_TYPE", new_role = "split")  # assign OBS_TYPE to splitting role
    
    for (var in input$selectedVars) {
      if (any(is.na(getCleanTrainData()[[var]]))) {
        shadow_name <- paste0(var, "_SHADOW") # create shadow variable
        rec <- rec %>% 
          step_mutate(!!shadow_name := as.integer(is.na(!!rlang::sym(var))))
      }
    }

    if (input$nominalUnknown == TRUE) {
      rec <- rec %>%
        step_unknown(all_nominal_predictors()) # create 'unknown' level for factors with missing values
    }
      
    if (input$impMethod == "KNN") { # imputation methods
      rec <- rec %>%
        step_impute_knn(all_predictors(), neighbors = input$knnSelect)
    } else if (input$impMethod == "Mean & Mode") {
      rec <- rec %>%
        step_impute_mean(all_numeric_predictors()) %>%
        step_impute_mode(all_nominal_predictors())
    } else if (input$impMethod == "Median & Mode") {
      rec <- rec %>%
        step_impute_median(all_numeric_predictors()) %>%
        step_impute_mode(all_nominal_predictors())
    }
    
    rec <- rec %>% 
      step_novel(all_nominal_predictors())  # to pick up any new categorical values in unseen data (e.g. new GOVERN_TYPE)
    
    if (input$transform == "Yeo Johnson") { # transformations
      rec <- rec %>%
        step_YeoJohnson(all_numeric_predictors())
    } else if (input$transform == "Square root") {
      rec <- rec %>%
        step_sqrt(all_numeric_predictors())
    }
    
    rec <- rec %>% 
      step_dummy(all_nominal_predictors()) %>%  # encode categorical features
      step_nzv(all_predictors()) # remove vars with zero variance


    if (input$centreScale == "Centre") { # transformations
      rec <- rec %>%
        step_center(all_numeric_predictors())
    } else if (input$centreScale == "Scale") {
      rec <- rec %>%
        step_scale(all_numeric_predictors())
    } else if (input$centreScale == "Centre & Scale") {
      rec <- rec %>%
        step_normalize(all_numeric_predictors())
    }
    
    # prep the recipe
    rec <- rec %>%
      prep(getCleanTrainData())

    #print(rec) # to check thst it's working
    rec

  })
  
  # traincontrol parameters for model
  getTrainControl <- reactive({
    
    req(input$methodSelect)
    req(input$numberSelect)
    req(input$repeatsSelect)

    train_control <- trainControl(method = input$methodSelect, 
                                  number = input$numberSelect,
                                  repeats = input$repeatsSelect)
  })
  
  # tuning parameters for model
  getTuneGrid <- reactive({
    
    req(input$alphaLength)
    req(input$lambdaMinMax)
    req(input$lambdaLength)

    tune_grid <- expand.grid(
      alpha = seq(0, 1, length = input$alphaLength),
      lambda = seq(input$lambdaMinMax[1],input$lambdaMinMax[2], length = input$lambdaLength)
    )
    
  })
  
  # the model
  getModel <- reactive({
    req(input$Go)
    
    isolate({
      
      set.seed(123) 
      model <- caret::train(getRecipe(),
                            data = getCleanTrainData(), 
                            method = "glmnet",
                            tuneGrid = getTuneGrid(),
                            trControl = getTrainControl())
      
      importance <- varImp(model)

      list(model = model, importance = importance)
      
    })
    
  })
  
  
  # display best alpha, lambda, RMSE, and feature importance
  output$modelSummary <- renderPrint({
    req(getModel())
    
    info <- getModel()
    
    results <- info$model$results
    best_alpha <- info$model$bestTune$alpha
    best_lambda <- info$model$bestTune$lambda
    best_rmse <- min(results$RMSE)
    feature_importance <- info$importance
    coefficients <- coef(info$model$finalModel, s = best_lambda)
    
    cat("Best alpha:", best_alpha, "\n")
    cat("Best lambda:", best_lambda, "\n")
    cat("Best RMSE:", best_rmse, "\n")
    cat("\n")
    print(feature_importance)
    #print(coefficients)
    #print(info$model)

  })
  
  
  # apply model to train data
  getResultsTrain <- reactive({

    train_data <- getCleanTrainData()
    
    predictions <- predict(getModel()$model, newdata = train_data)
    
    train_data$predictions <- predictions  
    
    train_data$residuals <- train_data$DEATH_RATE - train_data$predictions
    
    return(train_data)
  })
  
  # plot model
  output$plotModel <- renderPlot({
    req(getModel())
    
    model = getModel()$model
    
    plot(model)
    
  })

  
  #-----------------------------------------------------------------------------
  # test
  
  # apply model to test data
  getResultsTest <- reactive({
    req(input$Go2)
    
    test_data <- test

    predictions <- predict(getModel()$model, newdata = test_data)
    
    test_data$predictions <- predictions  
    
    test_data$residuals <- test_data$DEATH_RATE - test_data$predictions
    return(test_data)
  })
  

  # visualise predictions
  output$visPreds <- renderPlotly({
    req(input$Go2, getResultsTest())
    test_data <- getResultsTest()
    
    rmse_value <- sqrt(mean((test_data$DEATH_RATE - test_data$predictions)^2))
    
    rang <- range(c(test_data$DEATH_RATE, test_data$predictions))
    
    p <- ggplot(data = test_data, mapping = aes(x = predictions, y = DEATH_RATE, label = index)) +
      geom_point() +
      geom_abline(slope = 1, col = "#5B9BD5") +
      labs(title = paste("Predictions of Death Rate ( RSME = ", round(rmse_value, 3), ")"), 
                         y = "actual", 
                         x = "predicted") + 
      coord_fixed(ratio = 1, xlim = rang, ylim = rang, expand = TRUE) +
      theme_bw() + 
      theme(
        plot.title = element_text(size = rel(1.5), face = "bold", hjust = 0.5),
        axis.title = element_text(size = rel(1.2), face = "bold"),
        axis.text = element_text(size = rel(1)))
      
    plotly::ggplotly(p, width = 660, height = 700)  # note the height adjustment for the plotly menu
  })

  #-----------------------------------------------------------------------------
  # Residuals
  
  # visualise residuals in box plot
  output$visResiduals <- renderPlot({

    req(input$residualType, input$range3)

    test_data <- req(getResultsTest())
    train_data <- req(getResultsTrain())

    data <- switch(input$residualType,
                   "Test Residuals" = {
                     test_data$type <- 'Test'
                     data.frame(index = test_data$index, residuals = test_data$residuals, Type = test_data$type)
                   },
                   
                   "Train Residuals" = {
                     train_data$type <- 'Train'
                     data.frame(index = train_data$index, residuals = train_data$residuals, Type = train_data$type)
                   },
                   
                   "Test + Train Residuals" = {
                     test_data$type <- 'Test'
                     train_data$type <- 'Train'
                     combined <- rbind(
                       data.frame(index = test_data$index, residuals = test_data$residuals, Type = test_data$type),
                       data.frame(index = train_data$index, residuals = train_data$residuals, Type = train_data$type)
                     )
                     combined
                   }
    )

    coef <- input$range3

    stats <- boxplot.stats(data$residuals, coef = coef)
    limits <- stats$stats

    outliers <- data[data$residuals %in% stats$out, ]

    ggplot(data, aes(x = "", y = residuals)) +
      geom_boxplot(coef = coef, fill = "lightgrey") + 
      geom_point(data = outliers, aes(y = residuals, color = Type), size = 2) +
      geom_text_repel(data = outliers, aes(label = index, y = residuals),
                      box.padding = 0.9,
                      point.padding = 0.5,
                      size = 5,
                      min.segment.length = 0,
                      max.overlaps = 20) +
      labs(title = paste("Box plot of", input$residualType, "with IQR multiplier of", coef),
           x = "",
           y = "Residuals") +
      theme_shiny() +
      scale_color_manual(values = c("Test" = "blue4", "Train" = "darkorange"))
  })
  

  output$outlierText2 <- renderUI({
    coef <- input$range3

    req(input$residualType, input$range3)

    test_data <- req(getResultsTest())

    train_data <- req(getResultsTrain())

    data <- switch(input$residualType,
                   "Test Residuals" = data.frame(index = test_data$index, residuals = test_data$residuals),
                   "Train Residuals" = data.frame(index = train_data$index, residuals = train_data$residuals),
                   "Test + Train Residuals" = data.frame(index = c(test_data$index, train_data$index), 
                                                         residuals = c(test_data$residuals, train_data$residuals))
    )

    coef <- input$range3
    
    iqr <- IQR(data$residuals)
    lower_bound <- quantile(data$residuals, 0.25) - coef * iqr
    upper_bound <- quantile(data$residuals, 0.75) + coef * iqr
    
    data$outlier <- data$residuals < lower_bound | data$residuals > upper_bound
    
    lower_outliers <- data[data$outlier & data$residuals < lower_bound, ]
    upper_outliers <- data[data$outlier & data$residuals > upper_bound, ]


    if(nrow(lower_outliers) > 0) {
      lower_outlier_labels <- paste("Lower outliers (", nrow(lower_outliers), "): ", paste(lower_outliers$index, collapse=", "), "<br>")
    } else {
      lower_outlier_labels <- "Lower outliers: None<br>"
    }

    if(nrow(upper_outliers) > 0) {
      upper_outlier_labels <- paste("Upper outliers (", nrow(upper_outliers), "): ", paste(upper_outliers$index, collapse=", "))
    } else {
      upper_outlier_labels <- "Upper outliers: None"
    }

    outlier_summary <- paste(lower_outlier_labels, upper_outlier_labels, sep="")

    HTML(outlier_summary)
  })
  
  # visualise residual plots 
  output$visResiduals2 <- renderPlot({
    
    test_data <- req(getResultsTest())

    test <- data.frame(predictions = test_data$predictions, residuals = test_data$residuals)
    
    plot1 <- ggplot(test, aes(x = predictions, y = residuals)) +
      geom_hline(yintercept = 0, color = "red") +
      geom_point() +
      labs(x = "Fitted Values", y = "Residuals", title = "Residual Plot (test)") +
      theme_shiny()
    
    plot2 <- ggplot(test, aes(sample = residuals)) +
      stat_qq() +
      stat_qq_line(color = "red") +
      ggtitle("Q-Q Plot of Residuals (test)") +
      xlab("Theoretical Quantiles") +
      ylab("Sample Quantiles") +
      theme_shiny()
    
    train_data <- req(getResultsTrain())
    
    train <- data.frame(predictions = train_data$predictions, residuals = train_data$residuals)
    
    plot3 <- ggplot(train, aes(x = predictions, y = residuals)) +
      geom_hline(yintercept = 0, color = "red") +
      geom_point() +
      labs(x = "Fitted Values", y = "Residuals", title = "Residual Plot (train)") +
      theme_shiny()
    
    plot4 <- ggplot(train, aes(sample = residuals)) +
      stat_qq() +
      stat_qq_line(color = "red") +
      ggtitle("Q-Q Plot of Residuals (train)") +
      xlab("Theoretical Quantiles") +
      ylab("Sample Quantiles") +
      theme_shiny()
    
    combined_plot <- (plot3 | plot4) / (plot1 | plot2)
    
    combined_plot
  })
  

  #-----------------------------------------------------------------------------

} # close server

