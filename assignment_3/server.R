shinyServer(function(input, output, session) {
  
  # initialisation ----
  models <- reactiveValues()  # this is a collection of the models

    
  # Ensure the "SavedModels folder exists
  if (!"./SavedModels" %in% list.dirs()) {
    dir.create("./SavedModels")
  }
  
  shiny::onSessionEnded(stopApp)

  
  # reactive getData ----
  getData <- reactive({
    d <- read.csv(file = "Ass3Data.csv", row.names = "Patient", stringsAsFactors = TRUE)  # "Patient" is no longer a variable
    d$ObservationDate <- as.Date(d$ObservationDate, "%Y-%m-%d")
    d
  })
  
  # output BoxPlots ----
  output$BoxPlots <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    req(d, input$Multiplier, length(numeric) > 0)
    d <- scale(d[,numeric], center = input$Normalise, scale = input$Normalise)
    boxplot(d, outline = TRUE, main = paste("Boxplot using IQR multiplier of", input$Multiplier), range = input$Multiplier, las = 2)
  })
  
  # output Missing ----
  output$Missing <- renderPlot({
    d <- getData()
    vis_dat(d)
  })
  
  # output Corr ----
  output$Corr <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    req(d, length(numeric) > 0)
    corrgram::corrgram(d, order = "OLO", main = "Numeric Data Correlation")
  })
  
  # output DataSummary ----
  output$DataSummary <- renderPrint({
    str(getData())
  })
  
  # output Table ----
  output$Table <- DT::renderDataTable({
    d <- getData()
    numeric <- c(FALSE, sapply(d, is.numeric)) # never round rownames which are the first column (when shown)
    DT::datatable(d) %>%
      formatRound(columns = numeric, digits = 3)
  })
  
  # reactive get Split
  getSplit <- reactive({
    set.seed(199)
    createDataPartition(y = getData()$Response, p = input$Split, list = FALSE)
  })
  
  # reactive getMethods ----
  getMethods <- reactive({
    mi <- caret::getModelInfo()
    Label <- vector(mode = "character", length = length(mi))
    Package <- vector(mode = "character", length = length(mi))
    Hyperparams <- vector(mode = "character", length = length(mi))
    Regression <- vector(mode = "logical", length = length(mi))
    Classification <- vector(mode = "logical", length = length(mi))
    Tags <- vector(mode = "character", length = length(mi))
    ClassProbs <- vector(mode = "character", length = length(mi))
    for (row in 1:length(mi)) {
      Label[row] <- mi[[row]]$label
      libs <- mi[[row]]$library
      libs <- na.omit(libs[libs != ""]) # remove blank libraries
      if (length(libs) > 0) {
        present <- vector(mode = "logical", length = length(libs))
        suppressWarnings({
          for (lib in 1:length(libs)) {
            present[lib] <- require(package = libs[lib], warn.conflicts = FALSE, character.only = TRUE, quietly = TRUE)
          }
        })
        check <- ifelse(present, "", as.character(icon(name = "ban")))
        Package[row] <- paste(collapse = "<br/>", paste(mi[[row]]$library, check))
      }
      d <- mi[[row]]$parameters
      Hyperparams[row] <- paste(collapse = "<br/>", paste0(d$parameter, " - ", d$label, " [", d$class,"]"))
      Regression[row] <- ifelse("Regression" %in% mi[[row]]$type, as.character(icon("check-square", class = "fa-3x")), "")
      Classification[row] <- ifelse("Classification" %in% mi[[row]]$type , as.character(icon("check-square", class = "fa-3x")),"")
      Tags[row] <- paste(collapse = "<br/>", mi[[row]]$tags)
      ClassProbs[row] <- ifelse(is.function(mi[[row]]$prob), as.character(icon("check-square", class = "fa-3x")), "")
    }
    data.frame(Model = names(mi), Label, Package, Regression, Classification, Tags, Hyperparams, ClassProbs, stringsAsFactors = FALSE)
  })
  
  # output Available ----
  output$Available <- DT::renderDataTable({
     m <- getMethods()
     m <- m[m$Regression != "", !colnames(m) %in% c("Regression", "Classification", "ClassProbs")]  # hide columns because we are looking at regression methods only
     DT::datatable(m, escape = FALSE, options = list(pageLength = 5, lengthMenu = c(5,10,15)), rownames = FALSE, selection = "none")
  })
  
  # reactive getTrainData ----
  getTrainData <- reactive({
    getData()[getSplit(),]
  })
  
  # reactive getTestData ----
  getTestData <- reactive({
    getData()[-getSplit(),]
  })
  
  # reactive getTrControl ----
  getTrControl <- reactive({
    # shared bootstrap specification i.e. 25 x bootstrap
    y <- getTrainData()[,"Response"]
    n <- 25
    set.seed(673)
    seeds <- vector(mode = "list", length = n + 1)
    for (i in 1:n) {
      seeds[[i]] <- as.integer(c(runif(n = 55, min = 1000, max = 5000)))
    }
    seeds[[n + 1]] <- as.integer(runif(n = 1, min = 1000, max = 5000))
    trainControl(method = "boot", number = n, repeats = NA, allowParallel = TRUE, search = "grid", 
                 index = caret::createResample(y = y, times = n), savePredictions = "final", seeds = seeds, 
                 trim = TRUE)
  })
  
  # output SplitSummary ----
  output$SplitSummary <- renderPrint({
    cat(paste("Training observations:", nrow(getTrainData()), "\n", "Testing observations:", nrow(getTestData())))
  })
  
  # reactive getResamples ----
  getResamples <- reactive({
    models2 <- reactiveValuesToList(models) %>% 
      rlist::list.clean( fun = is.null, recursive = FALSE)
    req(length(models2) > 1)
    results <- caret::resamples(models2)
    
    #scale metrics using null model. Tough code to follow -sorry
    NullModel <- "null"
    if (input$NullNormalise & NullModel %in% results$models) {
      actualNames <- colnames(results$values)
      # Normalise the various hyper-metrics except R2 (as this is already normalised)
      for (metric in c("RMSE", "MAE")) {
        col <- paste(sep = "~", NullModel, metric)
        if (col %in% actualNames) {
          nullMetric <- mean(results$values[, col], na.rm = TRUE)
          if (!is.na(nullMetric) & nullMetric != 0) {
            for (model in results$models) {
              mcol <- paste(sep = "~", model, metric)
              if (mcol %in% actualNames) {
                results$values[, mcol] <- results$values[, mcol] / nullMetric
              }
            }
          }
        }
      }
    }
    
    # hide results worse than null model
    subset <- rep(TRUE, length(models2))
    if (input$HideWorse & NullModel %in% names(models2)) {
      actualNames <- colnames(results$values)
      col <- paste(sep = "~", "null","RMSE" )
      if (col %in% actualNames) {
        nullMetric <- mean(results$values[, col], na.rm = TRUE)
        if (!is.na(nullMetric)) {
          m <- 0
          for (model3 in results$models) {
            m <- m + 1
            mcol <- paste(sep = "~", model3, "RMSE")
            if (mcol %in% actualNames) {
              subset[m] <- mean(results$values[, mcol], na.rm = TRUE) <= nullMetric
            }
          }
        }
      }
      results$models <- results$models[subset]
    }
    
    updateRadioButtons(session = session, inputId = "Choice", choices = results$models, selected = "monmlp")  ## change the value parameter to your best method
    results
  })
  
  # output SelectionBoxPlot (plot) ----
  output$SelectionBoxPlot <- renderPlot({
    mod <- getResamples()
    bwplot(mod, notch = input$Notch)
  })
  
  # output Title (UI) ----
  output$Title <- renderUI({
    tags$h3(paste("Unseen data results for chosen model:", input$Choice))
  })
  
  # reactive getTestResults ----
  getTestResults <- reactive({
    dat <- getTestData()
    req(input$Choice)
    mod <- models[[input$Choice]]
    predictions <- predict(mod, newdata = dat)
    d <- data.frame(dat$Response, predictions, row.names = rownames(dat))
    colnames(d) <- c("obs", "pred")
    d
  })
  
  # reactive getTrainResults ----
  getTrainResults <- reactive({
    dat <- getTrainData()
    req(input$Choice)
    mod <- models[[input$Choice]]
    predictions <- predict(mod, newdata = dat)
    d <- data.frame(dat$Response, predictions, row.names = rownames(dat))
    colnames(d) <- c("obs", "pred")
    d
  })
  
  # Range for charts
  getResidualRange <- reactive({
    d1 <- getTrainResults()
    d1$residuals <- d1$obs - d1$pred
    d2 <- getTestResults()
    d2$residuals <- d2$obs - d2$pred
    d <- c(d1$residuals, d2$residuals)
    range(d, na.rm = TRUE)
  })
  
  # output TestSummary (print)
  output$TestSummary <- renderPrint({
    if (is.null(input$Choice) || input$Choice == "") {
      cat("No model chosen")
    } else {
      caret::defaultSummary(getTestResults())
    }
  })
  
  # output TestPlot (plot) ----
  output$TestPlot <- renderPlot({
    d <- getTestResults()
    req(nrow(d) > 0)
    par(pty = "s")
    range <- range(c(d$obs, d$pred), na.rm = TRUE)
    plot(d, xlim = range, ylim = range, main = "Predicted versus Observed for test data")
    abline(a = 0, b = 1, col = c("blue"), lty = c(2), lwd = c(3))
  })
  
  # output TestResiduals (plot) ----
  output$TestResiduals <- renderPlot({
    d <- getTestResults()
    req(nrow(d) > 0)
    d$residuals <- d$obs - d$pred
    coef <- input$IqrM
    limits <- boxplot.stats(x = d$residuals, coef = coef)$stats
    label <- ifelse(d$residuals < limits[1] | d$residuals > limits[5], rownames(d), NA)
    ggplot(d, mapping = aes(y = residuals, x = 0, label = label)) +
      ylim(getResidualRange()[1], getResidualRange()[2]) +
      geom_boxplot(coef = coef, orientation = "vertical", ) +
      ggrepel::geom_text_repel() +
      labs(title = "Test-Residual Boxplot",  subtitle = paste(coef, "IQR Multiplier")) +
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  })
  
  # output TrainResiduals (plot) ----
  output$TrainResiduals <- renderPlot({
    d <- getTrainResults()
    req(nrow(d) > 0)
    d$residuals <- d$obs - d$pred
    coef <- input$IqrM
    limits <- boxplot.stats(x = d$residuals, coef = coef)$stats
    label <- ifelse(d$residuals < limits[1] | d$residuals > limits[5], rownames(d), NA)
    ggplot(d, mapping = aes(y = residuals, x = 0, label = label)) +
      ylim(getResidualRange()[1], getResidualRange()[2]) +
      geom_boxplot(coef = coef, orientation = "vertical") +
      ggrepel::geom_text_repel() +
      labs(title = "Train-Residual Boxplot",  subtitle = paste(coef, "IQR Multiplier")) +
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  })
  
  
  # METHOD * null ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getNullRecipe ----
  getNullRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData())
  })
  
  # observeEvent null_Go ----
  observeEvent(
    input$null_Go,
    {
      method <- "null"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getNullRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl())
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )

  observeEvent(
    input$null_Load,
    {
      method  <- "null"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$null_Delete,
    {
      models[["null"]] <- NULL
      gc()
    }
  )
  
  # observeEvent null_Metrics ----
  output$null_Metrics <- renderTable({
    req(models$null)
    models$null$results[ which.min(models$null$results[, "RMSE"]), ]
  })
  
  # output null_Recipe ---
  output$null_Recipe <- renderPrint({
    req(models$null)
    models$null$recipe
  })  
  

  
  
  # METHOD * glmnet ---------------------------------------------------------------------------------------------------------------------------
  library(glmnet)   #  <------ Declare any modelling packages that are needed (see Method List tab)
  # reactive getGlmnetRecipe ----
  getGlmnetRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$glmnet_Preprocess) %>%           # use <method>_Preprocess 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent glmnet_Go ----
  observeEvent(
    input$glmnet_Go,
    {
      method <- "glmnet"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)

      startTime <- Sys.time()
      
      tryCatch({
        model <- caret::train(getGlmnetRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 5, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        endTime <- Sys.time()
        timeTaken <- endTime - startTime
        cat("Time taken to run the glmnet model:", timeTaken, "minutes\n")
        
        removeNotification(id = method)
        stopMode(clus)
      })
     }
  )
  
  observeEvent(
    input$glmnet_Load,
    {
      method  <- "glmnet"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$glmnet_Delete,
    {
      models[["glmnet"]] <- NULL
      gc()
    }
  )
  
  # output glmnet_ModelSummary (text) ----
  output$glmnet_ModelSummary0 <- renderText({
    description("glmnet")   # Use the caret method name here
  })
  
  # output glmnet_Metrics (table) ----
  output$glmnet_Metrics <- renderTable({
    req(models$glmnet)
    models$glmnet$results[ which.min(models$glmnet$results[, "RMSE"]), ]
  })
  
  # output glmnet_ModelPlots (plot) ----
  output$glmnet_ModelPlots <- renderPlot({
    req(models$glmnet)
    plot(models$glmnet)
  })

  # output glmnet_Recipe (print) ----
  output$glmnet_Recipe <- renderPrint({
    req(models$glmnet)
    models$glmnet$recipe
  })  
  
  # output glmnet_ModelSummary2 (print) ----
  output$glmnet_ModelSummary2 <- renderPrint({
    req(models$glmnet)
    print(models$glmnet)
  })

  # output glmnet_Coef (print) ----
  output$glmnet_Coef <- renderTable({
    req(models$glmnet)
    co <- as.matrix(coef(models$glmnet$finalModel, s  = models$glmnet$bestTune$lambda))  # special for glmnet
    df <- as.data.frame(co, row.names = rownames(co))
    df[df$s1 != 0.000, ,drop=FALSE]
  }, rownames = TRUE, colnames = FALSE)


  # METHOD * pls ---------------------------------------------------------------------------------------------------------------------------
  library(pls)  #  <------ Declare any modelling packages that are needed (see Method List tab)
  
  # reactive getPlsRecipe ----
  getPlsRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$pls_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent pls_Go ----
  observeEvent(
    input$pls_Go,
    {
      method <- "pls"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getPlsRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), 
                              tuneLength = 25, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$pls_Load,
    {
      method  <- "pls"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$pls_Delete,
    {
      models[["pls"]] <- NULL
      gc()
    }
  )
  
  # output pls_ModelSummary0 (text) ----
  output$pls_ModelSummary0 <- renderText({
    description("pls")   # Use the caret method name here
  })

  # output pls_Metrics (table) ----
  output$pls_Metrics <- renderTable({
    req(models$pls)
    models$pls$results[ which.min(models$pls$results[, "RMSE"]), ]
  })
  
  # output pls_ModelPlots (plot) ----
  output$pls_ModelPlots <- renderPlot({
    req(models$pls)
    plot(models$pls)
  })     
  
  # output pls_Recipe (print) ----
  output$pls_Recipe <- renderPrint({
    req(models$pls)
    models$pls$recipe
  })  

  # output pls_ModelSummary2 (print) ----
  output$pls_ModelSummary2 <- renderPrint({
    req(models$pls)
    summary(models$pls$finalModel)
  })
  
  # output pls_Coef (print) ----
  output$pls_Coef <- renderTable({
    req(models$pls)
    co <- coef(models$pls$finalModel)
    as.data.frame(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)
  
  
  # METHOD * rpart ---------------------------------------------------------------------------------------------------------------------------
  library(rpart)  #  <------ Declare any modelling packages that are needed (see Method List tab)
  library(rpart.plot)
  
  # reactive getRpartRecipe ----
  getRpartRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$rpart_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))
  })
  
  # observeEvent rpart_Go ----
  observeEvent(
    input$rpart_Go,
    {
      method <- "rpart"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getRpartRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(),
                              tuneLength = 5, na.action = na.rpart)  #<- note the rpart-specific value for na.action (not needed for other methods)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )

  observeEvent(
    input$rpart_Load,
    {
      method  <- "rpart"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$rpart_Delete,
    {
      models[["rpart"]] <- NULL
      gc()
    }
  )
  
  # output rpart_ModelSummary0 (print) ----
  output$rpart_ModelSummary0 <- renderText({
    description("rpart")   # Use the caret method name here
  })
  
  # output rpart_Metrics (table) ----
  output$rpart_Metrics <- renderTable({
    req(models$rpart)
    models$rpart$results[ which.min(models$rpart$results[, "RMSE"]), ]
  })
  
  # output rpart_Recipe (print) ----
  output$rpart_Recipe <- renderPrint({
    req(models$rpart)
    models$rpart$recipe
  })  
  
  # output rpart_ModelPlots (plot) ----
  output$rpart_ModelPlots <- renderPlot({
    req(models$rpart)
    plot(models$rpart)
  })
  
  # output rpart_ModelTree (plot) ----
  output$rpart_ModelTree <- renderPlot({
    req(models$rpart)
    rpart.plot::rpart.plot(models$rpart$finalModel, roundint = FALSE)
  })     
  

  
  # maintenance point ---------------------------------------------------------------------------------------------------------------------------
  # add further methods here  
  
  # METHOD * pcr ---------------------------------------------------------------------------------------------------------------------------
 
  # reactive getpcrRecipe ----
  getpcrRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$pcr_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))
  })
  
  # observeEvent pcr_Go ----
  observeEvent(
    input$pcr_Go,
    {
      method <- "pcr"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getpcrRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(),
                              tuneLength = 5)  
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$pcr_Load,
    {
      method  <- "pcr"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$pcr_Delete,
    {
      models[["pcr"]] <- NULL
      gc()
    }
  )
  
  # output pcr_ModelSummary0 (print) ----
  output$pcr_ModelSummary0 <- renderText({
    description("pcr")   # Use the caret method name here
  })
  
  # output pcr_Metrics (table) ----
  output$pcr_Metrics <- renderTable({
    req(models$pcr)
    models$pcr$results[ which.min(models$pcr$results[, "RMSE"]), ]
  })
  
  # output pcr_Recipe (print) ----
  output$pcr_Recipe <- renderPrint({
    req(models$pcr)
    models$pcr$recipe
  })  
  
  # output pcr_ModelPlots (plot) ----
  output$pcr_ModelPlots <- renderPlot({
    req(models$pcr)
    pcr_final_mod <- models$pcr$finalModel
    par(mfrow = c(2, 2))
    plot(pcr_final_mod)
  })
  
  # output pcr_ModelSummary2 (print) ----
  output$pcr_ModelSummary2 <- renderPrint({
    req(models$pcr)
    print(models$pcr)
  })
  
  # output pcr_Coef (print) ----
  output$pcr_Coef <- renderTable({
    req(models$pcr)
    co <- coef(models$pcr$finalModel)
    
    co_df <- as.data.frame(co)
    co_df$Predictor <- rownames(co_df)
    colnames(co_df) <- c("Coefficient", "Predictor")
    
    co_df <- co_df[order(abs(co_df$Coefficient), decreasing = TRUE), ]
    
    co_df
  }, rownames = FALSE, colnames = TRUE)
  
  # METHOD * lmStepAIC ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getLmStepAICRecipe ----
  getLmStepAICRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$lmStepAIC_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))
  })
  
  # observeEvent lmStepAIC_Go ----
  observeEvent(
    input$lmStepAIC_Go,
    {
      method <- "lmStepAIC"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      
      startTime <- Sys.time()
      
      tryCatch({
        model <- caret::train(getLmStepAICRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(),
                              tuneLength = 5)  
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        endTime <- Sys.time()
        timeTaken <- endTime - startTime
        cat("Time taken to run the lmStepAIC model:", timeTaken, "minutes\n")
        
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$lmStepAIC_Load,
    {
      method  <- "lmStepAIC"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$lmStepAIC_Delete,
    {
      models[["lmStepAIC"]] <- NULL
      gc()
    }
  )
  
  # output lmStepAIC_ModelSummary0 (print) ----
  output$lmStepAIC_ModelSummary0 <- renderText({
    description("lmStepAIC")   # Use the caret method name here
  })
  
  # output lmStepAIC_Metrics (table) ----
  output$lmStepAIC_Metrics <- renderTable({
    req(models$lmStepAIC)
    models$lmStepAIC$results[ which.min(models$lmStepAIC$results[, "RMSE"]), ]
  })
  
  # output lmStepAIC_Recipe (print) ----
  output$lmStepAIC_Recipe <- renderPrint({
    req(models$lmStepAIC)
    models$lmStepAIC$recipe
  })  
  
  # output lmStepAIC_ModelPlots (plot) ----
  output$lmStepAIC_ModelPlots <- renderPlot({
    req(models$lmStepAIC)
    lmStepAIC_final_mod <- models$lmStepAIC$finalModel
    par(mfrow = c(2, 2))
    plot(lmStepAIC_final_mod)
  })
  
  # output lmStepAIC_ModelSummary2 (print) ----
  output$lmStepAIC_ModelSummary2 <- renderPrint({
    req(models$lmStepAIC)
    print(models$lmStepAIC)
  })
  
  # output lmStepAIC_Coef (print) ----
  output$lmStepAIC_Coef <- renderTable({
    req(models$lmStepAIC)
    co <- coef(models$lmStepAIC$finalModel)
    
    co_df <- as.data.frame(co)
    co_df$Predictor <- rownames(co_df)
    colnames(co_df) <- c("Coefficient", "Predictor")
    
    co_df <- co_df[order(abs(co_df$Coefficient), decreasing = TRUE), ]
    
    co_df
  }, rownames = FALSE, colnames = TRUE)
  

  # METHOD * bstTree ---------------------------------------------------------------------------------------------------------------------------
  library(bst)  

  # reactive getBstTreeRecipe ----
  getBstTreeRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$bstTree_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))
  })
  
  # observeEvent bstTree_Go ----
  observeEvent(
    input$bstTree_Go,
    {
      method <- "bstTree"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getBstTreeRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(),
                              tuneLength = 5)  
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$bstTree_Load,
    {
      method  <- "bstTree"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$bstTree_Delete,
    {
      models[["bstTree"]] <- NULL
      gc()
    }
  )
  
  # output bstTree_ModelSummary0 (print) ----
  output$bstTree_ModelSummary0 <- renderText({
    description("bstTree")   # Use the caret method name here
  })
  
  # output bstTree_Metrics (table) ----
  output$bstTree_Metrics <- renderTable({
    req(models$bstTree)
    models$bstTree$results[ which.min(models$bstTree$results[, "RMSE"]), ]
  })
  
  # output bstTree_Recipe (print) ----
  output$bstTree_Recipe <- renderPrint({
    req(models$bstTree)
    models$bstTree$recipe
  })  
  
  # output bstTree_ModelPlots (plot) ----
  output$bstTree_ModelPlots <- renderPlot({
    req(models$bstTree)
    plot(models$bstTree)
  })
  
  # output bstTree_Summary2 (print) ----
  output$bstTree_ModelSummary2 <- renderPrint({
    req(models$bstTree)
    print(models$bstTree)
  })
  
  
  # METHOD * monmlp ---------------------------------------------------------------------------------------------------------------------------
  library(monmlp)  
  
  # reactive getMonmlpRecipe ----
  getMonmlpRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$monmlp_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))
  })
  
  # observeEvent monmlp_Go ----
  observeEvent(
    input$monmlp_Go,
    {
      method <- "monmlp"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      startTime <- Sys.time()
      tryCatch({
        model <- caret::train(getMonmlpRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(),
                              tuneLength = 5)  
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        endTime <- Sys.time()
        timeTaken <- endTime - startTime
        cat("Time taken to run the monmlp model:", timeTaken, "minutes\n")
        
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$monmlp_Load,
    {
      method  <- "monmlp"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$monmlp_Delete,
    {
      models[["monmlp"]] <- NULL
      gc()
    }
  )
  
  # output monmlp_ModelSummary0 (print) ----
  output$monmlp_ModelSummary0 <- renderText({
    description("monmlp")   # Use the caret method name here
  })
  
  # output monmlp_Metrics (table) ----
  output$monmlp_Metrics <- renderTable({
    req(models$monmlp)
    models$monmlp$results[ which.min(models$monmlp$results[, "RMSE"]), ]
  })
  
  # output monmlp_Recipe (print) ----
  output$monmlp_Recipe <- renderPrint({
    req(models$monmlp)
    models$monmlp$recipe
  })  
  
  # output monmlp_ModelPlots (plot) ----
  output$monmlp_ModelPlots <- renderPlot({
    req(models$monmlp)
    plot(models$monmlp)
  })
  
  # output monmlp_ModelSummary2 (print) ----
  output$monmlp_ModelSummary2 <- renderPrint({
    req(models$monmlp)
    print(models$monmlp)
  })
  
  
  # METHOD * svmLinear ---------------------------------------------------------------------------------------------------------------------------
  library(kernlab)

  # reactive getSvmLinearRecipe ----
  getSvmLinearRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$svmLinear_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))
  })

  # observeEvent svmLinear_Go ----
  observeEvent(
    input$svmLinear_Go,
    {
      method <- "svmLinear"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getSvmLinearRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(),
                              tuneLength = 5)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )

  observeEvent(
    input$svmLinear_Load,
    {
      method  <- "svmLinear"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )

  observeEvent(
    input$svmLinear_Delete,
    {
      models[["svmLinear"]] <- NULL
      gc()
    }
  )

  # output svmLinear_ModelSummary0 (print) ----
  output$svmRadial_ModelSummary0 <- renderText({
    description("svmLinear")   # Use the caret method name here
  })

  # output svmLinear_Metrics (table) ----
  output$svmLinear_Metrics <- renderTable({
    req(models$svmLinear)
    models$svmLinear$results[ which.min(models$svmLinear$results[, "RMSE"]), ]
  })

  # output svmLinear_Recipe (print) ----
  output$svmLinear_Recipe <- renderPrint({
    req(models$svmLinear)
    models$svmLinear$recipe
  })

  # output svmLinear_ModelSummary2 (print) ----
  output$svmLinear_ModelSummary2 <- renderPrint({
    req(models$svmLinear)
    print(models$svmLinear)
  })

  
  # METHOD * mlpML ---------------------------------------------------------------------------------------------------------------------------
  library(RSNNS)  #  <------ Declare any modelling packages that are needed (see Method List tab)

  # reactive getmlpMLRecipe ----
  getmlpMLRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$mlpML_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
  })

  # observeEvent mlpML_Go ----
  observeEvent(
    input$mlpML_Go,
    {
      method <- "mlpML"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getmlpMLRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(),
                              tuneLength = 25, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )

  observeEvent(
    input$mlpML_Load,
    {
      method  <- "mlpML"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$mlpML_Delete,
    {
      models[["mlpML"]] <- NULL
      gc()
    }
  )

  # output mlpML_ModelSummary0 (text) ----
  output$mlpML_ModelSummary0 <- renderText({
    description("mlpML")   # Use the caret method name here
  })

  # output mlpML_Metrics (table) ----
  output$mlpML_Metrics <- renderTable({
    req(models$mlpML)
    models$mlpML$results[ which.min(models$mlpML$results[, "RMSE"]), ]
  })

  # output mlpML_ModelPlots (plot) ----
  output$mlpML_ModelPlots <- renderPlot({
    req(models$mlpML)
    plot(models$mlpML)
  })

  # output mlpML_Recipe (print) ----
  output$mlpML_Recipe <- renderPrint({
    req(models$mlpML)
    models$mlpML$recipe
  })

  # output mlpML_ModelSummary2 (print) ----
  output$mlpML_ModelSummary2 <- renderPrint({
    req(models$mlpML)
    summary(models$mlpML$finalModel)
  })

  # output mlpML_Coef (print) ----
  output$mlpML_Coef <- renderTable({
    req(models$mlpML)
    co <- coef(models$mlpML$finalModel)
    as.data.frame(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)

  # METHOD * gaussprRadial ---------------------------------------------------------------------------------------------------------------------------
  
  
  # reactive getgaussprRadialRecipe ----
  getgaussprRadialRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$gaussprRadial_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent gaussprRadial_Go ----
  observeEvent(
    input$gaussprRadial_Go,
    {
      method <- "gaussprRadial"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getgaussprRadialRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(),
                              tuneLength = 25, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$gaussprRadial_Load,
    {
      method  <- "gaussprRadial"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$gaussprRadial_Delete,
    {
      models[["gaussprRadial"]] <- NULL
      gc()
    }
  )
  
  # output gaussprRadial_ModelSummary0 (text) ----
  output$gaussprRadial_ModelSummary0 <- renderText({
    description("gaussprRadial")   # Use the caret method name here
  })
  
  # output gaussprRadial_Metrics (table) ----
  output$gaussprRadial_Metrics <- renderTable({
    req(models$gaussprRadial)
    models$gaussprRadial$results[ which.min(models$gaussprRadial$results[, "RMSE"]), ]
  })
  
  # output gaussprRadial_ModelPlots (plot) ----
  output$gaussprRadial_ModelPlots <- renderPlot({
    req(models$gaussprRadial)
    plot(models$gaussprRadial)
  })
  
  # output gaussprRadial_Recipe (print) ----
  output$gaussprRadial_Recipe <- renderPrint({
    req(models$gaussprRadial)
    models$gaussprRadial$recipe
  })
  
  # output gaussprRadial_ModelSummary2 (print) ----
  output$gaussprRadial_ModelSummary2 <- renderPrint({
    req(models$gaussprRadial)
    summary(models$gaussprRadial$finalModel)
  })
  
  # output gaussprRadial_Coef (print) ----
  output$gaussprRadial_Coef <- renderTable({
    req(models$gaussprRadial)
    co <- coef(models$gaussprRadial$finalModel)
    as.data.frame(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)
  
  
  # METHOD * gaussprLinear ---------------------------------------------------------------------------------------------------------------------------
 
  # reactive getgaussprLinearRecipe ----
  getgaussprLinearRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$gaussprLinear_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
  })
  
   # observeEvent gaussprLinear_Go ----
  observeEvent(
    input$gaussprLinear_Go,
    {
      method <- "gaussprLinear"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getgaussprLinearRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(),
                              tuneLength = 25, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )

  observeEvent(
    input$gaussprLinear_Load,
    {
      method  <- "gaussprLinear"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )

  observeEvent(
    input$gaussprLinear_Delete,
    {
      models[["gaussprLinear"]] <- NULL
      gc()
    }
  )

  # output gaussprLinear_ModelSummary0 (text) ----
  output$gaussprLinear_ModelSummary0 <- renderText({
    description("gaussprLinear")   # Use the caret method name here
  })

  # output gaussprLinear_Metrics (table) ----
  output$gaussprLinear_Metrics <- renderTable({
    req(models$gaussprLinear)
    models$gaussprLinear$results[ which.min(models$gaussprLinear$results[, "RMSE"]), ]
  })

  # output gaussprLinear_ModelPlots (plot) ----
  output$gaussprLinear_ModelPlots <- renderPlot({
    req(models$gaussprLinear)
    plot(models$gaussprLinear)
  })

  # output gaussprLinear_Recipe (print) ----
  output$gaussprLinear_Recipe <- renderPrint({
    req(models$gaussprLinear)
    models$gaussprLinear$recipe
  })

  # output gaussprLinear_ModelSummary2 (print) ----
  output$gaussprLinear_ModelSummary2 <- renderPrint({
    req(models$gaussprLinear)
    summary(models$gaussprLinear$finalModel)
  })

  # output gaussprLinear_Coef (print) ----
  output$gaussprLinear_Coef <- renderTable({
    req(models$gaussprLinear)
    co <- coef(models$gaussprLinear$finalModel)
    as.data.frame(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)
  
  # METHOD * rf ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getrfRecipe ----
  getrfRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$rf_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
  })
  
   # observeEvent rf_Go ----
  observeEvent(
    input$rf_Go,
    {
      method <- "rf"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getrfRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(),
                              tuneLength = 25, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )

  observeEvent(
    input$rf_Load,
    {
      method  <- "rf"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )

  observeEvent(
    input$rf_Delete,
    {
      models[["rf"]] <- NULL
      gc()
    }
  )

  # output rf_ModelSummary0 (text) ----
  output$rf_ModelSummary0 <- renderText({
    description("rf")   # Use the caret method name here
  })

  # output rf_Metrics (table) ----
  output$rf_Metrics <- renderTable({
    req(models$rf)
    models$rf$results[ which.min(models$rf$results[, "RMSE"]), ]
  })

  # output rf_ModelPlots (plot) ----
  output$rf_ModelPlots <- renderPlot({
    req(models$rf)
    plot(models$rf)
  })

  # output rf_Recipe (print) ----
  output$rf_Recipe <- renderPrint({
    req(models$rf)
    models$rf$recipe
  })

  # output rf_ModelSummary2 (print) ----
  output$rf_ModelSummary2 <- renderPrint({
    req(models$rf)
    summary(models$rf$finalModel)
  })

  # output rf_Coef (print) ----
  output$rf_Coef <- renderTable({
    req(models$rf)
    co <- coef(models$rf$finalModel)
    as.data.frame(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)
  
  # METHOD * svmRadialCost ---------------------------------------------------------------------------------------------------------------------------

  # reactive getsvmRadialCostRecipe ----
  getsvmRadialCostRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$svmRadialCost_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
   })



  # observeEvent svmRadialCost_Go ----
  observeEvent(
    input$svmRadialCost_Go,
    {
      method <- "svmRadialCost"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getsvmRadialCostRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(),
                              tuneLength = 25, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )

  observeEvent(
    input$svmRadialCost_Load,
    {
      method  <- "svmRadialCost"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )

  observeEvent(
    input$svmRadialCost_Delete,
    {
      models[["svmRadialCost"]] <- NULL
      gc()
    }
  )

  # output svmRadialCost_ModelSummary0 (text) ----
  output$svmRadialCost_ModelSummary0 <- renderText({
    description("svmRadialCost")   # Use the caret method name here
  })

  # output svmRadialCost_Metrics (table) ----
  output$svmRadialCost_Metrics <- renderTable({
    req(models$svmRadialCost)
    models$svmRadialCost$results[ which.min(models$svmRadialCost$results[, "RMSE"]), ]
  })

  # output svmRadialCost_ModelPlots (plot) ----
  output$svmRadialCost_ModelPlots <- renderPlot({
    req(models$svmRadialCost)
    plot(models$svmRadialCost)
  })

  # output svmRadialCost_Recipe (print) ----
  output$svmRadialCost_Recipe <- renderPrint({
    req(models$svmRadialCost)
    models$svmRadialCost$recipe
  })

  # output svmRadialCost_ModelSummary2 (print) ----
  output$svmRadialCost_ModelSummary2 <- renderPrint({
    req(models$svmRadialCost)
    summary(models$svmRadialCost$finalModel)
  })

  # output svmRadialCost_Coef (print) ----
  output$svmRadialCost_Coef <- renderTable({
    req(models$svmRadialCost)
    co <- coef(models$svmRadialCost$finalModel)
    as.data.frame(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)
  
  # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------

  
  
})
