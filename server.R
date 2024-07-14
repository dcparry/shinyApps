# DATA423 assignment 1
# Dianne Parry
# server file 

library(shiny)
source("global.R")
library(naniar)
library(visdat)
library(car)

# server
function(input, output, session) {
  
  # histogram of categorical variables
  output$barChart <- renderPlot({
    ggplot(data = dat, aes_string(x = input$var, fill = input$var)) +
      geom_bar(width = 0.5, colour = "black") +
      theme_shiny() +
      labs(title = paste("Distribution of", input$var),
           y = "Count")
  })
  
  #box plots of sensor values by categorical variables
  output$boxPlots <- renderPlot({
    
    ggplot(sensor_dat_long, aes_string(x = input$var, y = "value", fill = input$var)) +
      geom_boxplot() +
      theme_shiny() +
      labs(title = paste("Boxplot of Sensor Values for", input$var),
           y = "Value") +
      scale_y_continuous(limits = c(-100,400))
  })
  
  # missing data chart
  selected_columns <- reactive({
    selected_groups <- input$columns
    selected_vars <- unlist(missing_groups[selected_groups])
    dat[, selected_vars, drop = FALSE]
  })

  output$visMiss <- renderPlot({
    vis_miss(selected_columns()) +
      ggtitle("Missing Value Distribution") +
      theme(
        plot.title = element_text(size = rel(1.8), face = "bold", margin = margin(0,0,5,0), hjust = 0.5),
        axis.title = element_text(size = rel(1.5), face = "bold"),
        axis.text.x = element_text(angle = 90),
        axis.text = element_text(size = rel(1.2)),
        legend.title = element_text(size = rel(1.5), face = "bold"),
        legend.text = element_text(size = rel(1.2), face = "bold"),
        legend.key = element_rect(fill = "transparent", colour = NA),
        legend.key.size = unit(1.5, "lines"),
        legend.background = element_rect(fill = "transparent", colour = NA))
  })
  
  # Cumulative sum of missing data
  output$visMiss2 <- renderPlot(
    gg_miss_var_cumsum(dat) +
      labs(title = "Cumulative Sum of Missing Values",
           x = "Variable",
           y = "Cumulative sum") +
      theme_shiny() +
      theme(
      axis.text.x = element_text(angle = 90))
  )

  # data table
  output$dataTable <- renderDT(
    dat, filter = 'top', options = list(
      pageLength = 10)
    )
  
  # line chart of sensor values over time
  output$sensorPlot <- renderPlot({
    
    selected_sensors <- input$sensorSelect
    filtered_sensor_data <- sensor_dat_long[sensor_dat_long$sensor %in% selected_sensors & 
                                              sensor_dat_long$Date >= input$dateSelect[1] &
                                              sensor_dat_long$Date <= input$dateSelect[2], ] 
      
    ggplot(filtered_sensor_data, aes(x = Date, y = value, color = sensor)) +
    geom_line() +
    labs(title = "Sensor Data Over Time",
         x = "Date",
         y = "Sensor value") +
      theme_shiny()
  })
  

  # box plot of numeric variables
  output$boxPlot <- renderPlot({
    
    if ("Centre" %in% input$transformSelect) {
      numerics <- scale(numerics, center = TRUE, scale = FALSE)
    }

    if ("Scale" %in% input$transformSelect) {
      numerics <- scale(numerics, center = FALSE, scale = TRUE)
    }

    car::Boxplot(
      y = numerics,
      ylab = NA,
      use.cols = TRUE,
      notch = FALSE,
      varwidth = FALSE,
      horizontal = FALSE,
      col = "#619CFF",
      main = "Boxplots of Numeric Data",
      outline = input$outliers,
      range = input$range,
      id = ifelse(input$outliers, list(n = Inf, location = "avoid"), FALSE))
  
  })
  
  # box plot of sensor values by categorical variables
  output$boxPlot2 <- renderPlot({
    
    sensor_data <- sensor_cols
    
    for (col in colnames(sensor_data)) {
      if (is.numeric(sensor_data[[col]])) {
        sensor_data[[col]] <- scale(sensor_data[[col]], center = TRUE, scale = TRUE)
      }
    }
    
    plot_data_long <- reshape2::melt(sensor_data)

    plot_data_long$Category <- dat[[input$categorySelect]]

    ggplot(plot_data_long, aes(x = Category, y = value, fill = variable)) +
      geom_boxplot() +
      labs(title = paste("Boxplots of Sensor Data by", input$categorySelect), 
           y = NULL, 
           fill = "Sensor") +
      theme_shiny()
  })
  
  # caption for box plot
  output$caption <- renderText({
    "Values have been centred and scaled."
  })
  
  
  # rising value chart
  output$risingValues <- renderPlot({
  selected_groups <- input$variableGroups
  transform_options <- input$transformSelect2

  if (length(selected_groups) == 0) {
    return(NULL)
  }

  selected_columns <- unlist(numeric_groups[selected_groups])
  sensor_cols <- dat[, selected_columns, drop = FALSE]

  if ("Centre" %in% transform_options) {
    sensor_cols <- scale(x = sensor_cols, center = TRUE, scale = FALSE)
  }

  if ("Scale" %in% transform_options) {
    sensor_cols <- scale(x = sensor_cols, center = FALSE, scale = TRUE)
  }

  for (col in 1:ncol(sensor_cols)) {
    sensor_cols[, col] <- sensor_cols[order(sensor_cols[, col]), col]
  }

  mypalette <- rainbow(ncol(sensor_cols))


  layout(matrix(c(1, 2), nrow = 1), widths = c(2, 1))
  par(mar = c(4, 4, 2, 2) + 0.1)
  #par(mfrow = c(1, 2), mar = c(2, 2, 2, 2) + 0.1)

  matplot(
    x = seq(1, 100, length.out = nrow(sensor_cols)),
    y = sensor_cols,
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

  legend("topleft", legend = colnames(sensor_cols), col = mypalette, lty = 1, lwd = 1, ncol = 2)
})
  
  # summary 
  output$summaryOutput <- renderUI({
    HTML({
      summary_text <- capture.output({
        dat %>%
          summarytools::dfSummary(col.widths = c(10, 80, 150, 120, 120, 180, 220)) %>%
          summarytools::view(method = "render")
      })
      paste(summary_text, collapse = "\n")
    })
  })

  # mosaic plot
  output$mosaicPlot <- renderPlot({
    selected_vars <- input$variableSelect
    
    if (length(selected_vars) > 0) { # so if no variables selected, doesn't have ugly error message
      formula <- as.formula(paste("~", paste(selected_vars, collapse = " + ")))
      vcd::mosaic(formula, data = dat,
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

  # correlation plot
  selected_data <- reactive({
    if (input$showOutliers) {
      return(numerics)
    } else {
      return(no_outliers)
    }
  })

  output$corrgramPlot <- renderPlot({
    req(nrow(selected_data()) > 0)
    corrgram(selected_data(),
             order = input$group,
             abs = input$abs, # doesn't seem to work? 
             cor.method = input$corrMeth,
             main = "Correlation of Numeric Data",
             lower.panel= panel.shade,
             upper.panel=panel.cor,
             labels = labels)
   })
  

  # table plot
  output$tabPlot <- renderPlot({
    selected_groups <- input$group_selector
    selected_vars <- unlist(tabPlot_groups[selected_groups])

    updateSelectInput(
      session,
      "sort",
      choices = c(selected_vars),
      selected = input$sort
    )

    filtered_data <- data2[, c(selected_vars, input$sort), drop = FALSE]
    tabplot::tableplot(filtered_data, sortCol = input$sort, scales = "lin")
    
  })
  
  # pairs plot
  output$pairsPlot <- renderPlot({
    
    selected_groups <- input$groupSelect
    selected_vars <- unlist(pairs_groups[selected_groups])
    
    filtered_data <- data2[, c(selected_vars), drop = FALSE]
    
    ggpairs(filtered_data)
    
  })



  
  
}

