# DATA423 assignment 1
# Dianne Parry
# global file for loading and preproccessing the data

library(tidyverse)
library(visdat)
library(lubridate)
library(vcd)
library(corrgram)
library(shinyWidgets) # grouping
library(GGally) # pairs
library(shinycssloaders) # spinner

dat <- read.csv("Ass1Data.csv", header = TRUE, stringsAsFactors = TRUE)

# change date variable to date type
dat$Date <- as.Date(dat$Date)

# change variables to factors
dat[, c(3, 5:14)] <- lapply(dat[, c(3, 5:14)], factor)

# set order of ordinal variables
order_priority <- c("Low", "Medium", "High")
order_price <- c("Cheap", "Fair", "Expensive")
order_temp <- c("Cold", "Warm", "Hot")
order_speed <- c("Slow", "Medium", "Fast")
order_duration <- c("Short", "Long", "Very Long")

# apply ordering or ordinal variable
dat$Priority <- factor(dat$Priority, levels = order_priority)
dat$Price <- factor(dat$Price, levels = order_price)
dat$Temp <- factor(dat$Temp, levels = order_temp)
dat$Speed <- factor(dat$Speed, levels = order_speed)
dat$Duration <- factor(dat$Duration, levels = order_duration)

#convert Date back to character for ordering tabplot
data2 <- dat
data2$Date <- as.character(data2$Date)
data2 <- data2[order(data2$Date), ]

# set ggplot defaults?
theme_shiny <- function(base_size = 16) {
  theme_minimal(base_size = base_size) %+replace%
    theme(
      plot.title = element_text(size = rel(1.2), face = "bold", margin = margin(0,0,5,0), hjust = 0.5),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.title = element_text(size = rel(1), face = "bold"),
      axis.text = element_text(size = rel(0.8)),
      axis.line = element_line(color = "black"),
      legend.title = element_text(size = rel(1), face = "bold"),
      legend.text = element_text(size = rel(0.8), face = "bold"),
      legend.key = element_rect(fill = "transparent", colour = NA),
      legend.key.size = unit(1.5, "lines"),
      legend.background = element_rect(fill = "transparent", colour = NA)
    )
}

#plot_colours <- c( "#4472C4", "#A5A5A5", "#5B9BD5", "#264478", "#636363", "#255E91")

# pivot longer sensor data
sensor_dat_long <- dat %>%
  pivot_longer(cols = starts_with("sensor"), 
               names_to = "sensor", 
               values_to = "value")

sensor_dat_long$numeric_sensor <- as.numeric(gsub("sensor", "", sensor_dat_long$sensor))
sensor_dat_long$sensor <- fct_reorder(sensor_dat_long$sensor, sensor_dat_long$numeric_sensor)

# sensor only df
sensor_cols <- dat[15:44]  # select the definitely-continuous columns

# numeric columns
numerics <- dat %>% 
  dplyr::select(1, 15:44)

# df with no outliers
outlier_rows <- dat[dat$Operator == "XP" & rowSums(dat[, c(1,15:44)] > 300, na.rm = TRUE) > 0, ]
no_outliers <- dat %>%
  anti_join(outlier_rows, by = "ID") %>% 
  dplyr::select(1,15:44)


# choices lists
choices_categorical <- c(names(dat[3]), names(dat[5:14]))
choices_sensors <- names(dat[15:44])
choices_sort <- c(names(data2[1:14]))

# renaming sensors to S1 etc
labels <- c("Y", "S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "S9", "S10",
            "S11", "S12", "S13", "S14", "S15", "S16", "S17", "S18", "S19", "S20",
            "S21", "S22", "S23", "S24", "S25", "S26", "S27", "S28", "S29", "S30")

# groupings
tabPlot_groups <- list(
  "Group 1" = c(names(data2[1:14])),
  "Sensors 1-10" = c(names(data2[15:24])),
  "Sensors 11-20" = c(names(data2[25:34])),
  "Sensors 21-30" = c(names(data2[35:44]))
)

missing_groups <- list(
  "Numeric" = c(names(dat[1]), names(dat[15:44])),
  "Date" = c(names(dat[4])),
  "Factor" = c(names(dat[2:3]), names(dat[5:14]))
)

numeric_groups <- list(
  "Y" = names(dat[1]),
  "Sensors 1-10" = names(dat[15:24]),
  "Sensors 11-20" = names(dat[25:34]),
  "Sensors 21-30" = names(dat[35:44])  
)

pairs_groups <- list(
  "Y + Categoricals 1" = c(names(dat[1]), names(dat[3]), names(data2[5:9])),
  "Y + Categoricals 2" = c(names(dat[1]), names(data2[10:14])),
  "Y + Sensors 1-10" = c(names(dat[1]), names(data2[15:24])),
  "Y + Sensors 11-20" = c(names(dat[1]), names(data2[25:34])),
  "Y + Sensors 21-30" = c(names(dat[1]), names(data2[35:44]))
)

