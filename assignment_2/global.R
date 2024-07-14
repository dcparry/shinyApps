# DATA423 assignment 2
# Dianne Parry
# global file for loading and preprocessing the data

#-------------------------------------------------------------------------------
# libraries

library(shiny)
library(shinythemes) 
library(tidyverse)
library(visdat) 
library(vcd)
library(corrgram) # correlation
library(shinyWidgets) # grouping
library(GGally) # pairs
library(shinycssloaders) # spinner
library(DT) # data table
library(naniar) # missing values
library(car) 
library(rpart) # predict missingness
library(rpart.plot) # predict missingness
library(missRanger) # for imputing missing vars for feature importance
library(randomForest) # for feature importance
library(recipes) 
library(caret) 
library(glmnet) 
library(plotly)
library(ggrepel) # label outliers on boxplot
library(patchwork) # display several plots in one


#-------------------------------------------------------------------------------
# subsets

# load raw data
dat <- read.csv("Ass2Data.csv", header = TRUE, na.strings = c("NA", "N/A"), stringsAsFactors = TRUE)

# create index so can get data back to the original order
dat$index <- 1:nrow(dat) 

# change variables to factors (govern_type, healthcare_basis, obs_type)
dat[, c(2, 12, 15)] <- lapply(dat[, c(2, 12, 15)], factor)

# set order of ordinal variables
order_govern <- c("STABLE DEM", "UNSTABLE DEM", "DICTATORSHIP", "OTHER")
order_hcbasis <- c("FREE", "INSURANCE", "PRIVATE")

# apply ordering of ordinal variable
dat$GOVERN_TYPE <- factor(dat$GOVERN_TYPE, levels = order_govern)
dat$HEALTHCARE_BASIS <- factor(dat$HEALTHCARE_BASIS, levels = order_hcbasis)

#-------------------------------------------------------------------------------
# ggplot default settings

theme_shiny <- function(base_size = 16) {
  theme_bw(base_size = base_size) %+replace%
    theme(
      plot.title = element_text(size = rel(1.2), face = "bold", margin = ggplot2::margin(0,0,5,0), hjust = 0.5),
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

plot_colours <- c( "#4472C4", "#A5A5A5", "#5B9BD5", "#264478", "#636363", "#255E91")

#-------------------------------------------------------------------------------
# deal with missing value placeholders

# create copy of raw data in case dat to be re-used
data <- dat

# Placeholders: replace numeric missing values with NA
data[data == -99] <- NA

# Placeholders: convert categorical variable from -- to NA (government type)
data$GOVERN_TYPE[data$GOVERN_TYPE == "--"] <- NA

# convert nominal values that are not applicable from missing to 0
# (where healthcare basic = free healthcare cost equals 0)
data$HEALTHCARE_COST_SHADOW <- as.numeric(is.na(data$HEALTHCARE_COST))
data$HEALTHCARE_COST[is.na(data$HEALTHCARE_COST)] <- 0

#-------------------------------------------------------------------------------
# data without NAs

data_no_NAs <- na.omit(data)

#-------------------------------------------------------------------------------
# subsets

# numeric columns
numerics <- data %>% 
  dplyr::select(3:11, 13:14)

# categorical columns
categoricals <- data %>% 
  dplyr::select(1:2, 12)

# training data
train <- data %>% 
  filter(OBS_TYPE == "Train")

# testing data
test <- data %>% 
  filter(OBS_TYPE == "Test") 


#-------------------------------------------------------------------------------
# groupings

missing_groups <- list(
  "Numeric" = names(numerics),
  "Categorical" = names(categoricals)
)

tabPlot_groups <- list(
  "Group 1" = c(names(data[1:7])),
  "Group 2" = c(names(data[8:14]))
)

pairs_groups <- list(
  "Group 1" = c(names(data[2:7])),
  "Group 2" = c(names(data[8:14]))
)
  
#-------------------------------------------------------------------------------
# choices

choices_sort <- c(names(data[1:16]))
choices_categorical <- c(names(categoricals[2:3]))
choices_numerics <- c(names(numerics))
choices_pairs <- c(names(data[2:14]))
choices_outliers <- c(names(numerics[1, 4:10]))
choices_errors <- names(data)[c(2, 3, 8, 11)]
choices_shadow <- c(names(data[2:11]))

#-------------------------------------------------------------------------------
# functions

pMiss <- function(x){ sum(is.na(x))/length(x)*100 }

#-------------------------------------------------------------------------------
# index lists

# it was decided to use indices rather than conditional filters for avoiding errors where the feature is no longer present; 
# but aware this can introduce human data entry errors. These indices may include both test and train outliers (they were copied
# from the outliers box plot caption in the shiny app which represented the complete dataset).

error_indices <- list(
  GOVERN_TYPE = c(27, 49, 76, 103, 170, 193, 230, 261, 300, 328, 342, 370, 399, 432), # indices where GOVERN_TYPE = NA
  GDP = c(17, 36, 60, 75, 77, 107, 109, 143, 268, 292, 295, 322, 372, 392, 417), # GDP lower outliers with IQR M = 2.4
  POPULATION = c(20, 51, 63, 91, 93, 195, 197, 242, 283, 338, 374, 386, 415), # indices where POP_DENSITY > POPULATION
  VAX_RATE = c(144, 145, 151, 303, 309, 369, 402, 405) # indices where VAX_RATE = 14
)

outlier_indices <- list(
  POPULATION = c(142, 150, 180, 187, 290),
  AGE50_PROPTN = c(92),
  POP_DENSITY = c(12, 227, 386, 469, 77, 179, 191, 302, 429),
  GDP = c(17, 36, 60, 75, 77, 107, 109, 117, 143, 268, 292, 295, 322, 372, 392, 417, 96, 395),
  INFANT_MORTALITY = c(116, 208, 329, 442, 304, 377),
  DOCS = c(82, 292),
  VAX_RATE = c(17, 36, 60, 107, 109, 143, 144, 145, 151, 233, 303, 309, 322, 369, 372, 392, 402, 405, 417, 5)
)


