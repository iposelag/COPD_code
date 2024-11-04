#!/bin/Rscript
###############################################################################
######################## library w/ common functions ##########################
###############################################################################
## command: 
## output: 

## ----------------------------------------------------------------------------------------------------------------------------------------
# Required packages
library(dplyr)

## ----------------------------------------------------------------------------------------------------------------------------------------
# Functions to make labels consistent
# Function to rename models name
rename_classifier <- function(input_classifier) {
  classifier_mapping <- c(
    rf = "RF",
    svm_r = "SVM-rad",
    svm_p = "SVM-poly",
    knn = "kNN",
    xgb_bundle = "XGB",
    glm = "GLM"
  )

  converted_classifier <- case_when(
    input_classifier %in% names(classifier_mapping) ~ classifier_mapping[input_classifier],
    TRUE ~ input_classifier # Keep the original value if not in the mapping
  )

  return(converted_classifier)
}

convert_classifiers_column <- function(input_df) {
  converted_df <- input_df %>%
    mutate(classifier = case_when(
      classifier == "rf" ~ "RF",
      classifier == "svm_r" ~ "SVM-rad",
      classifier == "svm_p" ~ "SVM-poly",
      classifier == "knn" ~ "kNN",
      classifier == "xgb" ~ "XGB", 
      classifier == "glm" ~ "GLM",
      TRUE ~ classifier # Keep the original value if no conditions are met
    ))
    return(converted_df)
    }

convert_input_column <- function(input_df) {
  converted_df <- input_df %>%
    mutate(ml_input = case_when(
      ml_input == "dea" ~ "DEA",
      ml_input == "mrmr/mrmr_103" ~ "mRMR",
      ml_input == "data_driven" ~ "data-driven",
      ml_input == "data_driven_guildify" ~ "data-driven GUILDify",
      ml_input == "data_driven_guildify_functional_based" ~ "data-driven GUILDify functional based",
      ml_input == "data_driven_omnipath" ~ "data-driven OmniPath",
      ml_input == "disgenet_curated" ~ "COPD-related curated",
      ml_input == "disgenet_curated_guildify" ~ "COPD-related curated GUILDify",
      ml_input == "disgenet_curated_guildify_functional_based" ~ "COPD-related curated GUILDify functional based",
      ml_input == "disgenet_curated_omnipath" ~ "COPD-related curated OmniPath",
      ml_input == "disgenet_entire_list" ~ "COPD-related entire list",
      ml_input == "omnipath_intersection" ~ "OmniPath intersection",
      ml_input == "omnipath_union" ~ "OmniPath union",
      TRUE ~ ml_input  # Keep the original value if no conditions are met
    ))
  return(converted_df)
}

convert_columns_to_numeric <- function(data, column_names) {
  for (column_name in column_names) {
    # Convert the specified column to numeric, handling NA values
    data[[column_name]] <- as.numeric(data[[column_name]], na.rm = TRUE)
    
    # Check for any issues
    if (any(is.na(data[[column_name]]))) {
      warning(paste("Some values in column", column_name, "couldn't be converted to numeric."))
    }
  }
  
  # Return the modified data frame
  return(data)
}

## ----------------------------------------------------------------------------------------------------------------------------------------
# Functions to define consistent colors
genes_list_colors <- c("DEA" = "#bc6c25",
                  "mRMR" = "#f6bd60",
                  "data-driven" = "#19787F",
                  "data-driven OmniPath" = "#8EBCBF",
                  "data-driven GUILDify" = "#d2e7d6",
                  "data-driven GUILDify functional based" = "#9b2226",
                  "COPD-related curated" = "#325486",
                  "COPD-related curated GUILDify" = "#cfe2f3",
                  "COPD-related curated GUILDify functional based" = "#FF6600",
                  "COPD-related curated OmniPath" = "#9AAAC3",
                  "COPD-related entire list" = "#bea9de",
                  "OmniPath intersection" = "#FF9999",
                  "OmniPath union" = "#ffbaba")
ml_models_colors <- c("RF" = "#6b3e26",
               "SVM-rad" = "#ffc5d9",
               "SVM-poly" = "#c2f2d0",
               "kNN" = "#ffcb85",
               "GLM" = "#fdf5c9",
               "XGB" = "#ff6f69")
dis_condition <- c("CTRL" = "#ffdcdb", "COPD" = "#91a8d0")
GOLD_stage <- c("0-At Risk" = "#f6e0b5", "1-Mild COPD" = "#eea990",
                "2-Moderate COPD" = "#aa6f73", "3-Severe COPD" = "#a39193",
                "4-Very Severe COPD" = "#66545e")
sex <- c("1-Male" = "#e1f7d5", "2-Female" = "#c9c9ff")
smoker <- c("1-Current" = "#83adb5", 
            "2-Ever (>100)" = "#c7bbc9",
            "3-Never"="#5e3c58")
age <-  c("(27,35]" = "#ece6ff", "(35,45]" = "#efbbff",
          "(45,55]" = "#d896ff", "(55,65]" = "#be29ec",
         "(65,75]" = "#800080", "(75,91]" = "#660066")
pneumocystis_colonization <- c("Negative" = "#b1cc74",
                              "Positive" = "#eec643")
platform_id <- c("GPL14550" = "#918450",
                 "GPL6480" = "#a41623")