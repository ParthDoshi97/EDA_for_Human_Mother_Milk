
## This File has Various function required in Data analysis of Nutrieshiled Data ##

### Function to check and install missing libraries ###
check_and_install_libraries <- function(libs) {
  missing_libs <- libs[!(libs %in% installed.packages()[,"Package"])]
  
  if (length(missing_libs) > 0) {
    message("Installing missing libraries: ", paste(missing_libs, collapse = ", "))
    install.packages(missing_libs, dependencies = TRUE)
  }
  
  for (lib in libs) {
    library(lib, character.only = TRUE)
  }
}

###### Function for Normalization #######

# Define a custom function to scale the data using the "range" method
scale_data <- function(data) {
  preprocessing_model <- preProcess(data, method = "BoxCox")
  scaled_data <- predict(preprocessing_model, newdata = data)
  return(scaled_data)
}

####### Functions For split data into mother and Infant ############
split_and_create_dataframes <- function(data) {
  # Split the data into a list based on the values in the 'Type' column
  split_data <- split(data, data$Type)
  
  # Create an empty list to store the data frames
  data_frames_list <- list()
  
  # Extract the data name from the 'data' argument
  data_name <- deparse(substitute(data))
  
  for (type in names(split_data)) {
    # Generate a variable name for the data frame based on the 'data_name' and 'Type' value
    var_name <- paste(data_name, "_", type, sep = "")
    
    # Assign the data frame for the current 'Type' to the list
    data_frames_list[[var_name]] <- split_data[[type]]
  }
  
  # Convert the list to individual variables in the global environment
  list2env(data_frames_list, envir = globalenv())
}

##### Function To convert Month Names to Zero #####
convert_month_to_zero <- function(data_frame) {
  data_frame$Month <- ifelse(data_frame$Month %in% c("CEN", "RBW"), "0", data_frame$Month)
  return(data_frame)
}

###### Functions For split data into month ############

split_and_create_dataframe_month <- function(data) {
  # Split the data into a list based on the values in the 'Type' column
  split_data <- split(data, data$Month)
  
  # Create an empty list to store the data frames
  data_frames_list <- list()
  
  # Extract the data name from the 'data' argument
  data_name <- deparse(substitute(data))
  
  for (month in names(split_data)) {
    # Generate a variable name for the data frame based on the 'data_name' and 'Type' value
    var_name <- paste(data_name, "_", month, sep = "")
    
    # Assign the data frame for the current 'Type' to the list
    data_frames_list[[var_name]] <- split_data[[month]]
  }
  
  # Convert the list to individual variables in the global environment
  list2env(data_frames_list, envir = globalenv())
}

##### Function To calculate Correlation Matrix #####
library(ggplot2)
library(ggcorrplot)
library(psych)  # Load the 'psych' package for corr.test()

calculate_and_visualize_correlation <- function(data, method = "pearson", title) {
  # Select only numeric variables from the data frame
  numeric_data <- data[, sapply(data, is.numeric)]
  
  # Calculate correlation matrix using the specified method
  correlationMatrix <- cor(numeric_data, method = method)
  
  # Calculate p-values for the correlations
  p_values <- corr.test(numeric_data, method = method)$p
  
  
  # Visualize the correlation matrix using ggcorrplot
  correlationPlot <- ggcorrplot(correlationMatrix,
             title = title,
             ggtheme = theme_classic,
             colors = c("red", "white", "forestgreen"),
             hc.order = FALSE,
             type = "lower",
             lab = FALSE,
             lab_size = 5.0,
             #p.mat = p_values
  )
  
  # Print the correlation plot in R Markdown
  return(correlationPlot)
  
  # Save the correlation matrix as a data frame
  #saved_correlation_matrix <- as.data.frame(correlationMatrix)
  
  # Return the correlation matrix as a data frame
  #return(saved_correlation_matrix)
}

  
  ## Function to Calculate the Root Mean Squared Error (RMSE) for each SCFA ###

calculate_mse_and_rmse <- function(test_data, predictions) {
  mse <- ((test_data[, c("Propionic","Isobutyric","Butyric","X2.Me.butyric","Isovaleric","Valeric","Caproic","Heptanoic","Valine","Leucine","Isoleucine")] - predictions)^2)
  
  # Calculate the Root Mean Squared Error (RMSE) for each SCFA
  rmse <- sqrt(colMeans(mse))
  
  return(list(mse = mse, rmse = rmse))
}

# extar r squared value
extract_r_squared <- function(model) {
  r_squared_values <- c()
  
  for (response_name in names(model$call$formula[[2]])) {
    r_squared_values[response_name] <- summary(model)$`Response `[[response_name]]$r.squared
  }
  
  return(r_squared_values)
}

