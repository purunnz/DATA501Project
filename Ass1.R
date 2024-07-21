# Load library 
library(ggplot2)
library(testthat)

# Define the custom function
# A.at least two inputs (1 required and 1 default/optional)
# shapiro_wilk_test <- function(data, disp_qqplot = FALSE) {
shapiro_wilk_test <- function(...) {
  
  # B.Input validation
  # Check the number of parameters
  args <- list(...)

  if (length(args) < 1 || length(args) > 2) {
    stop("The function requires 1 or 2 parameters: data (required) and disp_qqplot (optional).")
  }

  # Assign arguments to variables
  data <- args[[1]]
  disp_qqplot <- ifelse(length(args) == 2, args[[2]], FALSE)
  
  # ---------------------------------------------------------
  if (is.null(data)) stop("Input data is null")

  # Perform the Shapiro-Wilk test
  test_result <- shapiro.test(data)
  
  # Print the result
  print(test_result)
  
  # qq plot 
  if (disp_qqplot) {
    # QQplot of normally distributed values
    qqnorm(data)
    # Add qqline to plot
    qqline(data, col = "darkgreen")
    
  }
  
  # Return the test result
  return(test_result)
}

##--------------------------------------------------------------------
# Basic/simple Tests
# Test 1: Normal data without QQ-plot
sample_data1 <- c(4.2, 5.3, 6.1, 7.4, 8.0, 4.8, 5.9, 6.2, 7.5, 5.7)
result1 <- shapiro_wilk_test(sample_data1)
result1 <- shapiro_wilk_test(sample_data1, disp_qqplot = TRUE)

# Test 2: Normal data with QQ-plot
set.seed(500)
sample_data2 <- rnorm(5000)
result2 <- shapiro_wilk_test(sample_data2, disp_qqplot = TRUE)

