# Load library 
library(ggplot2)

# Define the custom function
shapiro_wilk_test <- function(data, dis_qqplot = FALSE) {
  
  # Perform the Shapiro-Wilk test
  test_result <- shapiro.test(data)
  
  # Print the result
  print(test_result)
  
  # qq plot 
  if (dis_qqplot) {
    # QQplot of normally distributed values
    qqnorm(data)
    # Add qqline to plot
    qqline(data, col = "darkgreen")
    
  }
  
  # Return the test result
  return(test_result)
}

# 1: Load data
sample_data <- c(4.2, 5.3, 6.1, 7.4, 8.0, 4.8, 5.9, 6.2, 7.5, 5.7)

set.seed(500)
# Create random normally distributed values
sample_datan <- rnorm(100)

# Create random uniformlly distributed values
sample_datau <- runif(100)


# 2: Perform the Shapiro-Wilk test & qqplot 
shapiro_test_result <- shapiro_wilk_test(sample_data, dis_qqplot = TRUE)
shapiro_test_result <- shapiro_wilk_test(sample_datan, dis_qqplot = TRUE)
shapiro_test_result <- shapiro_wilk_test(sample_datau, dis_qqplot = TRUE)



