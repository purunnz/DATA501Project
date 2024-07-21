# Define the custom function
shapiro_wilk_test <- function(data) {
  # Perform the Shapiro-Wilk test
  test_result <- shapiro.test(data)
  
  # Print the result
  print(test_result)
  
  # Return the test result
  return(test_result)
}

# 1: Load your data
sample_data <- c(4.2, 5.3, 6.1, 7.4, 8.0, 4.8, 5.9, 6.2, 7.5, 5.7)


# 2: Perform the Shapiro-Wilk test
shapiro_test_result <- shapiro_wilk_test(sample_data)


# 3: Print the result
print(shapiro_test_result)


