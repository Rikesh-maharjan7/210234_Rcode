# Load libraries
library(tidyverse)
library(caret)
library(ggplot2)

# Load the dataset
df <- read.csv("~/Desktop/Analysis/data.csv")  # Adjust the path to your dataset

# Ensure the dataset has the required columns
df <- df %>%
  select(x1, x2, x3, x4, x5)  # Ensure only the relevant columns are included

# Split the data into training and testing sets (70% training, 30% testing)
set.seed(100)
trainIndex <- createDataPartition(df$x2, p = 0.7, list = FALSE)
trainData <- df[trainIndex, ]
testData <- df[-trainIndex, ]

# Task 2: Define Uniform Priors
set.seed(123)
x4_prior <- runif(10000, 0.8, 1.0)    # Uniform prior for x4
bias_prior <- runif(10000, -0.2, 0.0) # Uniform prior for bias

# Task 3: Simulate and Apply Rejection ABC
simulate_model <- function(x4, bias, X) {
  # Ensure X matches dimensions (2 columns: x4 and bias)
  X %*% c(x4, bias)
}

# Design Matrix X (Model 1: x4 and bias)
X <- cbind(trainData$x4, 1)  # x4 and a constant column for bias

# Observed response vector (Y)
y_actual <- trainData$x2  # Replace with actual observed responses

# Simulated Responses
y_simulated <- sapply(1:10000, function(i) simulate_model(x4_prior[i], bias_prior[i], X))

# Calculate Distance Metric (Euclidean distance between simulated and observed y)
distances <- apply(y_simulated, 2, function(y_sim) sqrt(sum((y_actual - y_sim)^2)))

# Select Samples Below a Threshold
epsilon <- 0.5
accepted_samples <- which(distances < epsilon)
posterior_x4 <- x4_prior[accepted_samples]
posterior_bias <- bias_prior[accepted_samples]

# Task 4: Plot Posterior Distributions
if (length(posterior_x4) > 1 && length(posterior_bias) > 1) {
  par(mfrow = c(1, 3))
  
  # Posterior of x4
  hist(posterior_x4, main = "Posterior of x4", xlab = "x4", breaks = 30)
  
  # Posterior of Bias
  hist(posterior_bias, main = "Posterior of Bias", xlab = "Bias", breaks = 30)
  
  # Joint Posterior
  plot(posterior_x4, posterior_bias, main = "Joint Posterior", xlab = "x4", ylab = "Bias")
} else {
  message("Not enough accepted samples for posterior distributions. Try increasing epsilon.")
}
epsilon <- 1.0  # Increase threshold for more accepted samples



y_actual <- c(-2.09852314, -1.87474148, -1.68982445, -0.91080702, 1.92145103,
              2.21242845, 0.83027204, -0.90434020, -0.27948245, -1.19134271, -1.67577896)

x4_prior <- runif(10000, 0.5, 1.5)   # Broaden range for x4
bias_prior <- runif(10000, -0.5, 0.5) # Broaden range for bias

hist(distances, main = "Distance Distribution", xlab = "Distance")

# Define Priors (Broaden Range)
x4_prior <- runif(10000, 0.5, 1.5)
bias_prior <- runif(10000, -0.5, 0.5)

# Simulated Responses
y_simulated <- sapply(1:10000, function(i) simulate_model(x4_prior[i], bias_prior[i], X))

# Calculate Distance Metric
distances <- apply(y_simulated, 2, function(y_sim) sqrt(sum((y_actual - y_sim)^2)))

# Select Samples Below a Threshold
epsilon <- 2.0  # Increase threshold
accepted_samples <- which(distances < epsilon)

# Handle Cases with No Accepted Samples
if (length(accepted_samples) > 0) {
  posterior_x4 <- x4_prior[accepted_samples]
  posterior_bias <- bias_prior[accepted_samples]
  
  # Plot Posterior Distributions
  par(mfrow = c(1, 3))
  hist(posterior_x4, main = "Posterior of x4", xlab = "x4", breaks = 30)
  hist(posterior_bias, main = "Posterior of Bias", xlab = "Bias", breaks = 30)
  plot(posterior_x4, posterior_bias, main = "Joint Posterior", xlab = "x4", ylab = "Bias")
} else {
  message("Still not enough accepted samples. Try further increasing epsilon or adjusting priors.")
}


