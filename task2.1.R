# Load libraries
library(tidyverse)

# Load and filter dataset
df <- read.csv("D:/masters/statics/final/data.csv")
df <- df %>%
  select(x1, x2, x3, x4, x5) # Ensure required columns are selected

# Generate Model 1
generateModel1 <- function(df) {
  set.seed(100)
  ones <- rep(1, nrow(df)) # Column of ones for bias
  model <- cbind(df$x4, df$x3^2, ones) # Include x4, x3^2, and bias
  colnames(model) <- c("x4", "x3_squared", "bias") # Name columns
  return(model)
}

# Calculate Theta Hat
thetaHat <- function(model, y) {
  return(solve(t(model) %*% model) %*% t(model) %*% y)
}

# RSS Calculation
calculateRSS <- function(y, y_hat) {
  return(sum((y - y_hat)^2))
}

# Variance Calculation
calculateVariance <- function(N, rss) {
  return(rss / (N - 1))
}

# Log-Likelihood Calculation
calculateLogLikelihood <- function(N, variance, rss) {
  return(- (N / 2) * log(2 * pi) - (N / 2) * log(variance) - (1 / (2 * variance)) * rss)
}

# AIC Calculation
calculateAIC <- function(N, k, log_likelihood) {
  return(2 * k - 2 * log_likelihood)
}

# BIC Calculation
calculateBIC <- function(N, k, log_likelihood) {
  return(k * log(N) - 2 * log_likelihood)
}

# Generate Model 1 and Fit
model_1 <- generateModel1(df)
y <- df$x2 # Response variable

# Compute Theta Hat
theta_hat_1 <- thetaHat(model_1, y)
cat("Theta Hat for Model 1:\n")
print(theta_hat_1)

# Predictions
y_hat_1 <- model_1 %*% theta_hat_1

# Compute RSS
RSS_Model1 <- calculateRSS(y, y_hat_1)
cat("RSS for Model 1:", RSS_Model1, "\n")

# Compute Variance
N <- nrow(df)
Variance_Model1 <- calculateVariance(N, RSS_Model1)
cat("Variance for Model 1:", Variance_Model1, "\n")

# Compute Log-Likelihood
LogLikelihood_Model1 <- calculateLogLikelihood(N, Variance_Model1, RSS_Model1)
cat("Log-Likelihood for Model 1:", LogLikelihood_Model1, "\n")

# Compute AIC
K_Model1 <- ncol(model_1) # Number of parameters
AIC_Model1 <- calculateAIC(N, K_Model1, LogLikelihood_Model1)
cat("AIC for Model 1:", AIC_Model1, "\n")

# Compute BIC
BIC_Model1 <- calculateBIC(N, K_Model1, LogLikelihood_Model1)
cat("BIC for Model 1:", BIC_Model1, "\n")

# Generate QQ Plot for Model 1
errors_1 <- y - y_hat_1

# Create a data frame for the QQ plot
qq_data_1 <- data.frame(
  sample_quantiles = qqnorm(errors_1, plot.it = FALSE)$x,
  model_error = qqnorm(errors_1, plot.it = FALSE)$y
)

# Fit a polynomial regression model
degree <- 3  # Adjust the degree as needed
poly_model_1 <- lm(model_error ~ poly(sample_quantiles, degree), data = qq_data_1)

# Predict values for the polynomial line
qq_data_1$poly_fit <- predict(poly_model_1, qq_data_1)

# Generate the QQ plot with the polynomial line
ggplot(qq_data_1, aes(x = sample_quantiles, y = model_error)) +
  geom_point(color = "blue", size = 2) +  # QQ points
  geom_line(aes(y = poly_fit), color = "red", size = 1) +  # Polynomial curve
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +  # Straight QQ line
  labs(
    title = "QQ Plot: Distribution of Prediction Errors with Polynomial Fit (Model 1)",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_minimal()
