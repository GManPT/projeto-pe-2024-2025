# Set parameters for Weibull distribution
lambda <- 18  # scale parameter
k <- 7        # shape parameter

# 1. Calculate expected value using gamma function
# E(X) = lambda * Gamma(1 + 1/k)
theoretical_mean <- lambda * gamma(1 + 1/k)
cat("1. Theoretical expected value using gamma function:", theoretical_mean, "\n")

# 2. Monte Carlo approximation
# Set seed for reproducibility
set.seed(2134)

# Generate 5000 samples from Weibull distribution
# Note: R's rweibull uses (shape, scale) parameterization
sample_size <- 5000
weibull_samples <- rweibull(sample_size, shape = k, scale = lambda)

# Calculate sample mean
monte_carlo_mean <- mean(weibull_samples)
cat("2. Monte Carlo approximation (n=5000):", monte_carlo_mean, "\n")

# Calculate absolute difference
absolute_difference <- abs(theoretical_mean - monte_carlo_mean)
cat("Absolute difference:", absolute_difference, "\n")
cat("Absolute difference (rounded to 4 decimal places):", round(absolute_difference, 4), "\n")

# Additional information
cat("\nAdditional details:\n")
cat("Sample standard deviation:", sd(weibull_samples), "\n")
cat("Standard error of mean:", sd(weibull_samples)/sqrt(sample_size), "\n")
