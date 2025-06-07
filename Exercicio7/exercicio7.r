# Given data
n <- 16
sum_x <- 120.68
sum_log_x <- 32.2

# Sample statistics
x_bar <- sum_x / n
log_x_bar <- sum_log_x / n

cat("Dados:\n")
cat("n =", n, "\n")
cat("sum(x_i) =", sum_x, "\n")
cat("sum(log(x_i)) =", sum_log_x, "\n")
cat("x_bar =", x_bar, "\n")
cat("log_x_bar =", log_x_bar, "\n\n")

# For Gamma distribution with pdf f(x) = (lambda^alpha / Gamma(alpha)) * x^(alpha-1) * exp(-lambda*x)
# The MLE equations are:
# 1) lambda_hat = alpha_hat / x_bar
# 2) digamma(alpha_hat) - log(alpha_hat) + log(x_bar) - log_x_bar = 0

# Define the equation to solve for alpha_hat
# digamma(alpha) - log(alpha) + log(x_bar) - log_x_bar = 0
equation_for_alpha <- function(alpha) {
  digamma(alpha) - log(alpha) + log(x_bar) - log_x_bar
}

# Use uniroot to find alpha_hat in the interval [62.2, 77.8]
alpha_hat <- uniroot(equation_for_alpha, interval = c(62.2, 77.8))$root

# Calculate lambda_hat using the relationship lambda_hat = alpha_hat / x_bar
lambda_hat <- alpha_hat / x_bar

# Calculate the modal length: (alpha - 1) / lambda
modal_length <- (alpha_hat - 1) / lambda_hat

cat("Estimativas de Maxima Verossimilhanca:\n")
cat("alpha_hat =", alpha_hat, "\n")
cat("lambda_hat =", lambda_hat, "\n")
cat("Comprimento modal (alpha_hat - 1) / lambda_hat =", modal_length, "\n")
cat("Comprimento modal arredondado a 2 casas decimais =", round(modal_length, 2), "\n")

# Verification: check if our alpha_hat satisfies the equation
cat("\nVerificacao:\n")
cat("Valor da equacao em alpha_hat:", equation_for_alpha(alpha_hat), "\n")