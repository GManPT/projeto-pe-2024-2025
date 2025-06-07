# Set parameters
set.seed(1089)
m <- 1800  # number of samples
n <- 12    # sample size
mu <- 0    # true mean
sigma <- 1.1  # known standard deviation
gamma <- 0.9  # confidence level

# Calculate alpha and critical value
alpha <- 1 - gamma
z_alpha_2 <- qnorm(1 - alpha/2)

cat("Parametros:\n")
cat("m =", m, "(numero de amostras)\n")
cat("n =", n, "(tamanho de cada amostra)\n")
cat("mu =", mu, "(valor esperado verdadeiro)\n")
cat("sigma =", sigma, "(desvio padrao conhecido)\n")
cat("gamma =", gamma, "(nivel de confianca)\n")
cat("alpha =", alpha, "\n")
cat("z_{alpha/2} =", z_alpha_2, "\n\n")

# Generate m samples and calculate confidence intervals
confidence_intervals <- matrix(0, nrow = m, ncol = 2)  # Lower and upper bounds
contains_mu <- logical(m)  # Track which intervals contain mu

for (i in 1:m) {
  # Generate sample of size n from normal distribution
  sample_data <- rnorm(n, mean = mu, sd = sigma)
  
  # Calculate sample mean
  x_bar <- mean(sample_data)
  
  # Calculate standard error
  se <- sigma / sqrt(n)
  
  # Calculate confidence interval
  margin_error <- z_alpha_2 * se
  lower_bound <- x_bar - margin_error
  upper_bound <- x_bar + margin_error
  
  # Store interval
  confidence_intervals[i, 1] <- lower_bound
  confidence_intervals[i, 2] <- upper_bound
  
  # Check if interval contains true mu
  contains_mu[i] <- (lower_bound <= mu) & (mu <= upper_bound)
}

# Calculate proportion of intervals that contain mu
proportion_containing_mu <- mean(contains_mu)

# Calculate the quotient
quotient <- proportion_containing_mu / gamma

cat("1. Intervalos de confianca gerados: ", m, "\n")
cat("2. Proporcao de intervalos que contem mu = 0:", proportion_containing_mu, "\n")
cat("Quociente (proporcao / gamma):", quotient, "\n")
cat("Quociente arredondado a 4 casas decimais:", round(quotient, 4), "\n")

# Additional statistics
cat("\nEstatisticas adicionais:\n")
cat("Erro padrao teorico:", sigma/sqrt(n), "\n")
cat("Margem de erro teorica:", z_alpha_2 * sigma/sqrt(n), "\n")
cat("Numero de intervalos que contem mu:", sum(contains_mu), "\n")
cat("Numero de intervalos que NAO contem mu:", sum(!contains_mu), "\n")