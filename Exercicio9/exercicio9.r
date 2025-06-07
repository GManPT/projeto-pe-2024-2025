# Set parameters
set.seed(4382)
m <- 700      # number of samples
n <- 13       # sample size
mu_0 <- 5     # null hypothesis mean
mu_1 <- 5.4   # alternative hypothesis mean
alpha <- 0.06 # significance level

# Calculate critical value for chi-square test
# We reject H0 if T0 > chi2_critical
df <- 2 * n
chi2_critical <- qchisq(1 - alpha, df)

cat("Parametros do teste:\n")
cat("n =", n, "\n")
cat("m =", m, "\n")
cat("mu_0 =", mu_0, "\n")
cat("mu_1 =", mu_1, "\n")
cat("alpha =", alpha, "\n")
cat("Graus de liberdade:", df, "\n")
cat("Valor critico chi-quadrado:", chi2_critical, "\n\n")

# Generate m samples from exponential distribution with mean mu_1
# and apply hypothesis test to each sample
rejections <- 0  # Count how many times we reject H0

for (i in 1:m) {
  # Generate sample from exponential with mean mu_1
  # Note: rexp uses rate parameter = 1/mean
  sample_data <- rexp(n, rate = 1/mu_1)
  
  # Calculate sample mean
  x_bar <- mean(sample_data)
  
  # Calculate test statistic T0 under H0 (assuming mu = mu_0)
  T0 <- (2 * n * x_bar) / mu_0
  
  # Test: reject H0 if T0 > critical value
  if (T0 > chi2_critical) {
    rejections <- rejections + 1
  }
}

# Estimate beta (probability of Type II error)
# Beta = P(not reject H0 | H1 is true)
beta_hat <- (m - rejections) / m

cat("Resultados da simulacao:\n")
cat("Numero de rejeicoes de H0:", rejections, "\n")
cat("Numero de nao rejeicoes de H0:", m - rejections, "\n")
cat("Estimativa de beta (beta_hat):", beta_hat, "\n")

# Calculate theoretical beta
# When H1 is true (mu = mu_1), the test statistic follows:
# T = (2*n*X_bar)/mu_0, where X_bar ~ Exp(mu_1)
# We need P(T <= chi2_critical | mu = mu_1)

# Under H1, T = (2*n*X_bar)/mu_0, where X_bar has mean mu_1
# The distribution of T under H1 is more complex, but we can calculate it
# T = (2*n*X_bar)/mu_0 where X_bar ~ Gamma(n, n/mu_1)
# So T ~ Gamma(n, n*mu_0/mu_1) * (2*n/mu_0) = Gamma(n, 2*n/mu_1)

# Actually, 2*n*X_bar/mu_1 ~ chi2(2n) when samples are from Exp(mu_1)
# So T = (2*n*X_bar/mu_0) = (mu_1/mu_0) * (2*n*X_bar/mu_1) ~ (mu_1/mu_0) * chi2(2n)

# The theoretical beta is P(T <= chi2_critical | H1)
# where T ~ (mu_1/mu_0) * chi2(2n)
# This is equivalent to P(chi2(2n) <= chi2_critical * mu_0/mu_1)

adjusted_critical <- chi2_critical * mu_0 / mu_1
beta_theoretical <- pchisq(adjusted_critical, df)

cat("Beta teorico:", beta_theoretical, "\n")

# Calculate quotient
quotient <- beta_hat / beta_theoretical

cat("Quociente beta_hat / beta:", quotient, "\n")
cat("Quociente arredondado a 4 casas decimais:", round(quotient, 4), "\n")

# Additional information
cat("\nInformacoes adicionais:\n")
cat("Valor critico ajustado:", adjusted_critical, "\n")
cat("Poder do teste (1 - beta):", 1 - beta_theoretical, "\n")
cat("Poder estimado (1 - beta_hat):", 1 - beta_hat, "\n")