# ================================================================================
# EXERCICIO 4 - Distribuição de Weibull: Valor Esperado Teórico vs Monte Carlo
# ================================================================================

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

cat("\n\n")

# ================================================================================
# EXERCICIO 5 - Simulação de Jogo com Três Dados: Comparação entre Soma 9 e 10
# ================================================================================

# Set seed for reproducibility
set.seed(2318)

# Number of games to simulate
n <- 38000

# Initialize counters
sum_9_count <- 0
sum_10_count <- 0

# Simulate n games
for (i in 1:n) {
  # Roll three dice
  dice1 <- sample(1:6, 1)
  dice2 <- sample(1:6, 1)
  dice3 <- sample(1:6, 1)
  
  # Calculate sum
  total_sum <- dice1 + dice2 + dice3
  
  # Count wins for each player
  if (total_sum == 9) {
    sum_9_count <- sum_9_count + 1
  } else if (total_sum == 10) {
    sum_10_count <- sum_10_count + 1
  }
}

# Calculate relative frequencies
freq_sum_9 <- sum_9_count / n
freq_sum_10 <- sum_10_count / n

# Calculate difference
difference <- freq_sum_10 - freq_sum_9

# Display results
cat("Numero de jogadas simuladas:", n, "\n")
cat("Numero de 'soma 9':", sum_9_count, "\n")
cat("Numero de 'soma 10':", sum_10_count, "\n")
cat("Frequencia relativa 'soma 9':", freq_sum_9, "\n")
cat("Frequencia relativa 'soma 10':", freq_sum_10, "\n")
cat("Diferenca (soma 10 - soma 9):", difference, "\n")
cat("Diferenca arredondada a 4 casas decimais:", round(difference, 4), "\n")

cat("\n\n")

# ================================================================================
# EXERCICIO 6 - Distribuição Irwin-Hall: Valor Exato vs Aproximações TLC e Simulação
# ================================================================================

# Parameters
n <- 12
x <- 6.25
m <- 100  # number of samples for simulation

# 1. Exact value using Irwin-Hall distribution formula
# P(S_n <= x) = (1/n!) * sum_{k=0}^{floor(x)} (-1)^k * choose(n,k) * (x-k)^n

exact_probability <- function(n, x) {
  floor_x <- floor(x)
  sum_term <- 0
  
  for (k in 0:floor_x) {
    term <- (-1)^k * choose(n, k) * (x - k)^n
    sum_term <- sum_term + term
  }
  
  return(sum_term / factorial(n))
}

p_n <- exact_probability(n, x)
cat("1. Valor exato p_n =", p_n, "\n")

# 2a. Central Limit Theorem approximation
# For X ~ Uniform(0,1): E(X) = 0.5, Var(X) = 1/12
# For S_n: E(S_n) = n * 0.5 = 6, Var(S_n) = n * (1/12) = 1

mean_sn <- n * 0.5
var_sn <- n * (1/12)
sd_sn <- sqrt(var_sn)

# Standardize and use normal approximation
z <- (x - mean_sn) / sd_sn
p_n_tlc <- pnorm(z)
cat("2a. Aproximacao TLC p_n_TLC =", p_n_tlc, "\n")

# 2b. Simulation
set.seed(4469)

# Generate m samples of size n from Uniform(0,1)
sn_values <- numeric(m)

for (i in 1:m) {
  # Generate n uniform random variables and sum them
  sample_x <- runif(n, 0, 1)
  sn_values[i] <- sum(sample_x)
}

# Calculate proportion of S_n values <= 6.25
p_n_sim <- mean(sn_values <= x)
cat("2b. Aproximacao por simulacao p_n_sim =", p_n_sim, "\n")

# 3. Absolute deviation between exact and CLT
desvio_tlc <- abs(p_n - p_n_tlc)
cat("3. Desvio absoluto |p_n - p_n_TLC| =", desvio_tlc, "\n")

# 4. Absolute deviation between exact and simulation
desvio_sim <- abs(p_n - p_n_sim)
cat("4. Desvio absoluto |p_n - p_n_sim| =", desvio_sim, "\n")

# 5. Ratio of deviations
quociente <- desvio_tlc / desvio_sim
cat("5. Quociente dos desvios =", quociente, "\n")
cat("5. Quociente arredondado a 4 casas decimais =", round(quociente, 4), "\n")

# Additional information
cat("\nInformacoes adicionais:\n")
cat("Media de S_n (teorica):", mean_sn, "\n")
cat("Desvio padrao de S_n (teorico):", sd_sn, "\n")
cat("Media das simulacoes de S_n:", mean(sn_values), "\n")
cat("Desvio padrao das simulacoes de S_n:", sd(sn_values), "\n")

cat("\n\n")

# ================================================================================
# EXERCICIO 7 - Estimação MLE para Distribuição Gamma
# ================================================================================

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

cat("\n\n")

# ================================================================================
# EXERCICIO 8 - Intervalos de Confiança: Simulação de Cobertura
# ================================================================================

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

cat("\n\n")

# ================================================================================
# EXERCICIO 9 - Teste de Hipóteses: Estimação do Erro Tipo II (Beta)
# ================================================================================

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

cat("\n\n")

# ================================================================================
# EXERCICIO 10 - Teste Qui-Quadrado de Aderência: Distribuição de Rayleigh
# ================================================================================

# Original data (200 observations)
original_data <- c(2.3, 2.7, 5.2, 0.7, 2.9, 0.6, 2.6, 2.2, 3.8, 0.5, 4.9, 5.4, 3.7, 0.4, 4, 3.6, 2, 0.8, 2.5, 2.8, 1.7, 3.3,
1.5, 0.4, 6.4, 1.5, 6, 2.1, 0.4, 4.6, 3.1, 4.4, 4, 2.1, 5, 3.3, 4.7, 3.4, 4.3, 4.5, 2.3, 0.5, 4.9, 3.5,
1.8, 1.9, 2.6, 4.3, 4.6, 5.2, 1.6, 2.8, 2.4, 2.8, 1.8, 3.6, 0.8, 5.1, 1.4, 3.2, 1, 6.3, 3.6, 3.6, 1.8,
0.9, 4.6, 2.5, 5.8, 0.6, 3.3, 3.2, 6.6, 2.6, 2.5, 1.5, 4.1, 1.7, 2.1, 1.5, 0.4, 4.8, 0.4, 1.5, 4.2, 3.3,
1.2, 8.1, 2.4, 2.8, 2.1, 6.3, 4.2, 1.3, 6, 1.3, 3.7, 2.5, 6.6, 2.7, 1.4, 2, 0.7, 4.3, 3.4, 4.3, 4, 4,
0.8, 2.3, 2.5, 5.4, 4.3, 0.5, 3.9, 2.2, 3.4, 1.3, 2.4, 4.7, 2, 1.3, 4.4, 2.9, 2.1, 2.5, 1.6, 2.3, 4.4,
1.9, 1.9, 1.7, 2, 4.2, 3.4, 3.9, 4.3, 1.3, 2.9, 2.2, 5.1, 2.3, 1.9, 2.9, 5.2, 3.4, 2.6, 2.4, 3.2, 1.3,
3.1, 5.1, 1.4, 4.2, 0.9, 1.3, 2.1, 2.6, 6.2, 1.6, 2.7, 1.7, 2.3, 3.3, 2.8, 1.2, 2.6, 1.5, 2, 2.8, 2.5, 2,
1.2, 2.2, 2.6, 2.5, 6, 1.9, 3, 3.8, 1.9, 3.2, 3.1, 1.8, 2.6, 1.9, 3.5, 3.7, 1.8, 2.2, 2, 1.3, 2, 1.1,
2.2, 3.1, 2.9, 1.3, 0.2, 3.9)

# Parameters
set.seed(5885)
n <- 160  # subsample size
sigma <- 2.4  # Rayleigh scale parameter
k <- 5  # number of classes

cat("Dados originais: ", length(original_data), "observacoes\n")
cat("Parametros do teste:\n")
cat("n =", n, "(tamanho da subamostra)\n")
cat("sigma =", sigma, "(parametro de escala Rayleigh)\n")
cat("k =", k, "(numero de classes)\n\n")

# 1. Select random subsample of size 160 without replacement
subsample_indices <- sample(1:length(original_data), n, replace = FALSE)
subsample <- original_data[subsample_indices]

cat("1. Subamostra selecionada (primeiros 10 valores):", head(subsample, 10), "...\n")

# 2. Define k=5 equiprobable classes under H0
# For Rayleigh distribution with CDF F(x) = 1 - exp(-x^2/(2*sigma^2))
# Quantiles for equiprobable classes: 0.2, 0.4, 0.6, 0.8, 1.0

probabilities <- seq(0.2, 1.0, by = 0.2)
# Rayleigh quantile function: Q(p) = sigma * sqrt(-2 * log(1-p))
class_boundaries <- sigma * sqrt(-2 * log(1 - probabilities))

cat("2. Limites das classes equiprovaveis:\n")
cat("   (0,", round(class_boundaries[1], 3), "]\n")
for (i in 2:length(class_boundaries)) {
  cat("   (", round(class_boundaries[i-1], 3), ",", round(class_boundaries[i], 3), "]\n")
}

# 3. Group observations into classes and get observed frequencies
observed_freq <- numeric(k)

for (i in 1:length(subsample)) {
  x <- subsample[i]
  if (x <= class_boundaries[1]) {
    observed_freq[1] <- observed_freq[1] + 1
  } else if (x <= class_boundaries[2]) {
    observed_freq[2] <- observed_freq[2] + 1
  } else if (x <= class_boundaries[3]) {
    observed_freq[3] <- observed_freq[3] + 1
  } else if (x <= class_boundaries[4]) {
    observed_freq[4] <- observed_freq[4] + 1
  } else {
    observed_freq[5] <- observed_freq[5] + 1
  }
}

cat("\n3. Frequencias absolutas observadas:\n")
for (i in 1:k) {
  cat("   Classe", i, ":", observed_freq[i], "\n")
}

# Expected frequency for each class (equiprobable under H0)
expected_freq <- n / k

cat("\nFrequencia esperada por classe:", expected_freq, "\n")

# 4. Chi-square goodness-of-fit test
chi_square_stat <- sum((observed_freq - expected_freq)^2 / expected_freq)
df <- k - 1  # degrees of freedom
p_value <- 1 - pchisq(chi_square_stat, df)

cat("\n4. Teste qui-quadrado:\n")
cat("Estatistica qui-quadrado:", chi_square_stat, "\n")
cat("Graus de liberdade:", df, "\n")
cat("Valor-p:", p_value, "\n")

# Decision at different significance levels
alpha_levels <- c(0.01, 0.05, 0.10)
decisions <- character(length(alpha_levels))

for (i in 1:length(alpha_levels)) {
  if (p_value < alpha_levels[i]) {
    decisions[i] <- "Rejeitar H0"
  } else {
    decisions[i] <- "Nao rejeitar H0"
  }
}

cat("\nDecisoes:\n")
cat("Nivel 1%:", decisions[1], "\n")
cat("Nivel 5%:", decisions[2], "\n")
cat("Nivel 10%:", decisions[3], "\n")

# Determine which option matches the results
if (all(decisions == "Nao rejeitar H0")) {
  answer <- "e"
  cat("\nResposta: e) Nao rejeitar H0 aos n.s. de 1%, 5% e 10%.\n")
} else if (all(decisions == "Rejeitar H0")) {
  answer <- "b"
  cat("\nResposta: b) Rejeitar H0 aos n.s. de 1%, 5% e 10%.\n")
} else if (decisions[1] == "Nao rejeitar H0" && decisions[2] == "Rejeitar H0" && decisions[3] == "Rejeitar H0") {
  answer <- "a"
  cat("\nResposta: a) Rejeitar H0 aos n.s. de 5% e 10% e nao rejeitar H0 ao n.s. de 1%.\n")
} else if (decisions[1] == "Nao rejeitar H0" && decisions[2] == "Nao rejeitar H0" && decisions[3] == "Rejeitar H0") {
  answer <- "c"
  cat("\nResposta: c) Rejeitar H0 ao n.s. de 10% e nao rejeitar H0 aos n.s. de 1% e 5%.\n")
} else {
  answer <- "d"
  cat("\nResposta: d) Teste e inconclusivo.\n")
}
