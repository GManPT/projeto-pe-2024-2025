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