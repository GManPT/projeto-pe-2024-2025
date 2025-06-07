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