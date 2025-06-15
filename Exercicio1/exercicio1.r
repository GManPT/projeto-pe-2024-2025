library(ggplot2)

# Set working directory to the script location
setwd("/home/vicente/projeto-pe-2024-2025/Exercicio1")

# Read the dataset with full path or ensure CSV is in correct directory
wine_data <- read.csv("winequality-white-q5.csv")

# Create the box plot with square root transformation of residual.sugar
plot <- ggplot(wine_data, aes(x = factor(quality), y = sqrt(residual.sugar))) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  labs(
    title = "Square Root of Residual Sugar by Wine Quality",
    x = "Wine Quality",
    y = "Square Root of Residual Sugar"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

# Display the plot
print(plot)
ggsave("wine_quality_boxplot.png", plot = plot, width = 10, height = 6, dpi = 300)
