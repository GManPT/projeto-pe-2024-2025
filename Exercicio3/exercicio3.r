library(ggplot2)

# Set working directory and read data
setwd("/home/vicente/projeto-pe-2024-2025/Exercicio3")
dados_clima_raw <- read.csv("clima.csv")

# Convert datetime and filter for November 2014
dados_clima_raw$datetime <- as.POSIXct(dados_clima_raw$Data)
dados_clima_raw$year <- as.numeric(format(dados_clima_raw$datetime, "%Y"))
dados_clima_raw$month <- as.numeric(format(dados_clima_raw$datetime, "%m"))
dados_clima_raw$date <- as.Date(dados_clima_raw$datetime)

# Filter for November 2014
dados_clima <- dados_clima_raw[dados_clima_raw$year == 2014 & dados_clima_raw$month == 11, ]

# Calculate daily median pressure
daily_medians <- aggregate(Pressão ~ date, data = dados_clima, FUN = median, na.rm = TRUE)
names(daily_medians)[2] <- "mediana_pressao_diaria"

# Merge back with original data
dados_clima <- merge(dados_clima, daily_medians, by = "date")

# Create and save plot
plot <- ggplot(dados_clima, aes(x = datetime)) +
  geom_line(aes(y = Pressão, color = "Pressão Horária"), alpha = 0.7) +
  geom_line(aes(y = mediana_pressao_diaria, color = "Mediana Diária"), linewidth = 1) +
  labs(title = "Variação da Pressão Atmosférica em Novembro de 2014",
       x = "Data e Hora", y = "Pressão (hPa)", color = "Legenda") +
  scale_x_datetime(date_breaks = "3 days", date_labels = "%d/%m") +
  scale_color_manual(values = c("Pressão Horária" = "deepskyblue", "Mediana Diária" = "red4")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top")

print(plot)
ggsave("clima_pressao_nov2014.png", width = 11, height = 6.5, dpi = 300)