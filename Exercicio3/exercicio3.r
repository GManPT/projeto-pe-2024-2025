# Load required libraries
library(ggplot2)
library(dplyr)
library(lubridate)
library(readr)

# Set working directory and read data
setwd("/home/alcafache/Documentos/PE/Exercicio3")
dados_clima <- read_csv("clima.csv", show_col_types = FALSE) %>%
  rename(datetime = Data, PRES = Pressão) %>%
  filter(year(datetime) == 2014, month(datetime) == 11) %>%
  group_by(date = as.Date(datetime)) %>%
  mutate(mediana_pressao_diaria = median(PRES, na.rm = TRUE)) %>%
  ungroup()

# Create and save plot
plot <- ggplot(dados_clima, aes(x = datetime)) +
  geom_line(aes(y = PRES, color = "Pressão Horária"), alpha = 0.7) +
  geom_line(aes(y = mediana_pressao_diaria, color = "Mediana Diária"), linewidth = 1) +
  labs(title = "Variação da Pressão Atmosférica em Novembro de 2014",
       x = "Data e Hora", y = "Pressão (hPa)", color = "Legenda") +
  scale_x_datetime(date_breaks = "3 days", date_labels = "%d/%m") +
  scale_color_manual(values = c("Pressão Horária" = "deepskyblue", "Mediana Diária" = "red4")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top")

print(plot)
ggsave("clima_pressao_nov2014.png", width = 11, height = 6.5, dpi = 300)