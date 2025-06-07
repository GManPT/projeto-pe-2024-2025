library(ggplot2)
library(readxl)
library(dplyr)

setwd("/home/alcafache/Documentos/PE/Exercicio2")

# Read and clean data for 2003
wine_data <- read_excel("wine_prod_EU.xlsx") %>%
  filter(!is.na(Category), `Product Group` != "Non-Vinified", Year == 2003) %>%
  mutate(Country_Group = case_when(
    `Member State` %in% c("France", "Italy", "Spain") ~ `Member State`,
    TRUE ~ "Others"
  )) %>%
  group_by(Country_Group, Category) %>%
  summarise(Total_Availability = sum(Availability, na.rm = TRUE), .groups = 'drop')

# Create bar chart
plot <- ggplot(wine_data, aes(x = Category, y = Total_Availability, fill = Country_Group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Wine Availability by Category and Country Group in 2003",
       x = "Wine Category", y = "Availability (10³ hL)", fill = "Country Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(type = "qual", palette = "Set2")

print(plot)
ggsave("wine_availability_2003.png", plot = plot, width = 12, height = 8, dpi = 300)
