library(tidyverse)
library(ggplot2)
library(wesanderson)
library(RColorBrewer )
library(plotly)

# setwd("C:/Archivos/datos/bea/codigos/majority_owned/")
# setwd("C:/Documents/data/bea/codigos/majority_owned/")

results_path = './results/bea/majority_owned_nonbank/'

#params
title_size=40
text_size= 30
axis_size= 5
strip_size= 6

data <- read.csv("./results/bea/majority_owned_nonbank/data_majority_owned_nonbank.csv") %>% 
  filter(sector != "Other Industries") %>% 
  mutate(sector = case_when(sector == "Electrical equipment, appliances, and components" ~
                              "Electrical equipment",
                            sector == "Finance (except depository institutions) and insurance" ~
                              "Finance and insurance", # "Finance without depository",
                            sector == "Professional, scientific, and technical services" ~
                                "Professional services",
                              T ~ sector ))
write.csv(data, 
          './results/bea/majority_owned_nonbank/data_majority_owned_nonbank_procesado.csv',
          row.names = F)
tcp_df <- read.csv('./results/indice_tcp.csv')


# TCP Plot
tcp_df %>% 
  # glimpse()
  ggplot(aes(year, index_TCP_1 ))+
  geom_line()


# Create a vector of unique sectors
sectors <- unique(data$sector)

melted_data <- data %>%
  gather(key = "variable", value = "value", asset, CI, Kcca, r_1, r_2, r_3, Kcca_1, Kcca_2, Kcca_3) %>%
  select(sector, country, year, variable, value)

kcca_vs_asset <- data %>% 
  mutate(kcca_prop = Kcca / asset )  %>% 
  select(c(sector, country, year, kcca_prop))
  

melted_data %>% 
  filter(! variable %in% c("asset", "CI", 'r_3') ) %>% 
  filter(grepl("^r", variable)) %>%
  ggplot(aes(x = year, y = value, color = sector)) + 
  geom_point(size = 0.3) +
  facet_wrap(~variable,
             labeller = labeller(variable = 
                                   c("r_1" = "r = net_income / Kcca",
                                     "r_2" = "r =total_sales / Kcca")),
             ) +
  labs(title = "Rotación. Distintas estimaciones")
ggsave("./results/bea/majority_owned_nonbank/rotacion_1_y_2.png", width = 10, height = 6) # Adjust width and height

kcca_plot  <- melted_data %>% 
  filter( variable %in% c("Kcca") ) %>% 
  # filter(! variable %in% c("asset", "CI", 'r_3') ) %>% 
  # filter(grepl("^Kcca", variable)) %>%
  ggplot(aes(x = year, y = value, color = country)) + 
  geom_point(size = 0.3) +
  facet_wrap(~sector,
             scales = 'free'
             
             # labeller = labeller(variable =
             #                       c("Kcca" = "Assets - PPE",
             #                         "r_2" = "total_sales / Kcca",
             #                         "r_2" = "total_sales / Kcca"
             #                         ))
  ) +
  theme(legend.position = 'none')
# ggplotly(kcca_plot)
ggsave("./results/bea/majority_owned_nonbank/kcca_1_y_2.png", width = 10, height = 6) # Adjust width and height

kcca_vs_asset %>% 
  ggplot(aes(x = year, y = kcca_prop, color = country)) + 
  geom_point(size = 0.3) +
  facet_wrap(~sector,
             scales = 'free') +
  theme(legend.position = 'none')

kcca_vs_asset$year <- as.factor(kcca_vs_asset$year)


# Create density plots facet-wrapped by years
ggplot(kcca_vs_asset, aes(x = kcca_prop)) +
  geom_density(fill = "skyblue", color = "black", alpha = 0.7) +
  facet_wrap(~ year) +
  geom_vline(data = summarise(group_by(kcca_vs_asset, year), mean_val = mean(kcca_prop, na.rm = TRUE)),
             aes(xintercept = mean_val), color = "red", linetype = "dashed", size = 1) +
  geom_vline(data = summarise(group_by(kcca_vs_asset, year), median_val = median(kcca_prop, na.rm = TRUE)),
             aes(xintercept = median_val), color = "blue", linetype = "dashed", size = 1) +
  labs(title = "Density Plots of kcca_prop across Years with Year-Specific Mean and Median Lines",
       x = "kcca_prop",
       y = "Density") +
  theme_minimal()
ggsave("./results/bea/majority_owned_nonbank/kcca_proportion_distribution.png", width = 10, height = 6) # Adjust width and height

