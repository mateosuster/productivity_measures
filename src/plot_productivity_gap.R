
## PLOTS
library(scales)
library(viridis)
library(tidyverse)
library(ggplot2)
# library(RColorBrewer )
# library(wesanderson)


df<- read.csv('./results/bea/majority_owned_nonbank/productivity_gap.csv') %>% 
  select(year,country, brecha_calculada   )

colores_personalizados <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a", "#a6cee3", "#b2df8a", "#fb9a99", "#fdbf6f", "#cab2d6", "#ffff99", "#636363", "#b15928")

# Crea el gráfico de línea con la paleta de colores personalizada
df %>% 
  # filter(! country %in% c("Hungary" , "Poland", "Portugal")) %>%
  filter( country %in% c( "France", "Germany")) %>%
  ggplot( aes(x = year, y = brecha_calculada, color = country)) +
  geom_line() +
  scale_color_manual(values = colores_personalizados) +
  scale_y_continuous(labels = percent_format(),
                     , breaks = seq(0, 3, by = 0.1)) +  # Configura el eje y como porcentaje
  
  theme_minimal()
ggsave('./results/bea/majority_owned_nonbank/productivity_gap_eu_sel.png')
summary(df)
