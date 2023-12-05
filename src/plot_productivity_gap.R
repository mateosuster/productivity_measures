
## PLOTS
library(scales)
library(viridis)
library(tidyverse)
library(ggplot2)
# library(RColorBrewer )
# library(wesanderson)


df<- read.csv('./results/bea/majority_owned_nonbank/productivity_gap_all_sectors.csv') %>% 
  select(year,country, sector, brecha_calculada  , 
         ipt_arg_99_index, ipt_bench_99_index) %>% 
  filter (sector %in% c("Total Manufacturing"  
                        ,"Transportation Equipment"
                        ,"Manufacturing without Transportation Equipment",
                        "Manufacturing without Food"
                        ) )

colores_personalizados <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a", "#a6cee3", "#b2df8a", "#fb9a99", "#fdbf6f", "#cab2d6", "#ffff99", "#636363", "#b15928")

# Crea el gráfico de línea con la paleta de colores personalizada
df %>% 
  # filter(! country %in% c("Hungary" , "Poland", "Portugal")) %>%
  filter( country %in% c( "France", "Germany"),
          # brecha_calculada > -10
          
          ) %>%
  ggplot( aes(x = year, y = brecha_calculada, color = country)) +
  geom_line() +
  facet_wrap(~sector)+
  scale_color_manual(values = colores_personalizados) +
  scale_y_continuous(labels = percent_format()
  , breaks = seq(0, 3, by = 0.1)
  ) +  # Configura el eje y como porcentaje
  theme_minimal()

ggsave('./results/bea/majority_owned_nonbank/productivity_gap_eu_manuf_3.png')
summary(df)




