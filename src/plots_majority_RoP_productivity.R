library(tidyverse)
library(ggplot2)
library(wesanderson)
library(RColorBrewer )

# setwd("C:/Archivos/datos/bea/codigos/majority_owned/")
# setwd("C:/Documents/data/bea/codigos/majority_owned/")

results_path = './results/bea/majority_owned_nonbank/'

#params
title_size=40
text_size= 30
axis_size= 5
strip_size= 6


data <- read.csv("./results/bea/majority_owned_nonbank/data_majority_owned_nonbank_procesado.csv") 
tcp_df <- read.csv('./results/indice_tcp.csv')
# Create a vector of unique sectors
sectors <- unique(data$sector)


#tg y pt
data %>%
  filter( country %in% c("South America", "European Union", "All Countries Total",
                         "Asia and Pacific", "Central America", "Mexico") ) %>% 
  filter( sector %in% c("All Industries Total","Mining", 
                        "Transportation Equipment", "Total Manufacturing"  ) ) %>% 
  group_by(country, sector) %>% 
  summarise(TGstock = mean(TGstock, na.rm=T) , 
            PT = mean(PT, na.rm=T)) %>% #PT/10e3
  reshape2::melt() %>% 
  ggplot(aes(x=reorder(variable,-value),y= value, fill = country)) +
  # geom_col(position = "dodge")+
  geom_bar(position = "dodge", stat="identity")+
  facet_wrap(sector~variable,
             scales = "free",
             ncol=2
             # ,strip.position = c("left", "top")
             # labeller = as_labeller(c(TGstock = "ratio TG", PT = "Miles de USD por obrero") )
  )+
  theme_minimal()+
  labs(title= "Productividad del trabajo y TG de inversiones de EEUU", 
       subtitle = "Promedio 1999-2019", y="Millones USD por obrero y ratio TG")+
  theme(plot.title = element_text(size= title_size),
        plot.subtitle = element_text(size= title_size*.8),
        legend.text = element_text(size=text_size),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size= text_size),
        axis.text.y = element_text(size= text_size),
        axis.text.x = element_blank(),
        strip.text = element_text(size=text_size),
        strip.placement = "outside" )+
  scale_fill_brewer(palette="Paired")
ggsave("./results/bea/majority_owned_nonbank/tg_y_pt_eu_sa_all_avg_sectors_more_countries.png", width = 20, height = 15)

data %>%
  filter( country %in% c("South America", "European Union", "All Countries Total") ) %>%
  # filter( sector %in% c("All Industries Total","Mining", 
  #                       "Transportation Equipment", "Total Manufacturing"  ) ) %>% 
  ggplot(aes(PT, TGstock, color = country)) + #PT/10e3
  geom_point()+
  facet_wrap(~sector, scales = "free_y")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1L))+
  theme_minimal()+
  labs(title= "Productividad del trabajo y TG de inversiones de EEUU", 
       subtitle = "América del Sur, Europa y total países",
       x="Productividad del trabajo", y= "Tasa de ganancia")+
  theme(plot.title = element_text(size= title_size*.5),
        plot.subtitle = element_text(size= title_size*.5*.8),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(size= 14),
        axis.text.y = element_text(size= 14),
        strip.text = element_text(size=14))+
  scale_color_manual(values=wes_palette(n=3, name="GrandBudapest1"))
ggsave("./results/bea/majority_owned_nonbank/tg_y_pt_eu_sa_all_scatter.png", width = 15, height = 10)

data %>%
  filter( country %in% c("South America", "European Union", "All Countries Total",
                         "Asia and Pacific", "Central America", "Mexico"
  ) ) %>%
  filter( sector %in% c("All Industries Total",#"Mining",
                        "Transportation Equipment", "Total Manufacturing"  ) ) %>%
  group_by(country, sector) %>% 
  summarise(TGstock = mean(TGstock, na.rm=T) , 
            PT = mean(PT, na.rm=T)) %>% #PT/10e3
  ggplot(aes(PT, TGstock, color = country)) +
  geom_point(size=7)+
  facet_wrap(~sector
             # , scales = "free_y"
  )+
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1L))+
  theme_linedraw()+
  labs(title= "Productividad y TG de inversiones de EEUU", 
       subtitle = "América del Sur, Europa y total países (promedio 1999-2019)",
       x="Productividad del trabajo (Millones USD por obrero)", y= "Tasa de ganancia")+
  theme(plot.title = element_text(size= title_size),
        plot.subtitle = element_text(size= title_size*.8),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size=text_size),
        axis.title = element_text(size=text_size),
        axis.text = element_text(size=text_size),
        strip.text = element_text(size=text_size-5))+
  scale_color_brewer(palette="Paired")
ggsave("./results/bea/majority_owned_nonbank/tg_y_pt_eu_sa_all_avg_scatter_selected_countries.png", width = 15, height = 10)
