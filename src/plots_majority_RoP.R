library(tidyverse)
library(ggplot2)
library(wesanderson)
library(RColorBrewer )

# setwd("C:/Archivos/datos/bea/codigos/majority_owned/")
# setwd("C:/Documents/data/bea/codigos/majority_owned/")

results_path = './results/bea/majority_owned_nonbank/tg/'

#params
title_size=40
text_size= 30
axis_size= 5
strip_size= 6


data <- read.csv("./results/bea/majority_owned_nonbank/tg/data_majority_owned_nonbank_procesado.csv") 
tcp_df <- read.csv('./results/indice_tcp.csv')
# Create a vector of unique sectors
sectors <- unique(data$sector)
# "Computers and electronic products"

# Tasa de Ganancia (TG)
data %>%
  filter( country %in% c("South America", "European Union", "All Countries Total", "Asia and Pacific") ) %>%
  filter( !sector %in% c("Finance without depository", "Finance and insurance") ) %>%
  ggplot(aes(year, TGstock, color = country)) +
  geom_line(size = 0.75, alpha = 0.75)+
  facet_wrap(~sector)+ #scales = "free"
  scale_y_continuous(labels = scales::percent)+
  theme_minimal()+
  labs(title= "Tasa de ganancia (sin rotación) de inversiones de EEUU", subtitle = "Europa y América del Sur")+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, size= axis_size),
        axis.text.y = element_text(size= axis_size),
        axis.title  = element_blank() ,
        strip.text = element_text(size=strip_size))+
  scale_color_manual(values=wes_palette(n=4, name="Moonrise2")) #"Royal2"
ggsave("./results/bea/majority_owned_nonbank/tg/tg_eu_sa_all_2.png")

# Tasa de Ganancia (TG)
data %>%
  filter( country %in% c("South America", "European Union", "All Countries Total", "Asia and Pacific") ) %>%
  filter( !sector %in% c("Finance without depository", "Finance and insurance") ) %>%
  ggplot(aes(year, TG, color = country)) +
  geom_line(size = 0.75, alpha = 0.75)+
  facet_wrap(~sector)+ #scales = "free"
  scale_y_continuous(labels = scales::percent)+
  theme_minimal()+
  labs(title= "Tasa de ganancia (con rotación) de inversiones de EEUU", subtitle = "Europa y América del Sur")+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, size= axis_size),
        axis.text.y = element_text(size= axis_size),
        axis.title  = element_blank() ,
        strip.text = element_text(size=strip_size))+
  scale_color_manual(values=wes_palette(n=4, name="Moonrise2")) #"Royal2"
ggsave("./results/bea/majority_owned_nonbank/tg/tg_eu_sa_all_3.png")


data %>%
  filter( Continent %in% c("South America") ) %>%
  filter( !sector %in% c("Finance without depository", 
                         "Finance and insurance",
                         "Professional services",
                         "Wholesale Trade") ) %>%
  ggplot(aes(year, TGstock, color = country)) +
  geom_line()+
  facet_wrap(~sector)+ #scales = "free"
  scale_y_continuous(labels = scales::percent)+
  theme_minimal()+
  labs(title= "Tasa de ganancia (sin rotación) de inversiones de EEUU", subtitle = "América del Sur")+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = axis_size+6),
        axis.text.x = element_text(angle = 45, size= axis_size+6),
        axis.text.y = element_text(size= axis_size+6),
        axis.title  = element_blank() ,
        strip.text = element_text(size=strip_size+5))+
  scale_color_brewer(palette="Paired")
ggsave("./results/bea/majority_owned_nonbank/tg/tg_sa_2.png", width = 15, height=10)


data %>%
  filter( Continent %in% c("South America") ) %>%
  filter( !sector %in% c("Finance without depository", 
                         "Finance and insurance",
                         "Professional services",
                         "Wholesale Trade") ) %>%
  ggplot(aes(year, TG, color = country)) +
  geom_line()+
  facet_wrap(~sector)+ #scales = "free"
  scale_y_continuous(labels = scales::percent)+
  theme_minimal()+
  labs(title= "Tasa de ganancia (con rotación) de inversiones de EEUU", subtitle = "América del Sur")+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = axis_size+6),
        axis.text.x = element_text(angle = 45, size= axis_size+6),
        axis.text.y = element_text(size= axis_size+6),
        axis.title  = element_blank() ,
        strip.text = element_text(size=strip_size+5))+
  scale_color_brewer(palette="Paired")
ggsave("./results/bea/majority_owned_nonbank/tg/tg_sa_3.png", width = 15, height=10)

data %>%
  filter( Continent %in% c("South America") | country %in% c("All Countries Total",
                                                             "European Union") ) %>%
  ggplot(aes(year, TGstock, color = country)) +
  geom_line()+
  facet_wrap(~sector, scales = "free")+
  scale_y_continuous(labels = scales::percent)+
  theme_minimal()+
  labs(title= "Tasa de ganancia (sin rotación) de inversiones de EEUU", subtitle = "América del Sur, Europa y total paises")+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, size= 5),
        axis.text.y = element_text(size= 5),
        axis.title  = element_blank() ,
        strip.text = element_text(size=10))+
  scale_color_brewer(palette="Paired")
ggsave("./results/bea/majority_owned_nonbank/tg/tg_sa_eu_all.png", width = 15, height=10)


data %>%
  filter( Continent %in% c("South America") | country %in% c("All Countries Total",
                                                             "European Union") ) %>%
  ggplot(aes(year, TG, color = country)) +
  geom_line()+
  facet_wrap(~sector, scales = "free")+
  scale_y_continuous(labels = scales::percent)+
  theme_minimal()+
  labs(title= "Tasa de ganancia (con rotación) de inversiones de EEUU", subtitle = "América del Sur, Europa y total paises")+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, size= 5),
        axis.text.y = element_text(size= 5),
        axis.title  = element_blank() ,
        strip.text = element_text(size=10))+
  scale_color_brewer(palette="Paired")
ggsave("./results/bea/majority_owned_nonbank/tg/tg_sa_eu_all_1.png", width = 15, height=10)


data %>%
  filter( country %in% c("South America", "European Union", "All Countries Total") ) %>%
  group_by(country, sector) %>% 
  summarise(TGstock = mean(TGstock, na.rm=T) , 
            PT = mean(PT, na.rm=T)) %>% 
  ggplot(aes(country, TGstock, fill = country)) +
  geom_col(position = "dodge")+
  facet_wrap(~sector, scales = "free")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L))+
  theme_minimal()+
  labs(title= "Tasa de ganancia (sin rotación) de inversiones de EEUU", subtitle = "América del Sur, Europa y total países (promedio 1999-2019)")+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size= 8),
        axis.title  = element_blank() ,
        strip.text = element_text(size=5))+
  scale_fill_manual(values=wes_palette(n=3, name="Royal2"))
ggsave("./results/bea/majority_owned_nonbank/tg/tg_eu_sa_all_avg.png")



data %>%
  filter( country %in% c("South America", "European Union", "All Countries Total") ) %>%
  group_by(country, sector) %>% 
  summarise(TG = mean(TG, na.rm=T) , 
            PT = mean(PT, na.rm=T)) %>% 
  ggplot(aes(country, TG, fill = country)) +
  geom_col(position = "dodge")+
  facet_wrap(~sector, scales = "free")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L))+
  theme_minimal()+
  labs(title= "Tasa de ganancia (con rotación) de inversiones de EEUU", subtitle = "América del Sur, Europa y total países (promedio 1999-2019)")+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size= 8),
        axis.title  = element_blank() ,
        strip.text = element_text(size=5))+
  scale_fill_manual(values=wes_palette(n=3, name="Royal2"))
ggsave("./results/bea/majority_owned_nonbank/tg/tg_eu_sa_all_avg_1.png")

# argentina


#manufacturing
