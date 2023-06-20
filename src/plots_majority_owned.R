library(tidyverse)
library(ggplot2)
library(wesanderson)
library(RColorBrewer )

# setwd("C:/Archivos/datos/bea/codigos/majority_owned/")
# setwd("C:/Documents/data/bea/codigos/majority_owned/")

#params
title_size=40
text_size= 30
axis_size= 5
strip_size= 6

#data
data <- read.csv("../results/bea/majority_owned_nonbank/data_majority_owned_nonbank.csv") %>% 
  filter(sector != "Other Industries") %>% 
  mutate(sector = case_when(sector == "Electrical equipment, appliances, and components" ~
                              "Electrical equipment",
                            sector == "Finance (except depository institutions) and insurance" ~
                              "Finance without depository",
                            sector == "Professional, scientific, and technical services" ~
                              "Professional services",
                            T ~ sector ))

# TG
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
ggsave("../results/bea/majority_owned_nonbank/tg_eu_sa_all_2.png")



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
ggsave("../results/bea/majority_owned_nonbank/tg_sa_2.png", width = 15, height=10)

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
ggsave("../results/bea/majority_owned_nonbank/tg_sa_eu_all.png", width = 15, height=10)


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
ggsave("../results/bea/majority_owned_nonbank/tg_eu_sa_all_avg.png")

# PT
data %>%
  filter( country %in% c("South America", "Europe") ) %>%
  ggplot(aes(year, PT*10e3, fill = country)) +
  geom_col(position = "dodge")+
  facet_wrap(~sector, scales = "free")+
  theme_minimal()+
  labs(title= "Productividad de inversiones de EEUU", subtitle = "Europa y América del Sur",
       y = "Miles de USD por obrero")+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, size= 5),
        axis.text.y = element_text(size= 5),
        axis.title.x  = element_blank() ,
        axis.title.y  = element_text(size = 3.9) ,
        strip.text = element_text(size=3.9))+
  scale_fill_manual(values=wes_palette(n=3, name="Royal1"))
ggsave("../results/bea/majority_owned_nonbank/pt_eu_sa.png")

data %>%
  filter( Continent %in% c("South America") ) %>%
  left_join(data %>% 
              filter(country == "Europe") %>% 
              select(year, sector, PTeu=PT) ,
            by = c("year", "sector" )) %>%
  mutate(PTrel = PT/PTeu - 1) %>% 
  ggplot(aes(year, PTrel, color = country)) +
  geom_line(size = 0.3)+
  facet_wrap(~sector, scales = "free")+
  scale_y_continuous(labels = scales::percent)+
  theme_minimal()+
  labs(title= "Brecha de productividad de inversiones de EEUU",
       subtitle = "América del Sur relativa a total Europa")+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, size= 5),
        axis.text.y = element_text(size= 4),
        axis.title  = element_blank()  ,
        strip.text = element_text(size=5))+
  scale_color_brewer(palette="Paired")
ggsave("../results/bea/majority_owned_nonbank/pt_eu_sa_relativa.png")


data %>%
  filter( country %in% c("South America", "European Union", "All Countries Total") ) %>%
  group_by(country, sector) %>% 
  summarise(TGstock = mean(TGstock, na.rm=T) , 
            PT = mean(PT*10e3, na.rm=T)) %>% 
  ggplot(aes(country, PT, fill = country)) +
  geom_col(position = "dodge")+
  facet_wrap(~sector, scales = "free")+
  theme_minimal()+
  labs(title= "Productividad de inversiones de EEUU", 
       subtitle = "América del Sur, Europa y total países (promedio 1999-2019)",
       y = "Miles de USD por obrero")+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size= 8),
        axis.title.x  = element_blank() ,
        axis.title.y  = element_text(size = 5) ,
        strip.text = element_text(size=5))+
  scale_fill_manual(values=wes_palette(n=3, name="Moonrise3"))
ggsave("../results/bea/majority_owned_nonbank/pt_eu_sa_all_avg.png")

data %>%
  filter( country %in% c("South America", "European Union", "All Countries Total") &
            sector %in% c("All Industries Total","Mining", "Transportation Equipment", "Total Manufacturing"        )) %>%
  group_by(country, sector) %>% 
  summarise(TGstock = mean(TGstock, na.rm=T) , 
            PT = mean(PT*10e3, na.rm=T)) %>% 
  ggplot(aes(country, PT, fill = country)) +
  geom_col(position = "dodge")+
  facet_wrap(~sector, scales = "free")+
  theme_minimal()+
  labs(title= "Productividad del trabajo de inversiones de EEUU", 
       subtitle = "América del Sur, Europa y total países (promedio 1999-2019)",
       y = "Miles de USD por obrero")+
  theme(plot.title = element_text(size= title_size),
        plot.subtitle = element_text(size= title_size*.8),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size=text_size),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size= text_size),
        axis.title.x  = element_blank() ,
        axis.title.y  = element_text(size = text_size) ,
        strip.text = element_text(size=text_size))+
  scale_fill_manual(values=wes_palette(n=3, name="Moonrise3"))
ggsave("../results/bea/majority_owned_nonbank/pt_eu_sa_all_avg_sectors.png", width = 15, height=10)


#salario
data %>%
  filter( country %in% c("South America", "European Union", "All Countries Total",
                         "Asia and Pacific", "Central America", "Mexico") ) %>% 
  filter( sector %in% c("All Industries Total","Mining", 
                        "Transportation Equipment", "Total Manufacturing"  ) ) %>% 
  group_by(country, sector) %>% 
  summarise(Rem = mean(Rem*10e6, na.rm=T) ) %>% 
  reshape2::melt() %>% 
  ggplot(aes(x=reorder(variable,-value),y= value, fill = country)) +
  # geom_col(position = "dodge")+
  geom_bar(position = "dodge", stat="identity")+
  facet_wrap(~sector, scales = "free", ncol=2)+
  theme_minimal()+
  labs(title= "Salario promedio en las inversiones de EEUU", 
       subtitle = "Promedio 1999-2019", y="USD")+
  theme(plot.title = element_text(size=title_size),
        plot.subtitle  = element_text(size=title_size*.8),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size=text_size),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size= text_size),
        axis.text.y = element_text(size= text_size),
        axis.text.x = element_blank(),
        strip.text = element_text(size=text_size))+
  scale_fill_brewer(palette="Paired")
ggsave("../results/bea/majority_owned_nonbank/salario_avg_sectors.png", width = 15, height = 10)

#tg y pt
data %>%
  filter( country %in% c("South America", "European Union", "All Countries Total",
                         "Asia and Pacific", "Central America", "Mexico") ) %>% 
  filter( sector %in% c("All Industries Total","Mining", 
                        "Transportation Equipment", "Total Manufacturing"  ) ) %>% 
  group_by(country, sector) %>% 
  summarise(TGstock = mean(TGstock, na.rm=T) , 
            PT = mean(PT*10e3, na.rm=T)) %>% 
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
       subtitle = "Promedio 1999-2019", y="Miles de USD por obrero y ratio TG")+
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
ggsave("../results/bea/majority_owned_nonbank/tg_y_pt_eu_sa_all_avg_sectors_more_countries.png", width = 20, height = 15)

data %>%
  filter( country %in% c("South America", "European Union", "All Countries Total") ) %>%
  # filter( sector %in% c("All Industries Total","Mining", 
  #                       "Transportation Equipment", "Total Manufacturing"  ) ) %>% 
  ggplot(aes(PT*10e3, TGstock, color = country)) +
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
ggsave("../results/bea/majority_owned_nonbank/tg_y_pt_eu_sa_all_scatter.png", width = 15, height = 10)

data %>%
  filter( country %in% c("South America", "European Union", "All Countries Total",
                         "Asia and Pacific", "Central America", "Mexico"
                         ) ) %>%
  filter( sector %in% c("All Industries Total",#"Mining",
                        "Transportation Equipment", "Total Manufacturing"  ) ) %>%
  group_by(country, sector) %>% 
  summarise(TGstock = mean(TGstock, na.rm=T) , 
            PT = mean(PT*10e3, na.rm=T)) %>% 
  ggplot(aes(PT, TGstock, color = country)) +
  geom_point(size=7)+
  facet_wrap(~sector
             # , scales = "free_y"
             )+
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1L))+
  theme_linedraw()+
  labs(title= "Productividad y TG de inversiones de EEUU", 
       subtitle = "América del Sur, Europa y total países (promedio 1999-2019)",
       x="Productividad del trabajo (miles de USD por obrero)", y= "Tasa de ganancia")+
  theme(plot.title = element_text(size= title_size),
        plot.subtitle = element_text(size= title_size*.8),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size=text_size),
        axis.title = element_text(size=text_size),
        axis.text = element_text(size=text_size),
        strip.text = element_text(size=text_size-5))+
  scale_color_brewer(palette="Paired")
ggsave("../results/bea/majority_owned_nonbank/tg_y_pt_eu_sa_all_avg_scatter_selected_countries.png", width = 15, height = 10)
