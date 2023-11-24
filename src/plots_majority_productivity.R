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


# Productivity (PT)
data_prod <- data %>%
  select(c(sector, country, Continent , year,value_added, employment, PT)) %>%
  filter(PT > 0 &  employment > 0 ) %>%
  arrange(desc(PT)) #    arrange(PT)


data_prod %>%
  filter( country %in% c("South America", "Europe") 
          & sector != "Mining"
  ) %>%
  ggplot(aes(year, PT, fill = country)) + #PT/10e3
  geom_col(position = "dodge")+
  facet_wrap(~sector)+ #, scales = "free"
  theme_minimal()+
  labs(title= "Productividad de inversiones de EEUU", subtitle = "Total Europa y América del Sur",
       y = "Millones USD por obrero")+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, size= 5),
        axis.text.y = element_text(size= 5),
        axis.title.x  = element_blank() ,
        axis.title.y  = element_text(size = 3.9) ,
        strip.text = element_text(size=3.9))+
  scale_fill_manual(values=wes_palette(n=3, name="Royal1"))
ggsave(paste0(results_path,"pt_eu_sa_2.png"))




# Iterate through each sector
for (sec in sectors) {
  # Subset the data for the current sector
  sector_data <- data_prod %>%
    filter(sector == sec) 
  
  # Create the plot for the current sector
  plot <- ggplot(sector_data, aes(year, PT , color = country)) +
    geom_line() +
    facet_wrap(~Continent) + #scales = "free"
    theme_minimal() +
    labs(title = "Productividad de inversiones de EEUU",
         subtitle = paste("Total paises por contiente", sec),
         y = "Millones  de USD por obrero") +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, size = 5),
          axis.text.y = element_text(size = 5),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 3.9),
          strip.text = element_text(size = 3.9)) 
  
  ggsave(filename = paste(results_path, "plot_pt_", sec, "_1.png", sep = ""),
         plot = plot)
  
  
}


## Productividad relativa
data_prod_relativa <- data_prod %>%
  filter( Continent %in% c("South America") ) %>%
  left_join(data %>% 
              filter(country == "Europe") %>% 
              select(year, sector, PTeu=PT) ,
            by = c("year", "sector" )) %>%
  left_join(data %>% 
              filter(country == "Germany") %>% 
              select(year, sector, PTger=PT) ,
            by = c("year", "sector" )) %>%
  left_join(tcp_df %>% 
              select('year', "TCC", "TCP_1"),
            by = 'year') %>% 
  mutate(PTrel_nominal = PT/PTeu ,
         PTrel_ger_nominal = PT/PTger) 

data_prod_relativa_arg <- data_prod_relativa %>% 
  filter(country == "Argentina") %>% 
  mutate(PT_tcp = PT * TCC / TCP_1,
         PTrel_tcp = PT_tcp / PTeu,
         PTrel_ger_tcp = PT_tcp / PTger
  ) 

write.csv(data_prod_relativa_arg,
          "./results/bea/majority_owned_nonbank/productividad_relativa_arg.csv",
          row.names=FALSE)

data_prod_relativa %>% 
  filter(! sector %in% c("Finance and insurance" , "Information"  ,
                         "Utilities", "Professional services" ) ) %>% 
  ggplot(aes(year, PTrel_nominal, color = country)) +
  geom_line(size = 0.3)+
  facet_wrap(~sector)+ #, scales = "free"
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
ggsave("./results/bea/majority_owned_nonbank/pt_eu_sa_relativa_2.png")

custom_palette <- colorRampPalette(brewer.pal(10, "Set1"))(15)

# ALL EUROPE
data_prod_relativa_arg %>%
  select(sector, year,  PTrel_nominal, PTrel_tcp) %>%  #PT, PT_tcp,
  # select(-c(PT, PT_tcp)) %>%
  pivot_longer(cols = starts_with("PT"), names_to = "variable", values_to = "value") %>%
  ggplot(aes(year, value, color = sector)) +
  geom_line(size = 0.3) +
  facet_wrap(~variable, labeller = labeller(variable = 
                                              c("PTrel_nominal" = "Productividad Relativa Nominal",
                                                "PTrel_tcp" = "Productividad Relativa TCP"))
             ,
             ncol =2) + 
  scale_y_continuous(labels = scales::percent) +
  # theme_minimal() +
  # theme_bw()+
  theme_light()+
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, size= 10),
    axis.text.y = element_text(size= 12),
    axis.title  = element_blank(),
    strip.text = element_text(size=8),
    legend.text = element_text(size = 8) # Adjust legend text size
  ) +
  scale_color_manual(values = custom_palette)+
  # scale_color_brewer(palette="Paired") +
  # viridis::scale_color_viridis(discrete = TRUE, option = "viridis") +
  labs(
    title = "Brecha de Productividad de Inversiones de EEUU",
    subtitle = "Argentina Relativa a Total Europa",
    caption = "Fuente: elaboración propia en base a BEA"
  ) +
  
  coord_cartesian(expand = FALSE) # Prevent extra space around the plot
ggsave("./results/bea/majority_owned_nonbank/pt_arg_sa_relativa_2.png", width = 10, height = 6) # Adjust width and height

#vs germany
data_prod_relativa_arg %>%
  select(sector, year,  PTrel_ger_tcp, PTrel_tcp) %>%  #PT, PT_tcp,
  filter(sector %in% c("All Industries Total",  "Chemicals",
                       "Machinery" , "Food" ,
                       "Primary and fabricated metals",
                       "Computers and electronic products",
                       "Mining", "Total Manufacturing") ) %>% 
  pivot_longer(cols = starts_with("PT"), names_to = "variable", values_to = "value") %>%
  ggplot(aes(year, value, color = sector)) +
  geom_line(size = 0.3) +
  facet_wrap(~variable, labeller = labeller(variable = 
                                              c("PTrel_ger_tcp" = "Productividad Relativa TCP vs Alemania",
                                                "PTrel_tcp" = "Productividad Relativa TCP vs Europa"))
             ,
             ncol =2) + 
  scale_y_continuous(labels = scales::percent) +
  # theme_minimal() +
  # theme_bw()+
  theme_light()+
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, size= 10),
    axis.text.y = element_text(size= 12),
    axis.title  = element_blank(),
    strip.text = element_text(size=8),
    legend.text = element_text(size = 8) # Adjust legend text size
  ) +
  scale_color_manual(values = custom_palette)+
  # scale_color_brewer(palette="Paired") +
  # viridis::scale_color_viridis(discrete = TRUE, option = "viridis") +
  labs(
    title = "Brecha de Productividad de Inversiones de EEUU",
    subtitle = "Argentina Relativa a Total Europa (sectores seleccionados)",
    caption = "Fuente: elaboración propia en base a BEA"
  ) +
  coord_cartesian(expand = FALSE) # Prevent extra space around the plot
ggsave("./results/bea/majority_owned_nonbank/pt_arg_relativa_ger.png", width = 10, height = 6) # Adjust width and height




## Nivel de productividad (todos los sectores)
data %>%
  filter( country %in% c("South America", "European Union", "All Countries Total") ) %>%
  group_by(country, sector) %>% 
  summarise(TGstock = mean(TGstock, na.rm=T) , 
            PT = mean(PT, na.rm=T)) %>% #PT/10e3
  ggplot(aes(country, PT, fill = country)) +
  geom_col(position = "dodge")+
  facet_wrap(~sector)+ #, scales = "free"
  theme_minimal()+
  labs(title= "Productividad de inversiones de EEUU", 
       subtitle = "América del Sur, Europa y total países (promedio 1999-2019)",
       y = "Millones USD por obrero")+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size= 8),
        axis.title.x  = element_blank() ,
        axis.title.y  = element_text(size = 5) ,
        strip.text = element_text(size=5))+
  scale_fill_manual(values=wes_palette(n=3, name="Moonrise3"))
ggsave("./results/bea/majority_owned_nonbank/pt_eu_sa_all_avg_1.png")


## Nivel de productividad (sectores seleccionados)
data %>%
  filter( country %in% c("South America", "European Union", "All Countries Total") &
            sector %in% c("All Industries Total","Mining", "Transportation Equipment", "Total Manufacturing"        )) %>%
  group_by(country, sector) %>% 
  summarise(TGstock = mean(TGstock, na.rm=T) , 
            PT = mean(PT, na.rm=T)) %>%  # PT/10e3
  ggplot(aes(country, PT, fill = country)) +
  geom_col(position = "dodge")+
  facet_wrap(~sector )+ #scales = "free"
  theme_minimal()+
  labs(title= "Productividad del trabajo de inversiones de EEUU", 
       subtitle = "América del Sur, Europa y total países (promedio 1999-2019)",
       y = "Millones USD por obrero")+
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
ggsave("./results/bea/majority_owned_nonbank/pt_eu_sa_all_avg_sectors_1.png", width = 15, height=10)

