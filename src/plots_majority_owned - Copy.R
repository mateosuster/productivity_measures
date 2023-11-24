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
ppp_df

# TCP Plot
tcp_df %>% 
  # glimpse()
  ggplot(aes(year, index_TCP_1 ))+
  geom_line()


# Create a vector of unique sectors
sectors <- unique(data$sector)

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
ggsave("./results/bea/majority_owned_nonbank/tg_eu_sa_all_2.png")



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
ggsave("./results/bea/majority_owned_nonbank/tg_sa_2.png", width = 15, height=10)

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
ggsave("./results/bea/majority_owned_nonbank/tg_sa_eu_all.png", width = 15, height=10)


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
ggsave("./results/bea/majority_owned_nonbank/tg_eu_sa_all_avg.png")

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
ggsave("./results/bea/majority_owned_nonbank/salario_avg_sectors.png", width = 15, height = 10)

## Salario relativo
salario_relativo_arg <- data %>%
  filter( country %in% c("Argentina") ) %>% 
  # filter( sector %in% c("All Industries Total","Mining", 
  #                       "Transportation Equipment", "Total Manufacturing"  ) ) %>% 
  select(c('sector', 'country', 'year', 'Rem')) %>% 
  left_join(data %>% 
              filter(country %in% c("Europe")) %>% 
              select(year, sector, RemEU=Rem) ,
            by = c("year", "sector" )) %>% 
  left_join(tcp_df %>% 
              select('year', "TCC", "TCP_1"),
            by = 'year') %>% 
  mutate(Rem_relativa_eu = Rem / RemEU - 1,
         Rem_tcp_arg = Rem * TCC / TCP_1,
         Rem_relativa_eu_tcp = Rem_tcp_arg / RemEU - 1)

write.csv(salario_relativo_arg,
          "./results/bea/majority_owned_nonbank/salario_relativa_arg.csv",
          row.names=FALSE)


custom_palette_2 <- colorRampPalette(brewer.pal(10, "Set1"))(16)
salario_relativo_arg %>%
  select(-c('Rem', 'Rem_tcp_arg', 'RemEU' )) %>% 
  filter(sector %in% c("All Industries Total",  "Chemicals",
                       "Machinery" , "Food" ,
                       "Primary and fabricated metals",
                       "Computers and electronic products",
                       "Mining", "Total Manufacturing") ) %>% 
  pivot_longer(cols = starts_with("Rem"), names_to = "variable", values_to = "value") %>% 
  # reshape2::melt() %>% 
  ggplot(aes(x=year,y= value, color = sector)) +
    geom_line(size = 0.3) +
    facet_wrap(~variable, labeller = labeller(variable = 
                                                c("Rem_relativa_eu" = "Brecha Salarial nominal vs Europa",
                                                  "Rem_relativa_eu_tcp" = "Brecha Salarial TCP vs Europa")),
               ncol =2) + 
    # scale_y_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::percent, 
                       breaks = scales::pretty_breaks(n = 10)) + # Add more ticks using 'breaks'
  
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
    scale_color_manual(values = custom_palette_2)+
    # scale_color_brewer(palette="Paired") +
    # viridis::scale_color_viridis(discrete = TRUE, option = "viridis") +
    labs(
      title = "Brecha Salarial de Inversiones de EEUU",
      subtitle = "Argentina Relativa a Total Europa (sectores seleccionados)",
      caption = "Fuente: elaboración propia en base a BEA"
    ) +
    coord_cartesian(expand = FALSE) # Prevent extra space around the plot
ggsave("./results/bea/majority_owned_nonbank/wage_arg_relativa_eu_2.png", width = 10, height = 6) # Adjust width and height
  


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
