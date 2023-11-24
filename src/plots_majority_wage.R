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
ppp_df_arg <- read.csv('./data/ppp/PPP.csv') %>% 
  filter(cod.variable == 'PPA_c_priv_serie'
         , iso3c == 'ARG')

ppp_df_ger <- read.csv('./data/ppp/PPP.csv') %>% 
  filter(cod.variable %in% c('PPA_c_priv_serie', 'TCN'), 
         iso3c =='DEU') %>% 
  pivot_wider(names_from = 'cod.variable', values_from = 'valor')

# Create a vector of unique sectors
sectors <- unique(data$sector)



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
            by = 'year')  %>% 
  # left_join(ppp_df_arg %>% 
  #             select(year = ANO4, 
  #                    PPP = valor),
  #           by = 'year') %>% 
  mutate(Rem_relativa_eu = Rem / RemEU ,
         Rem_tcp_arg = Rem * TCC / TCP_1,
         Rem_relativa_eu_tcp = Rem_tcp_arg / RemEU 
         # , Rem_ppp_arg = Rem * TCC / PPP  
         ) %>% 
  select(-c('RemEU'))

write.csv(salario_relativo_arg,
          "./results/bea/majority_owned_nonbank/salario_relativa_arg.csv",
          row.names=FALSE)


custom_palette_2 <- colorRampPalette(brewer.pal(10, "Set1"))(16)

salario_relativo_arg_melt <- salario_relativo_arg %>%
  # select(-c('Rem', 'Rem_tcp_arg', 'RemEU' )) %>% 
  filter(sector %in% c("All Industries Total",  "Chemicals",
                       "Machinery" , "Food" ,
                       "Primary and fabricated metals",
                       "Computers and electronic products",
                       "Mining", "Total Manufacturing") ) %>% 
  pivot_longer(cols = starts_with("Rem"), 
               names_to = "variable", 
               values_to = "value") 

salario_relativo_arg_melt %>%  
  filter(! variable %in%  c('Rem', 'Rem_tcp_arg', 'RemEU', 'Rem_ppp_arg' ) ) %>% 
  ggplot(aes(x=year,y= value, color = sector)) +
  geom_line(size = 0.3) +
  facet_wrap(~variable, labeller = labeller(variable = 
                                              c("Rem_relativa_eu" = "Salario relativo a Europa en USD nominal",
                                                "Rem_relativa_eu_tcp" = "Brecha relativo a Europa en USD TCP")),
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
    title = "Salario relativo de Inversiones de EEUU",
    subtitle = "Argentina Relativa a Total Europa (sectores seleccionados)",
    caption = "Fuente: elaboración propia en base a BEA"
  ) +
  coord_cartesian(expand = FALSE) # Prevent extra space around the plot
ggsave("./results/bea/majority_owned_nonbank/wage_arg_relativa_eu_2.png", width = 10, height = 6) # Adjust width and height

# SALARIO EN PPP
salario_relativo_arg_ppp <- salario_relativo_arg_melt %>% 
  filter(variable == 'Rem') %>% 
  select(year, sector, 
         TCC_ARG=TCC, TCP_ARG=TCP_1,
         PPP_ARG=PPP,
         Rem_ARG_USD = value) %>% 
  # left_join(ppp_df_arg %>%
  #             select(year = ANO4,
  #                    PPP = valor),
  #           by = 'year') %>%
  left_join(data %>% 
              filter(country %in% c("Germany")) %>% 
              select(year, sector, Rem_GER_USD=Rem) ,
            by = c("year", "sector" )) %>% 
  left_join(ppp_df_ger %>% 
              select(year = ANO4,
                     PPP_GER =PPA_c_priv_serie,
                     TCC_GER=TCN)) %>% 
mutate(Rem_ARG_PPP = Rem_ARG_USD * TCC_ARG / PPP_ARG,
       Rem_GER_PPP = Rem_GER_USD * TCC_GER / PPP_GER,
       Rem_rel_arg_ger_PPP = Rem_ARG_USD/Rem_GER_USD )

write.csv(salario_relativo_arg_ppp,
          './results/bea/majority_owned_nonbank/salario_relativa_arg_ppp.csv')

salario_relativo_arg_ppp %>% 
  select(c(year, sector, Rem_ARG_PPP, Rem_rel_arg_ger_PPP )) %>% 
  pivot_longer(cols = starts_with("Rem"), 
               names_to = "variable", 
               values_to = "value") %>% 
  ggplot(aes(year,value, color = sector)) +
  geom_line() + 
    geom_line(size = 0.3) +
    facet_wrap(~variable, labeller = labeller(variable = 
                                                c("Rem_ARG_PPP" = "Salario en USD PPP",
                                                  "Rem_rel_arg_ger_PPP" = "Salario relativo a Alemania en USD PPP")),
               ncol =2
              ,scales = 'free'
              ) + 
    scale_y_continuous(#labels = scales::percent, 
                       breaks = scales::pretty_breaks(n = 10)) + # Add more ticks using 'breaks'

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
    labs(
      title = "Salario relativo en PPP de Inversiones de EEUU",
      subtitle = "Argentina Relativa a Alemania (sectores seleccionados)",
      caption = "Fuente: elaboración propia en base a BEA"
    ) +
    coord_cartesian(expand = FALSE) # Prevent extra space around the plot


salario_relativo_arg_ppp %>% 
  select(c(year, sector, Rem_rel_arg_ger_PPP )) %>% 
 
  ggplot(aes(year,Rem_rel_arg_ger_PPP, color = sector)) +
  geom_line() + 
  geom_line(size = 0.3)+
  scale_y_continuous(labels = scales::percent, 
    breaks = scales::pretty_breaks(n = 10)) + # Add more ticks using 'breaks'
  
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
  labs(
    title = "Salario relativo en PPP de Inversiones de EEUU",
    subtitle = "Argentina Relativa a Alemania (sectores seleccionados)",
    caption = "Fuente: elaboración propia en base a BEA"
  ) +
  coord_cartesian(expand = FALSE) # Prevent extra space around the plot
ggsave("./results/bea/majority_owned_nonbank/salario_relativo_alemania.png", width = 15, height = 10)
