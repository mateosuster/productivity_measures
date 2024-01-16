
library(tidyverse)
library(readxl)
library(ggplot2)
source("C:/Archivos/repos/pioYPFConicet/analisis/codigos/functions_hidrocarburos.R")

prepro <- function(x, name , type =F ) {
  a <- 21+(19*2)
  b <-  a + 18
  
  if(type == "all"){
    c =83*19+1  
  }else{c =83*19 }
  
  
  x %>% 
    select(c(1:20, a:b) ) %>% 
    head(83) %>% 
  gather(key = "anio", value = "valor", 2:39) %>%
  mutate(anio = as.double(substr(anio, 1, 4)) ,
         valor = as.double(valor),
         var = name,
         sector = ifelse(row_number() %in% 1:c, "All Industries Total", "Total Manufacturing"  )) %>%
         # sector = ifelse(row_number() %in% 1:c, "Total sectores", "Total manufactura"  )) %>%
  rename(pais = "...1") 
  # ,
  #          sector =case_when( 1:5 ~ "All_industries_total",
  #                             6:10 ~ "Mining"))
  
}


##############
# 1998 - 2017
##############
#millions of USD
dt_income  <- read_excel("C:/Archivos/datos/bea/download (38).xls", 
                           sheet = "Sheet0", skip = 7)  %>%  
  prepro(., name = "direct_investment_income", type ="all")

df_position  <- read_excel("C:/Archivos/datos/bea/download (39).xls", 
                         sheet = "Sheet0", skip = 7) %>% 
  prepro(., name = "direct_investment_position")


#############
# 1982 - 1998
#############
df_income_82 <- read_csv("C:/Archivos/datos/bea/other_data/income_82_98.csv", skip = 5)
names(df_income_82) <- c("pais", paste(df_income_82[1,],df_income_82[2,])[-1])
df_income_82 <-  df_income_82 %>% 
  slice(3:85) %>% 
  gather(key = "vble", value = "valor", 2:ncol(.)) %>% 
  mutate(anio = as.numeric(str_extract(vble, "[0-9]+")) ,
         sector = gsub("[0-9]+", "", vble), 
         sector = gsub(".{1}$", "", sector),
         vble = "income", 
         valor = as.double(valor)) 


df_position_82 <- read_csv("C:/Archivos/datos/bea/position_82_98.csv", skip = 5)
names(df_position_82) <- c("pais", paste(df_position_82[1,],df_position_82[2,])[-1])
df_position_82 <-  df_position_82 %>% 
  slice(3:85) %>% 
  gather(key = "vble", value = "valor", 2:ncol(.)) %>% 
  mutate(anio = as.numeric(str_extract(vble, "[0-9]+")) ,
         sector = gsub("[0-9]+", "", vble), 
         sector = gsub(".{1}$", "", sector),
         vble = "position", 
         valor = as.double(valor)) 

#######
# JOIN
######
data <- dt_income %>% 
  filter(pais != "Other") %>% 
  select(pais, anio,income = valor , sector) %>% 
  left_join(df_position %>%
              select(pais, anio, position=valor, sector) 
            , by =c("pais", "anio", "sector")) %>% 
  rbind(df_income_82 %>% 
          filter(pais != "Other", sector %in% c("All Industries Total", "Total Manufacturing" )) %>% 
          select(pais, anio,income = valor , sector) %>% 
          left_join(df_position_82 %>%
                      select(pais, anio, position=valor, sector) 
                    , by =c("pais", "anio", "sector")) ) %>% 
  mutate(tg = income/position,
         pais = case_when(pais == "All Countries Total" ~ "Total países", T~ pais),
         sector = case_when(sector == "All Industries Total" ~ "Total sectores",
                            sector == "Total Manufacturing" ~ "Total manufactura" , T ~sector))


####################################
# PLOT
#####################################
plot <-  data %>% 
  filter(pais %in% c("Argentina", "Venezuela", "Total países")) %>% 
  ggplot(aes(anio, tg*100, color = pais))+
  geom_line()+
  facet_wrap(~sector)+
  theme(legend.position = "bottom",
        axis.title.x  = element_blank(),
        legend.title = element_text("País"))+
  labs(y = "%", x = "")+
  scale_color_manual(name = "País" , 
                     values=c(  "dodgerblue1", "gray1", "#CC6666"))
plot_tg = plot_theme(plot+theme(axis.title.x  = element_blank()))
ggsave("C:/Archivos/repos/pioYPFConicet/analisis/resultados/comparacion_paises/tg_ied_eeuu.png",
       plot = plot_tg, height = 5, width = 10)


write.csv(data %>% 
            filter(pais %in% c("Argentina", "Venezuela", "Total países")) %>% 
            select(pais, anio, sector, tg),
          "../resultados/comparacion_paises/tg_ied_eeuu.csv",
          row.names = F)

#########################
