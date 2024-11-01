library(tidyverse)
library(readxl)
library(lubridate)
source("./src/functions.R")

#directory
results_path = './results/bea/majority_owned_nonbank/'

#plots params
title_size=40
text_size= 30
axis_size= 5
strip_size= 6

# load data
data <- read.csv("./results/bea/majority_owned_nonbank/data_majority_owned_nonbank_procesado.csv") 
# Create a vector of unique sectors
sectors <- unique(data$sector)

tcp_df <- read.csv('./results/indice_tcp.csv')

# ISO Codes
isocodes<- read_excel("./data/ocde/34107835.xls")  %>%
  select('CODE','country'=  'Country')

# PPI
## PPI benchmark
ppi <- read.csv("./data/ocde/DP_LIVE_27112023180609408.csv") %>% 
  filter(SUBJECT == 'TOT_MKT' , FREQUENCY == 'A', MEASURE == 'IDX2015') %>% 
  mutate(fecha = parse_date_time(TIME, orders = "y"),
         fecha = ymd(fecha),
         TIME = as.numeric(TIME)) %>%
  group_by(LOCATION) %>%
  # mutate(ppi_97 = generar_indice(Value, 'fecha', "1997-01-01"))
  mutate(ppi_99 = generar_indice(serie=Value, fecha=TIME, fecha_base=1999)) %>% 
  ungroup()


# PPI 
## PPI ARG
ppi_arg <- read_csv('./data/arg/ppi_arg.csv') %>% 
  mutate(ppi_99 = generar_indice(ppi_04, year, 1999))

# Productivity (PT)
data_prod_0 <- data %>%
  select(c(sector, country, Continent , year,value_added, employment, PT)) %>%
  # filter(PT >= 0 &  employment > 0 ) %>%
  filter(  employment > 0 ) %>%
  left_join(isocodes, by = 'country') %>% 
  left_join(ppi %>% 
              select(country = 'LOCATION', 
                     year = 'TIME',
                     ppi_99),
            by = c('CODE'='country', 'year'='year')
  )





data_prod <- data_prod_0 %>%
  select(-'PT') %>% 
  pivot_longer(cols = c(value_added, employment),
               names_to = "variable", values_to = "value") %>% 
  pivot_wider(names_from = sector, 
              # values_from = value,
              # id_cols = c(  "country",  "Continent",  "year", "CODE", "ppi_99", "value"  )
              ) %>% 
  mutate(
    `Manufacturing without Transportation Equipment` = 
      `Total Manufacturing` - `Transportation Equipment`,
    `Manufacturing without Food` = 
      `Total Manufacturing` - `Food`
    ) %>% 
  pivot_longer(cols = -c(country, Continent, year, CODE, ppi_99, variable),
               names_to = "sector",
               values_to = "value") %>% 
  pivot_wider(names_from = variable) %>% 
  mutate(PT = value_added/employment)
  
 

# Print the countries with NA in CODE
na_codes <- data_prod %>% filter(is.na(CODE))
print(unique(na_codes$country))

df_manuf <- data_prod #%>% 
  # filter(sector == "Total Manufacturing"  )

df_manuf_arg <- df_manuf %>% 
  arrange(year)%>% 
  filter(CODE == 'ARG') %>% 
  left_join(tcp_df %>% 
              select(year, TCC,  TCP_1),
            by= 'year')  %>% 
  select(-ppi_99) %>% 
  left_join(ppi_arg %>%
              select(year, ppi_99),
            by= 'year') %>% 
  group_by(sector) %>% 
  mutate(ipt_arg_99 = (PT*TCC) / ppi_99 ,
         ipt_arg_99_index =  generar_indice(ipt_arg_99, year, 1999)) %>% 
  select(year, country, sector, PT_arg=PT,  ppi_arg_99=ppi_99, ipt_arg_99, ipt_arg_99_index) %>% 
  ungroup()


df_manuf_eu <- df_manuf %>% 
  filter(Continent == 'Europe') %>% #, country!= 'Austria') %>% 
  filter(!is.na(ppi_99))  %>% 
  mutate(ipt_bench_99 = PT/ppi_99) %>%
  group_by(country, sector) %>% 
  arrange(year)%>% 
  mutate(ipt_bench_99_index =  generar_indice(ipt_bench_99, year, 1999)) %>%
  ungroup() %>% 
  select(year, country,sector, PT_bench=PT, ppi_bench_99=ppi_99, ipt_bench_99, ipt_bench_99_index)

df_manuf_benchmark <- df_manuf %>% 
  # filter(Continent != '')
  filter(Continent == 'Europe' , year == 1999) %>% 
  select(year, country, sector, PT_bench_base = PT) %>% 
  left_join( df_manuf_arg %>% 
               filter(year == 1999) %>% 
                    left_join(tcp_df %>% 
                                select(year, TCC, TCP_1),
                              by= 'year') %>% 
               select(year,sector, PT_arg_base = PT_arg, TCC, TCP_1),
             by = c('year', 'sector') ) %>% 
  mutate(brecha_anio_base = (PT_arg_base*TCC/TCP_1)/PT_bench_base )

df_manuf_brecha <- df_manuf_arg %>% 
  select(-country) %>% 
  right_join(df_manuf_eu,
            by=c('year', 'sector')) %>% 
  left_join(df_manuf_benchmark %>% 
              select(year, country, sector, brecha_anio_base ),
            by=c('year', 'country', 'sector'))

#SAVE
write.table(df_manuf_brecha,
            './results/bea/majority_owned_nonbank/brecha_productividad_all_sectors.csv',
          row.names = F
         # , dec = ','
         #  ,sep = ';'
          )

