## DEMOCRACIA MUNICIPAL EN SALTA
## Autor: Franco Galeano
## 31/08/2020

##DATA WRANGLING

#Paquetes

library(tidyverse) # Easily Install and Load the 'Tidyverse'



## Agrupar transferencias por periodo para posterior analisis

transferencias <- read_csv2(file = "TTpercapSalta.csv", trim_ws = TRUE)

transferencias <- transferencias %>% 
  group_by(IDMUNICIPIO, MUNICIPIO, PERIOD) %>% 
  summarize(TFApcProm = round(mean(TFAPERCAP, na.rm = TRUE),2)) %>% 
  write_csv("TTpc_Salta_XPeriod.csv") # Guardo como csv

#Ver cuantos intendentes hay y el promedio de mandatos de cada uno
intendentes <- read_csv2(file = "intendentes.csv", trim_ws = TRUE) %>% group_by(INT) %>% 
  count() %>% ungroup() 

mean(intendentes$n)

summary(intendentes$n)
