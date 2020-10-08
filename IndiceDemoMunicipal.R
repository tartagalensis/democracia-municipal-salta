## DEMOCRACIA MUNICIPAL EN SALTA
## Autor: Franco Galeano
## 31/08/2020

## INDICE DE DEMOCRACIA MUNICIPAL
## Se construye indice de democracia municipal en base a PCA
## Se compara con Factor Analysis


## Este script utiliza partes de código del cap 15 del libro AnalizaR Datos Políticos
## Autores del cap: Caterina Labrín y Francisco Urdinez
# Link: https://arcruz0.github.io/libroadp/indexes.html


#Paquetes
library(FactoMineR) # Multivariate Exploratory Data Analysis and Data Mining, CRAN v2.3
library(factoextra) # Extract and Visualize the Results of Multivariate Data Analyses, CRAN v1.0.7
library(GGally) # Extension to 'ggplot2', CRAN v2.0.0
library(ggcorrplot) # Visualization of a Correlation Matrix using 'ggplot2', CRAN v0.1.3
library(haven) # Import and Export 'SPSS', 'Stata' and 'SAS' Files, CRAN v2.3.1
library(tidyverse) # Easily Install and Load the 'Tidyverse' 
library(here)
library(glue)
library(officer)
library(rvg)
library(viridis)
library(readxl)
# 1- LOAD DATA ####

# 1-A Cargo base de datos
demo <- read_csv2("variables_IDS.csv")

# 1 - B Transformación
demo_analisis <- demo %>% 
  filter(INTERRUP == 0, #Elimino mandatos interrumpidos
         IDYEAR != 1987, # Quito elecciones 1987 y 2019 (data incompleta)
         IDYEAR != 2019) %>% 
  mutate(comp_eje = (1 - PORC_VALVOT), #Creo competencia ejecutiva (1 - % votos validos)
         comp_leg = (1 - PORC_BANCASMT), # Creo competencia legislativa
         control_cd = PORC_BANGAN, #Creo control cd
         control_suc = CONTROL_SUC) %>%  #Creo control de sucesion
  select(MUNICIPIO, IDYEAR, IDMUNICIPIO, #Selecciono variables para trabajar
         INT, PARTINT, comp_eje,
         comp_leg, control_cd, control_suc) %>% 
  na.omit() #Elimino na (no se eliminan filas)

## Voy a utilizar los mismos datos para comparar pca vs factor analysis
write_csv2(demo_analisis, "CSVparaIDSstata.csv") #Guardo como csv 

# 1- C Escalo los datos (control suc tiene escala diferente al resto)
demo_escalado <- demo_analisis %>% 
  select(comp_eje,comp_leg,
         control_cd, control_suc) %>% 
  mutate_all(as.numeric) %>% 
  scale() %>% 
  na.omit() %>% 
  as_tibble()


# 2- PCA ####

# 2 - A - Corr
## Analizo correlacion entre componentes del IDS
corr_escalado <- demo_escalado %>% 
  cor(use = "pairwise") %>% 
  round(1)


#Corrplot
ggcorrplot(corr_escalado, type = "lower", lab = T, show.legend = F) 

## Analisis de copmonentes principales
pca_escalado <- princomp(demo_escalado)

summary(pca_escalado, loadings = T, cutoff = 0.3)


# Otra forma de presentar mismo resultado 
get_eigenvalue(pca_escalado)

## Scree Plot
fviz_eig(pca_escalado, choice = c("eigenvalue"), addlabels = T, ylim = c(0, 3))


# BiPlot del PCA
fviz_pca_biplot(pca_escalado, repel = F, col.var = "black", col.ind = "gray")


# Dimensión 1 -> que variables la explican: Comp_eje, control_cd y comp_leg
fviz_contrib(pca_escalado, choice = "var", axes = 1, top = 10)

# Dimensión 2 -> que variables la explican: Control_suc
fviz_contrib(pca_escalado, choice = "var", axes = 2, top = 10)



# 3- DEMO-INDEX ####
#Realizo PCA (otra forma de hacerlo)
pca_1 <- PCA(demo_escalado, graph = T)

#Eigenvalores
get_eig(pca_1)

# Creo indice de democracia municipal
data_pca_escalada <- pca_1$ind$coord %>% 
  as_tibble() %>% 
  mutate(pca_01 = (Dim.1 * 55.49 + Dim.2 * 23.52)/ 79.01) #Cada dimension con Eigenvalor >1 * peso / varianza conjunta

# Almaceno indice
ids_escalado_pca <- bind_cols(demo_analisis, data_pca_escalada) %>% 
  mutate(ids = GGally::rescale01(pca_01) * 100) %>% #Escalado entre 1 y 100
  select(ids, everything())


# Agrupo por municipios
ids_escalado_pca %>%
  group_by(MUNICIPIO) %>%
  select(ids) %>%
  summarize(ids = round(mean(ids, na.rm = TRUE),2)) %>% 
  view()




# 4- PCA vs Factor Analysis ####
# comparamos resultados PCA con Factor Analysis

# Cargo IDS de STATA
stata <- read_dta("ids_FactorAnalysis.dta") %>% 
  mutate(ids_FA = GGally::rescale01(ids) * 100) %>% 
  select(ids_FA, everything())

# PCA
uno <- ids_escalado_pca %>%
  mutate(IDYEAR = as.character(IDYEAR),
         IDMUNICIPIO = as.character(IDMUNICIPIO)) %>% 
  select(MUNICIPIO, IDMUNICIPIO, IDYEAR, ids) %>% 
  print()

#Factor analysis
dos <- stata %>% select(IDMUNICIPIO, IDYEAR, ids_FA) %>%
  print()

# Uno bases
comparacion <- uno %>% 
  left_join(dos, by=c("IDMUNICIPIO", "IDYEAR")) %>% 
  mutate(diferencia = (ids - ids_FA)) #Creo variable de diferencia


# Estadisticas descriptivas
summary(comparacion)

#Promedio diferencias
mean(comparacion$diferencia)

# Matriz de correlaciones
comparacion %>% 
  select(ids, ids_FA, diferencia) %>% 
  cor(use = "pairwise") %>% 
  round(3)

## Correlaciones entre IDS y sus componentes (PCA)
corrplot_1 <- ids_escalado_pca %>%
  mutate(Comp_Ejecutiva = comp_eje, Comp_Legis = comp_leg, Control_CD = control_cd,
         Control_Sucesion = control_suc, IDS = pca_01) %>% 
  select(Comp_Ejecutiva, Comp_Legis,Control_CD, Control_Sucesion, IDS) %>%
  cor(use = "pairwise") %>% 
  round(3)

## CORRPLOT ENTRE IDS Y COMPONENTES
ggcorrplot(corrplot_1, type = "lower", lab = T) +
  theme_void() +
  theme(axis.text.y=element_text(size=25),
        axis.text.x = element_text(size=25)) 
  

# 5- Save Results ####
## Guardo resultados finales
ids_escalado_pca %>% 
  select(MUNICIPIO, IDYEAR, IDMUNICIPIO, INT,
         PARTINT, comp_eje, comp_leg, control_cd,
         control_suc, ids) %>% 
  write_csv2("IDS_ResultadosGaleano.csv")


# PLOTS AND TABLES ####

## Media del IDS por mandato electoral (TABLA 3) ####

ids_escalado_pca %>% 
  select(ids,IDYEAR) %>% 
  group_by(IDYEAR) %>% 
  summarise(mean = mean(ids, na.rm = T), sd = sd(ids, na.rm = TRUE))


# Correlacion entre componentes del IDS (TABLA 4) ####
ids_escalado_pca %>% select(ids, comp_eje, comp_leg, control_cd,
                            control_suc) %>% 
  cor(use = "pairwise") %>% 
  round(3)

ids_escalado_pca %>% 
  select(ids, IDYEAR, MUNICIPIO) %>% view()

# Boxplot de competencia ejecutiva (Grafico 1) ####
boxplot_grafico1 <- ids_escalado_pca %>% 
  select(comp_eje, IDYEAR) %>% 
  filter(IDYEAR != 2013) %>% 
  filter(IDYEAR != 2017) %>% 
  mutate( IDYEAR = as.character(IDYEAR), `Competencia Ejecutiva` = comp_eje) %>% 
  ggplot(aes(IDYEAR,  `Competencia Ejecutiva`)) +
  geom_boxplot() +
  theme_void() +
  theme(axis.text.y=element_text(size=25),
        axis.text.x = element_text(size=30),
        axis.title.y = element_text(size=25, angle = 90)) 

#Ggplot a rvg
p_dml <- rvg::dml(ggobj = boxplot_grafico1)

officer::read_pptx() %>%
  officer::add_slide() %>%
  officer::ph_with(p_dml, ph_location()) %>%
  base::print(
  target = here::here("demo_two.pptx"
  )
)
#Media de Control de sucesión ####
mean(ids_escalado_pca$control_suc)
#Media de Competencia Ejecutiva ####
mean(ids_escalado_pca$comp_eje)


## Grafico 2 ####
seriesdetiempo <- read_excel("base_analisis_resultados.xlsx",sheet = "panel")

boxplot_2 <- seriesdetiempo %>% 
  filter(MUNICIPIO != "TOLAR GRANDE") %>% 
  select(IDYEAR, TTPERCAP) %>% 
  filter(IDYEAR != 2013) %>% 
  filter(IDYEAR != 2017) %>% 
  group_by(IDYEAR) %>% 
  mutate(TTPERCAP = TTPERCAP /100) %>% 
  mutate( IDYEAR = as.character(IDYEAR), `Transferencias PC` = TTPERCAP) %>% 
  ggplot(aes(IDYEAR,  `Transferencias PC`)) +
  geom_boxplot() +
  theme_void() +
  theme(axis.text.y=element_text(size=25),
        axis.text.x = element_text(size=30),
        axis.title.y = element_text(size=25, angle = 90)) 


#Ggplot a rvg
p_dml2 <- rvg::dml(ggobj = boxplot_2)

officer::read_pptx() %>%
  officer::add_slide() %>%
  officer::ph_with(p_dml2, ph_location()) %>%
  base::print(
    target = here::here("demo_two.pptx"
    )
  )
## Estadisticas descriptivas VIs - VD - Tabla 5 ####

seriesdetiempo %>% 
  mutate(TTPERCAP = TTPERCAP /100) %>% 
  select(ids, TTPERCAP, NBIPP, ANALFPP, CERCANIA) %>% 
  summarise_all(mean)

seriesdetiempo %>% 
  mutate(TTPERCAP = TTPERCAP /100) %>% 
  select(ids, TTPERCAP, NBIPP, ANALFPP, CERCANIA) %>% 
  summarise_all(sd)

seriesdetiempo %>% 
  mutate(TTPERCAP = TTPERCAP /100) %>% 
  select(ids, TTPERCAP, NBIPP, ANALFPP, CERCANIA) %>% 
  summarise_all(min)

seriesdetiempo %>% 
  mutate(TTPERCAP = TTPERCAP /100) %>% 
  select(ids, TTPERCAP, NBIPP, ANALFPP, CERCANIA) %>% 
  summarise_all(max)

seriesdetiempo %>% 
  mutate(TTPERCAP = TTPERCAP /100) %>% 
  select(ids, TTPERCAP, NBIPP,CERCANIA) %>% 
  count()

## Cuadro anexo (Tabla 2) ####
tabla <- ids_escalado_pca %>% 
  select(MUNICIPIO, comp_eje, comp_leg, control_cd, control_suc, ids) %>% 
  group_by(MUNICIPIO) %>% 
  summarise(comp_eje = mean(comp_eje, na.rm = T),
            comp_leg = mean(comp_leg, na.rm = T),
            control_cd = mean(control_cd, na.rm = T),
            control_suc = mean(control_suc, na.rm = T),
            ids_mean = mean(ids, na.rm = T),
            sd = sd(ids, na.rm = TRUE))
  
frecuencia <- demo_analisis %>% group_by(MUNICIPIO) %>% tally() %>% print()

tabla_2 <- tabla %>% left_join(frecuencia) %>% arrange(ids_mean) %>% janitor::adorn_totals()

tabla_2 %>% write_csv2("Tabla_IDSxMunicipio.csv")


## IDS POR PERIODO ELECTORAL GRAFICO PPT ####

ids_escalado_pca %>% 
  select(ids, IDYEAR) %>% 
  filter(IDYEAR != 2013) %>% 
  filter(IDYEAR != 2017) %>% 
  mutate( IDYEAR = as.character(IDYEAR)) %>% 
  ggplot(aes(IDYEAR,  ids)) +
  geom_boxplot() +
  theme_void() +
  theme(axis.text.y=element_text(size=25),
        axis.text.x = element_text(size=30),
        axis.title.y = element_text(size=25, angle = 90)) 
