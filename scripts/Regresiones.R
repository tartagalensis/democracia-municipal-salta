library(readxl)
library(tidyverse)
library(plm)
library(texreg)
library(zoo)


panel <- read_excel("data/base_analisis_resultados.xlsx",sheet = "panel") %>% 
  mutate(transferencias = TTPERCAP/100, interaccion = (NBIPP * TTPERCAP/100)) %>% 
  select(MUNICIPIO, IDMUNICIPIO, IDYEAR, ids, transferencias, NBIPP, ANALFPP, CERCANIA, interaccion) %>% 
  print()


# POOLED OLS ####
pooled <- lm(ids ~ transferencias + NBIPP + ANALFPP + CERCANIA + interaccion, data = panel)
summary(pooled)



# FIXED EFFECTS ####
fe <- plm(ids ~ transferencias + NBIPP + ANALFPP + CERCANIA + interaccion,
          data = panel,
          index = c("MUNICIPIO", "IDYEAR"))

summary(fe)

# RANDOM EFFECTS ####

re <- plm(ids ~ transferencias + NBIPP + ANALFPP + CERCANIA + interaccion,
          data = panel,
          index = c("MUNICIPIO", "IDYEAR"),
          model = "random")
summary(re)




fe_1 <- plm(ids ~ transferencias + NBIPP,
                data = panel,
                index = c("MUNICIPIO", "IDYEAR"))

fe_2 <- plm(ids ~ transferencias + CERCANIA,
            data = panel,
            index = c("MUNICIPIO", "IDYEAR"))

fe_3 <- plm(ids ~ transferencias + NBIPP + CERCANIA,
            data = panel,
            index = c("MUNICIPIO", "IDYEAR"))

fe_4 <-  plm(ids ~ transferencias + NBIPP + interaccion,
             data = panel,
             index = c("MUNICIPIO", "IDYEAR"))

#Mejor modelo de FE
fe_5 <- plm(ids ~ transferencias + NBIPP + CERCANIA + interaccion,
            data = panel,
            index = c("MUNICIPIO", "IDYEAR"))

#Mejor modelo de RE
random_5 <- plm(ids ~ transferencias + NBIPP + CERCANIA + interaccion,
            data = panel,
            index = c("MUNICIPIO", "IDYEAR"),
            model = "random")



#Test de Hausman
phtest(fe_5,random_5)

fe_models <- list(fe_1, fe_2, fe_3, fe_4, fe_5)


## RESULTS ####
screenreg(fe_models,
          custom.model.names = c("Modelo 1", "Modelo 2", "Modelo 3","Modelo 4", "Modelo 5"),
          custom.coef.names = c("Transferencias PC","NBI PC", "Cercanía Gob","Transferencias * NBI"),
          single.row = TRUE)

# EXPORT RESULTS
htmlreg( list(fe_1, fe_2, fe_3, fe_4, fe_5),
         file = "modelos_efectos_fijos.doc", 
         custom.model.names = c("Modelo 1", "Modelo 2", "Modelo 3","Modelo 4", "Modelo 5"),
         custom.coef.names = c("Transferencias PC","NBI PC", "Cercanía Gob","Transferencias * NBI"),
         single.row = TRUE,
         inline.css = FALSE,
         doctype = T,
         html.tag = T, 
         head.tag = T,
         body.tag = T)


library(ggcorrplot)

corr_selected <- panel %>% 
  select(ids, transferencias,NBIPP,CERCANIA) %>% 
  # calcular la matriz de correlación y redondear a un decimal
  cor(use = "pairwise") %>% 
  round(4)

ggcorrplot(corr_selected, type = "lower", lab = T, show.legend = F)


library(lmtest)

coeftest(fe, vcov. = function(x){vcovHC(x, type = "sss")}) 

coeftest(re, vcov. = function(x){vcovHC(x, type = "sss")}) 


coeftest(fe, vcov = vcovBK, type = "HC1", cluster = "time")

coeftest(re, vcov = vcovBK, type = "HC1", cluster = "time")


summary(panel$NBIPP)

summary(panel$TTPERCAP)

summary(panel)
