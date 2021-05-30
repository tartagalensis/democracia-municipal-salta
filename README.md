# Democracia Municipal en Salta

Este repo incluye scripts y datos para replicar el analisis realizado en:
Galeano, Franco (2021): Transferencias, desarrollo y alineamiento. Determinantes de la democracia
municipal en Salta (1987-2019), Revista SAAP Vol. 15, Nº 1, mayo 2021, 73-102

https://doi.org/10.46468/rsaap.15.1.A3

## Datos:
*variables_IDS.csv* 

* MUNICIPIO: Nombre del municipio
* IDYEAR: ID del mandato
* IDMUNICIPIO: ID del Municipio
* IDFILA: IDYEAR + IDMUNICIPIO
* INT: Nombre intendente
* PARTINT: Partido del intendente
* PORC_VALVOT: Porcentaje de votos válidos
* BANC_GAN: Bancas ganadas en el Concejo Deliberante (CD) cuando es electo
* PORC_BANGAN: Porcentaje de bancas ganadas en el CD cuando es electo
* BANCAS: Número total de bancas del Concejo Deliberante
* BANCASMT: Bancas ganadas por el partido del intendente en el Concejo Deliberante (CD) - Elección de Medio Término
* PORC_BANCASMT: Porcentaje de bancas ganadas por el partido del intendente en el Concejo Deliberante (CD) - Elección de Medio Término
* CONTROL_SUC: Variable control de sucesión -> 0 cuando es baja (Gana la oposición), 1 cuando es media (Gana el mismo partido o un aliado) y 2 cuando es alta (Reelige intendente o gana algún familiar/persona de mucha cercanía)
* INTERRUP: 1 si el mandato fue interrumpido

*base_analisis_resultados.xlsx*

* En la hoja panel se encuentran las variables necesarias para realizar la regresión

## Scripts:

*DataWrangling.R*
Ordenado y transformación de datos

*IndiceDemoMunicipal.R*
Se construye el índice de democracia municipal utilizado luego en el archivo de las regresiones

*Regresiones.R*
Modelado 
