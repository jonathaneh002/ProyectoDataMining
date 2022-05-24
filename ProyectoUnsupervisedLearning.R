
library("ggplot2")
library("dplyr")
library('lubridate')


############################################
#                                          #
#                                          #
#     Proyecto Unsupervised Learning       #              
#         Jonathan Espinoza 20022          #
#         Juan Andres Galicia 20278        #
#                                          #
#                                          #
############################################
df<-read.xlsx2("Online Retail.xlsx", sheetIndex = 1, header = T, rows=10)



#a. ¿Qué tipos de datos identificamos? 
#b. ¿Cuál es la distribución de las variables univariadas? 
#c. Presentar por lo menos 2 tablas de contingencia que relacionen las variables. 
#d. Preguntas
#e. Presentar gráficas para responder las preguntas planteadas en elpunto anterior
#f. Modelos:
#   a. Clustering (recomendación aplicarlo a clientes) 
#   b. Association rules


