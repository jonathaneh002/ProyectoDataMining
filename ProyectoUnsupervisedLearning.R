
library("ggplot2")
library("dplyr")
library('lubridate')
library(readxl)

############################################
#                                          #
#                                          #
#     Proyecto Unsupervised Learning       #              
#         Jonathan Espinoza 20022          #
#         Juan Andres Galicia 20278        #
#                                          #
#                                          #
############################################

df<- read_excel("Online Retail.xlsx")
df$date<-as.Date(df$InvoiceDate)



#a. ¿Qué tipos de datos identificamos? 
#b. ¿Cuál es la distribución de las variables univariadas? 
#c. Presentar por lo menos 2 tablas de contingencia que relacionen las variables. 
#d. Preguntas
#e. Presentar gráficas para responder las preguntas planteadas en elpunto anterior
#f. Modelos:
#   a. Clustering (recomendación aplicarlo a clientes) 
#   b. Association rules


#a)
str(df)
levels(df$Country)


#c)
df%>%
  distinct(InvoiceNo, Country)%>%
  group_by(Country)%>%
  count()%>%
  arrange(desc(n))

df%>%
  group_by(StockCode)%>%
  count()%>%
  arrange(desc(n))





