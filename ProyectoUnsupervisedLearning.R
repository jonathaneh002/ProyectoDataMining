
library("ggplot2")
library("dplyr")
library('lubridate')
library(readxl)
library(countrycode)
library(tidyr)
library(purrr)
library(dendextend)
library(cluster)
library(weights)
library(arules)
library(ggmap)
library(geosphere)


############################################
#                                          #
#                                          #
#     Proyecto Unsupervised Learning       #              
#         Jonathan Espinoza 20022          #
#         Juan Andres Galicia 20278        #
#                                          #
#                                          #
############################################
df <- read_excel('Online Retail.xlsx')
df$date<-as.Date(df$InvoiceDate)



#a. ¿Qué tipos de datos identificamos? -------------------------------------------------------------

str(df)
levels(df$Country)

#b. ¿Cuál es la distribución de las variables univariadas? -----------------------------------------

summary(df)

#Stockcode
StockCode_head <- df %>% 
  group_by(StockCode) %>% 
  count() %>%
  arrange(desc(n)) %>%
  head(25)

StockCode_head <- as.data.frame(StockCode_head)

ggplot(StockCode_head, aes(x=StockCode, y=n)) +
  geom_col(fill=rgb(0.1,0.4,0.5,0.7)) +
  xlab('Codigo de Producto') +
  ylab('Cantidad')+
  ggtitle('25 productos mas comprados')
  

#Quantity
Quantity <- df %>% 
  group_by(Quantity) %>% 
  count() %>%
  arrange(desc(n)) %>%
  head(25)

Quantity <- as.data.frame(Quantity)

ggplot(Quantity, aes(x=Quantity, y=n))+
  geom_col(fill=rgb(0.1,0.4,0.5,0.7), color= 'white') +
  xlab('Cantidad') +
  ylab('Cuenta')+
  ggtitle('25 cantidades más compradas')


#InvoiceDate
max(df$InvoiceDate)
min(df$InvoiceDate)

df2 <- mutate(df, Date = date(df$InvoiceDate))

df2 %>% 
  group_by(Date) %>%
  count() %>%
  arrange(desc(n)) %>%
  ggplot(aes(x=Date, y=n))+
  geom_col(fill=rgb(0.1,0.4,0.5,0.7), color= 'white') +
  xlab('Fecha') +
  ylab('Transacciones')+
  ggtitle('Transacciones por fecha')
 


#UnitPrice
summary(df$UnitPrice)

ggplot(df, aes(x=UnitPrice)) +
  geom_boxplot(fill=rgb(0.1,0.4,0.5,0.7), color="black")+
  xlim(0, 25) + 
  coord_flip() +
  xlab('Precio')+
  ggtitle('Distribucion de precios')

#Country

df %>%
  group_by(Country) %>%
  count() %>%
  arrange(desc(n)) %>%
  ggplot(aes(x='', y = n, fill= Country))+
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  ggtitle('Transacciones por pais')

#c. Presentar por lo menos 2 tablas de contingencia que relacionen las variables. -------------------------------------------

df%>%
  distinct(InvoiceNo, Country)%>%
  group_by(Country)%>%
  count()%>%
  arrange(desc(n))

df%>%
  group_by(StockCode)%>%
  count()%>%
  arrange(desc(n))

#d. Preguntas-------------------------------------------------------------------------------------------------------------

#¿Los clientes son recurrentes o solo compran en una ocasión? 
sum(is.na(df$CustomerID))

df%>%
  distinct(InvoiceNo, CustomerID)%>%
  group_by(CustomerID)%>%
  count()%>%
  arrange(desc(n))%>%
  ggplot(aes(x=CustomerID, y=n))+
  geom_col()


#   ¿Los precios se comportan diferentes según la región?
df2 <- mutate(df, Region = (countrycode(sourcevar = df$Country, origin = "country.name",destination = "region")))
df2$Region <- as.factor(df2$Region)   


#¿La cantidad de productos diferentes que compran los clientes varían por región? 

df2%>%
  distinct(StockCode, Region)%>%
  group_by(Region)%>%
  count()%>%
  ggplot(aes(x=Region, y=n))+
  geom_col()+
  labs(y="Cantidad")+
  ggtitle("Cantidad de productos diferentes por region")+
  theme_bw()

#-----------------------------------------------------------------

#¿Las ventas presentan alguna estacionalidad por mes? 
df2 <- mutate(df2, Month = month(df2$date), Year = year(df2$date))
df2$Year <- as.factor(df2$Year)


Ventas_mes <- df2[df2$Year == '2011', ] %>%
  distinct(InvoiceNo, Month) %>%
  group_by(Month) %>%
  count() 

Ventas_mes <- as.data.frame(rbind(Ventas_mes, (df2[df2$Year == '2010', ] %>%
                                  distinct(InvoiceNo, Month) %>%
                                  group_by(Month) %>%
                                  count())))

Ventas_mes[13, 1] <- 0


ggplot(Ventas_mes, aes(x=as.factor(Month), y=n))+
  geom_col(fill=rgb(0.1,0.4,0.5,0.7), color= 'white') +
  xlab('Mes (0 como 12/2010)') +
  ylab('Transacciones')+
  ggtitle('Transacciones por mes')

#-----------------------------------------------------------------

#¿Transacciones por mes de cada pais?
Mes_pais <- as.data.frame(df2[df2$Year == '2011', ] %>%
  distinct(InvoiceNo, Month, Country) %>%
  group_by(Country, Month ) %>%
  count() )

Mes_pais <- as.data.frame(rbind(Mes_pais, (df2[df2$Year == '2010', ] %>%
                                                 distinct(InvoiceNo, Month, Country) %>%
                                                 group_by(Country, Month ) %>%
                                                 count())))

Mes_pais[291:314, 2] <- 0


ggplot(Mes_pais, aes(x=as.factor(Month), y=n))+
  geom_col(fill=rgb(0.1,0.4,0.5,0.7), color= 'white') +
  xlab('Mes (0 como 12/2010)') +
  ylab('Transacciones')+
  ggtitle('Transacciones de cada mes por pais') + 
  facet_wrap(. ~ Country, scales = "free_y")


#-----------------------------------------------------------------

#¿En que país se tiene la mayor cantidad de ganancia?
df2 <- mutate(df2, Total = (Quantity * UnitPrice))

Total_pais <- df2[, c('Country', 'Total')] %>% 
  group_by(Country) %>%
  summarise(Total = sum(Total)) %>% 
  arrange(desc(Total))%>%
  head(10)


ggplot(Total_pais, aes(x=Country, y=Total))+
  geom_col(fill=rgb(0.1,0.4,0.5,0.7), color= 'white')

#-----------------------------------------------------------------

#Como se comporta la cantidad de usuarios por pais?
df2%>%
  distinct(Country, CustomerID)%>%
  group_by(Country)%>%
  count()%>%
  arrange(desc(n))%>%
  ggplot(aes(x=reorder(Country, n), y=n))+
  geom_col()+
  coord_flip()+
  labs(x="Country", y="Cantidad")
  
  
#¿Como se ha comportado el precio de los top 10 productos a lo largo del tiempo?
tp10<-df2%>% 
  group_by(StockCode) %>% 
  count()%>%
  arrange(desc(n)) %>%
  head(10)

df2%>%
  filter(StockCode %in% tp10$StockCode)%>%
  distinct(StockCode, date, UnitPrice)%>%
  ggplot(aes(x=month(date), y=UnitPrice))+
    geom_smooth()+
    facet_wrap(~ StockCode, scales = "free_y")+
    labs(x="mes")+
    ggtitle("Linea del tiempo de precio de los top 10 productos")



#e. Presentar gráficas para responder las preguntas planteadas en elpunto anterior----------------------------------------
#f. Modelos:--------------------------------------------------------------------------------------------------------------
#   a. Clustering (recomendación aplicarlo a clientes) 


df_clustering <- df[df$Quantity > 0 & df$UnitPrice > 0, ]
df_clustering <- drop_na(df_clustering, CustomerID)
df_clustering <- drop_na(df_clustering, Country)


df_clustering <- df_clustering %>% mutate(Total = (Quantity * UnitPrice) )

df3 <- df_clustering %>% 
  group_by(CustomerID) %>%
  summarise(Total = sum(Total), Country = first(Country))


frequency <- df_clustering %>% 
                distinct(InvoiceNo, CustomerID) %>%
                group_by(CustomerID) %>%
                count()

df3 <- cbind(df3, frequency[,2])
df3 <- rename(df3, Frecuency = n)

Last <- df_clustering %>% 
          distinct(CustomerID, InvoiceDate)

maximum <- (max(Last$InvoiceDate)) + days(1)
Last$Last <- maximum - Last$InvoiceDate

Last2 <- as.data.frame(Last %>% 
  group_by(CustomerID) %>%
  summarise(Last = min(Last)))

df3 <- cbind(df3, Last2[,2])
df3 <- rename(df3, Last = 'Last2[, 2]')


Columnas = colnames(df3)
Tipos = sapply(df3, class)

nombres_DF = as.data.frame(cbind(Columnas, Tipos)) 

variables_para_dummificar = nombres_DF%>%
  filter(Tipos == "character")

for(i in 1:nrow(variables_para_dummificar)) {
  
  columna_ref = as.character(variables_para_dummificar[i, 1])
  temporal = select(df3, columna_ref)
  colnames(temporal) = "REF"
  temporal$REF = as.factor(temporal$REF)
  temporal_dummy = dummify(temporal$REF)
  nombres_dummy = colnames(temporal_dummy)
  nombres_dummy = paste(columna_ref, nombres_dummy, sep = "-")
  colnames(temporal_dummy) = nombres_dummy
  archivo = if(i == 1){ archivo = temporal_dummy} else {archivo = cbind(archivo, temporal_dummy)}
  
}

archivo = as.data.frame(archivo)


#Datos atipicos


#Total
ggplot(df3, aes(x=Total)) +
  geom_boxplot()


Q1 = quantile(df3$Total, c(0.25))
Q3 = quantile(df3$Total, c(0.75))
RIQ = Q3 - Q1
df4 = df3[(df3$Total >= (Q1 - 1.5*RIQ)) & (df3$Total <= (Q3 + 1.5*RIQ)), ]

#Frecuency
ggplot(df4, aes(x=Frecuency)) +
  geom_boxplot()


Q1 = quantile(df4$Frecuency, c(0.25))
Q3 = quantile(df4$Frecuency, c(0.75))
RIQ = Q3 - Q1
df4 = df4[(df4$Frecuency >= (Q1 - 1.5*RIQ)) & (df4$Frecuency <= (Q3 + 1.5*RIQ)), ]


#Last 
ggplot(df4, aes(x=Last)) +
  geom_boxplot()

df4$Last <- as.numeric(df4$Last)

Q1 = quantile(df4$Last, c(0.25))
Q3 = quantile(df4$Last, c(0.75))
RIQ = Q3 - Q1
df4 = df4[(df4$Last >= (Q1 - 1.5*RIQ)) & (df4$Last <= (Q3 + 1.5*RIQ)), ]




df_clustering <- (df4[c('Total','Frecuency','Last')])

df_clustering <- scale(df_clustering)

df_costumers <- df4

#-------------------------Kmeans-----------------------------------------------#

tot_withinss <- map_dbl(2:10,  function(k){
  model <- kmeans(x = df_clustering, centers = k)
  model$tot.withinss
})

# Genero un dataframe con el valor de k y de tot_withinss
elbow_df <- data.frame(
  k = 2:10 ,
  tot_withinss = tot_withinss
)

# Muestro la gráfica de codo
ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_line() +
  scale_x_continuous(breaks = 2:10)


# Uso map_dbl para correr varios modelos cambiando el valor de k
sil_width <- map_dbl(2:10,  function(k){
  model <- pam(x = df_clustering, k = k)
  model$silinfo$avg.width
})

# Genero un dataframe que tenga el valor k y la silueta
sil_df <- data.frame(
  k = 2:10,
  sil_width = sil_width
)

# Grafico la relación entre k y la silueta
ggplot(sil_df, aes(x = k, y = sil_width)) +
  geom_line() +
  scale_x_continuous(breaks = 2:10)

#Defino una semilla para que los valores sean comparables con la solución, debido a que k-means tiene un punto de partida aleatorio podría dar resultados diferentes y la única forma de asegurar que los resultados sean comparables es tener una semilla definida.

set.seed(255)

# Construir un modelo k-means model con los datos de customers_spend con k=4
model_clientes <- kmeans(df_clustering, centers = 3)

# Extraer el vactor con los valores de clusters asignados
cluster_k <- as.data.frame(model_clientes$cluster)
colnames(cluster_k) = "cluster_k"

# Generar un dataframe con los datos de los clientes y el cluster
segment_customers_k = bind_cols(df4[c('Total', 'Frecuency', 'Last')], cluster_k) %>%
  mutate(cluster_k = as.factor(cluster_k))

# Calculamos el tamaño de cada cluster
Conteo_clusters1_char = segment_customers_k %>%
  dplyr::group_by(cluster_k) %>%
  dplyr::summarise(conteo = n())

Conteo_clusters1_char

# Graficamos el resultado de los 3 clusters
clusplot(segment_customers_k, segment_customers_k$cluster_k, shape=TRUE, color=TRUE, labels=2, shade = T)


# Calculo el promedio de cada categoría
Resumen_K_char = segment_customers_k %>% 
  group_by(cluster_k) %>% 
  summarise_all(funs(mean(.)))

Resumen_K_char

#Comparación Total
ggplot(segment_customers_k, aes(x=cluster_k, y=Total, fill=cluster_k)) + 
  geom_boxplot()

#Comparación Frecuency
ggplot(segment_customers_k, aes(x=cluster_k, y=Frecuency, fill=cluster_k)) + 
  geom_boxplot()

#Comparación Last
ggplot(segment_customers_k, aes(x=cluster_k, y=Last, fill=cluster_k)) + 
  geom_boxplot()

#------------------------------------------------------------------------------------------------------------#

#---------------------------Jerarquico------------------------------------#

dist_customers <- dist(df_clustering)

# Realizo la clusterización según el método de vinculación que defina 
hc_customers <- hclust(dist_customers, method = "complete")

plot(hc_customers)

cluster <- cutree(hc_customers, k=3)


segment_customers = cbind(df_costumers, cluster)

# Cuento la cantidad de clientes asignados a cada cluster
count(segment_customers, cluster)

# Agrego color al dendograma según el corte de altura definido
dend_customers <- as.dendrogram(hc_customers)
dend_colored <- color_branches(dend_customers, k = 3)

# Grafico el dendograma con colores

plot(dend_colored)
rect.hclust(hc_customers, k=3) # agrega un rectángulo utilizando k


# Calculo el promedio de cada categoría
Resumen = segment_customers %>% 
  group_by(cluster) %>% 
  summarise_all(funs(mean(.)))

Resumen

#-------------------------------------------------------------------------------------------------------------#

#   b. Association rules
df2$StockCode=factor(df2$StockCode)
df2$InvoiceNo=factor(df2$InvoiceNo)
sp<-split(df2$StockCode, df2$InvoiceNo)
trans<-as(sp, "transactions")
inspect(trans[1:5])
itemFrequencyPlot(trans, topN = 20)
itemFrequencyPlot(trans, support = 0.05)
image(sample(trans, 1000))
rules<-apriori(trans, parameter=list(suppor=0.025, confidence = 0.2, minlen=2))
summary(rules)
inspect(sort(rules, by="lift"))
rulesdf<-as(rules, "data.frame")

#3 geografia analitica
#a ventas
apikey = "AIzaSyAzIPOLbTCkY-vMQtQJ7nXNk3ytvs8NlSU"

register_google(key = apikey)

ventas<-df2%>%
  distinct(Country, InvoiceNo)%>%
  group_by(Country)%>%
  count()

#csv
write.csv(ventas, 'Ventas.csv')

ventas<-ventas%>%
  mutate(ubi=geocode(Country, source = "google"))


ggmap(get_googlemap(center = "Mali", zoom=1, maptype = "terrain",color="color"))+
  geom_point(data=ventas, aes(x=ventas$longitud, y=ventas$latitud, size = ventas$n), color="red")+
  labs(x="Longitud",y="Latitud",size="Ventas")+
  ggtitle("Ventas por país")


#b
df2$total=df2$UnitPrice*df2$Quantity
ppromedio<-df2%>%
  group_by(InvoiceNo, Country)%>%
  summarise(media=mean(total))%>%
  ungroup()%>%
  group_by(Country)%>%
  summarise(mediaPais=mean(media))%>%
  arrange(desc(mediaPais))

ppromedio<-ppromedio%>%
  mutate(ubi=geocode(Country, source = "google"))  

ggmap(get_googlemap(center= "Mali",zoom=1, maptype = "terrain", color="color"))+
  geom_point(data=ppromedio, aes(x=ppromedio$ubi$lon, y =ppromedio$ubi$lat, size=ppromedio$mediaPais), color="blue")+
  labs(x="Longitud",y="Latitud", size="Precio promedio")+
  ggtitle("Precio promedio por país")

#c
cantC<-df2%>%
  distinct(CustomerID, Country)%>%
  group_by(Country)%>%
  count()

cantC<-cantC%>%
  mutate(ubi=geocode(Country, source = "google")) 

ggmap(get_googlemap(center="Mali", zoom=1, maptype = "terrain", color="color"))+
  geom_point(data=cantC, aes(x=cantC$ubi$lon, y=cantC$ubi$lat, size=cantC$n), color="dark green")+
  labs(x="Longitud",y="Latitud", size="Cantidad Clientes")+
  ggtitle("Cantidad de clientes por país")


#d
cantP<-df2%>%
  distinct(StockCode, Country)%>%
  group_by(Country)%>%
  count()%>%
  arrange(desc(n))

cantP<-cantP%>%
  mutate(ubi=geocode(Country, source = "google"))

ggmap(get_googlemap(center="Mali", zoom=1, maptype = "terrain",color="color"))+
  geom_point(data=cantP, aes(x=cantP$ubi$lon, y=cantP$ubi$lat, size=cantP$n, color="orange"))+
  guides(color=F)+
  labs(x="Longitud",y="Latitud", size="Cantidad Productos")+
  ggtitle("Cantidad de Productos por país")


#csv

write.csv(ventas, 'Ventas.csv')
write.csv(ppromedio, 'ppromedio.csv')
write.csv(cantC, 'cantC.csv')
write.csv(cantP, 'cantP.csv')


