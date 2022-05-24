
library("ggplot2")
library("dplyr")
library('lubridate')
library(readxl)
library(countrycode)

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

ggplot(StockCode_head, aes(x=as.factor(StockCode), y=n)) +
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

ggplot(Quantity, aes(x=as.factor(Quantity), y=n))+
  geom_col(fill=rgb(0.1,0.4,0.5,0.7), color= 'white') +
  xlab('Cantidad') +
  ylab('Cuenta')+
  ggtitle('25 cantidades más compradas')


#InvoiceDate
max(df$InvoiceDate)
min(df$InvoiceDate)

df2 <- mutate(df2, Date = date(df$InvoiceDate))

df2 %>% 
  distinct(Date, InvoiceNo) %>%
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
  distinct(InvoiceNo, Country) %>%
  group_by(Country) %>%
  count() %>%
  arrange(desc(n)) %>%
  ggplot(aes(x='', y = n, fill= Country))+
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y") +
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

#   ¿Los precios se comportan diferentes según la región?
df2 <- mutate(df, Region = (countrycode(sourcevar = df2$Country, origin = "country.name",destination = "region")))
df2$Region <- as.factor(df2$Region)   



ggplot(df2, aes(y=UnitPrice))+
  geom_boxplot(fill=rgb(0.1,0.4,0.5,0.7), color="black")+ 
  ylim(0, 15) + 
  facet_grid(. ~ Region)

#¿Las ventas presentan alguna estacionalidad por mes? 
df2 <- mutate(df2, Month = month(df2$Date), Year = year(df2$Date))
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

#¿En que país se tiene la mayor cantidad de ganancia?
df2 <- mutate(df2, Total = (Quantity * UnitPrice))

Total_pais <- df2[, c('Country', 'Total')] %>% 
  group_by(Country) %>%
  summarise(Total = sum(Total)) %>% 
  arrange(desc(Total))%>%
  head(10)


ggplot(Total_pais, aes(x=Country, y=Total))+
  geom_col(fill=rgb(0.1,0.4,0.5,0.7), color= 'white')

#e. Presentar gráficas para responder las preguntas planteadas en elpunto anterior----------------------------------------
#f. Modelos:--------------------------------------------------------------------------------------------------------------
#   a. Clustering (recomendación aplicarlo a clientes) 
#   b. Association rules









