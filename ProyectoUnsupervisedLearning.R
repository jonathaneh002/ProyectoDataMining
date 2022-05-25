
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

ggplot(ToothGrowth, aes(x=dose, y=len, color=dose)) +
  geom_boxplot()

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
  geom_col()
  

#Como se comporta la cantidad de usuarios por pais?
df2%>%
  distinct(Country, CustomerID)%>%
  group_by(Country)%>%
  count()%>%
  arrange(desc(n))%>%
  ggplot(aes(x=reorder(Country, n), y=n))+
  geom_col()+
  coord_flip()
  
  
#¿Como se ha comportado el precio de los top 10 productos a lo largo del tiempo?
tp10<-df2%>% 
  group_by(StockCode) %>%
  arrange(desc(n)) %>%
  head(10)

df2%>%
  filter(StockCode %in% tp10$StockCode)%>%
  distinct(StockCode, date, UnitPrice)%>%
  #mutate(nombre)
    ggplot(aes(x=(date), y=UnitPrice))+
  geom_smooth()+
  facet_wrap(~ StockCode, scales = "free_y")



#e. Presentar gráficas para responder las preguntas planteadas en elpunto anterior----------------------------------------
#f. Modelos:--------------------------------------------------------------------------------------------------------------
#   a. Clustering (recomendación aplicarlo a clientes) 
#   b. Association rules




