#Modelo de prediccion de precio de computadores.
#Predicción del precio de los computadores según la disposición del hardware 

library(dplyr)
library(ggplot2)
library(car)
library(lmtest)
url <- 'https://raw.githubusercontent.com/josedavidm12/Regresion/refs/heads/main/laptop%20Price%20Prediction%20Dataset.csv'
datos <- read.csv2(url, sep ="," ,dec=".", stringsAsFactors = TRUE)
options(max.print=100000000)
#Análisis exploratorio

datos<-datos%>%rename(Ram= `RAM..GB.`,Espacio= `Storage..GB.`,Precio= `Price....`, Ssize=`Screen.Size..inches.`,
                      Peso=`Weight..kg.`, Bateria= `Battery.Life..hours.`, Garantia = `Warranty..years.`, Grafica = `Graphics.Card`, SO = `Operating.System`)


datos$Ram<-as.factor(datos$Ram)
datos$Ssize<-as.factor(datos$Ssize)
datos$Garantia<-as.factor(datos$Garantia)
datos$Espacio<-as.factor(datos$Espacio)

hardware<-datos[,-c(2)]

m1<-lm(Precio~Ram*Garantia*Espacio*Ssize*Bateria,data=hardware)
summary(m1)
vif(m1)
options(max.print=100000)
influence.measures(m1)

nueva_base <- datos %>% filter(Ram != 8)

m2 <- lm(Precio~Ram*Espacio*Ssize,data=nueva_base)
summary(m2)


which(apply(influencia1$is.inf, 1, any))

nueva_base1 <- datos %>% filter(Ram != 8,Ssize != 14, Ssize != 15.6, Ram!=64)


##########################
m21<-lm(Precio~Ram*Processor+Bateria*Peso,data=hardware)
summary(m21)

#Puntos de influencia

residuales21 <- rstandard(m21)
residuales21

ggplot(m21, aes(.fitted, .resid)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Residuos vs Valores Ajustados",
       x = "Valores Ajustados", y = "Residuos") +
  theme_minimal()


plot(datos$Precio, residuales21)
abline(h=0)

#Pruebas estadisticas
options(max.print=500000)
  influence.measures(m21)
  
  influencia1 <- influence.measures(m21)
  
  # Extraer las métricas numéricas (que están en la parte "infmat" del objeto)
  medidas_influencia_numericas <- influencia1$infmat
  
  # Convertir a data frame para facilitar el manejo
  medidas_influencia_df <- data.frame(medidas_influencia_numericas)
  
  
  medidas_influencia_df
  medidas_influencia_df










###########################
m22<- lm(Precio~Ram*Processor+Bateria+Ssize*Espacio,data=hardware)
summary(m22)

#Puntos de influencia

residuales22 <- rstandard(m22)
residuales22

plot(datos$Precio, residuales22)
abline(h=0)

 #Pruebas estadisticas

influence.measures(m22)

############################



#Prueba de residuales

ad.test(m22$residuals)
shapiro.test(m22$residuals) #Normalidad
bptest(m22) #Homocedasticidad
dwtest(m22) #Independencia

influencia1<-influence.measures(m1)


#Prueba de AIC (Stepwise) para checkear la significancia de las variables
#Modelo completo
modelo_completo <- lm(Precio ~ ., data = hardware)
vif(modelo_completo)
modelo_reducido <- lm(Precio ~ Ram+Garantia+Ssize , data = hardware)

AIC(modelo_completo)
AIC(m3)

BIC(modelo_completo)
BIC(modelo_reducido)


residuales<-rstandard(modelo_completo)
plot(datos$Precio,residuales)
abline(h=0)

ggplot(modelo_completo, aes(.fitted, .resid)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Residuos vs Valores Ajustados",
       x = "Valores Ajustados", y = "Residuos") +
  theme_minimal()

influence.measures(modelo_completo)

