
#Cargar librerías 
library(tidyverse) 
library(readr)
library(dplyr)

#Leer y mirar los datos 
url <- "https://raw.githubusercontent.com/alejandraandrea/slides-xaringan-mixed-models/master/dragons.tsv"
download.file(url, "dragons.tsv")
dragones <- read_tsv("dragons.tsv") #read_tsv de {readr}
glimpse(dragones) #glimpse de {dplyr} #En {base} lo más similar es str().

#Limpieza y transformación de datos (¡ok!)

#Resumen preliminar de los datos
library(skimr)
skim(dragones) #En {base} lo más similar es summary()

#Análisis exploratorio de datos 
dragones %>% ggplot(aes(x=bodyLength, 
                        y=testScore)) + 
  geom_jitter(alpha=.2) +
  theme_bw()

#Existe una relacion con tendencia lineal

#Con más de una covariable continua, se recomienda estandarizar las covariables antes de continuar (restarle la media y dividirlo por la raiz de la varianza para comparar el tamanio de los efectos y acelerar la convergencia del modelo)
#dragons$bodyLength2 <- scale(dragons$bodyLength) #{base}

#Cargar paquetes
library(tidymodels) #Colección de paquetes para el modelado y análisis estadístico, que comparte la filosofía de diseño subyacente, la gramática y las estructuras de datos del tidyverse 
library(broom) #{broom} de {tidymodels} #Resume información clave sobre objetos estadísticos en tidy tibbles (df ordenados) 

#Ajustar un modelo lineal 
ajuste_lm <- lm(testScore ~ bodyLength, data=dragones) #lm() de {stats}

#Ajustar un modelo implica estimar los parametros

#Resumir información sobre las estimaciones del modelo
tidy(ajuste_lm) #Con summary() en {base} 

#RTA: el largo del cuerpo si influencia a la respuesta, comparando el p value

#Resumir información sobre el modelo ajustado
glance(ajuste_lm)

#RTA: mientras menores sean estas medidas tengo un mejor modelo

#Graficar el ajuste del modelo
info_ajuste_lm <- augment_columns(ajuste_lm,dragones) #augment_columns de {broom} agrega columnas con valores ajustados, residuos y otros resultados comunes (fitted, )

info_ajuste_lm %>% ggplot(aes(x=bodyLength, y=testScore)) + 
  geom_jitter(alpha=.2) + 
  geom_line(aes(x=bodyLength,y=.fitted))+
  theme_bw()

#RTA: ajustar es un proceso de estimacion

#Verificar los supuestos: Analisis de Diagnostico 

#Linealidad
plot(ajuste_lm, which=1) #plot de {stats}
#Se refiere a la forma funcional lineal entre las variables explicativas y 
#la variable respuesta. Un gráfico de los residuos versus los valores ajustados será utilizado. 
#Si los residuos se dispersan al azar alrededor de la línea cero se sugiere que la suposición 
#de que la relación es lineal es razonable.
#RTA: La linea roja es un promedio de cada punto y puede indicar que el modelo es extraño xq hay una especie de tendencia. 

#Normalidad
plot(ajuste_lm, which=2) #plot de {stats}
#Independencia y normalidad: La independencia se refiere a que los errores son 
#independientes o no están correlacionados. La normalidad a que los residuos 
#siguen la distribución normal. Un gráfico Q-Q normal será utilizado. 
#Si los residuos se desvían de la línea recta, 
#entonces los residuos tienen colas más pesadas que la distribución normal.
#RTA: los puntos se alejan bastante de las colas 

#Homocedasticidad (varianzas iguales)
plot(ajuste_lm, which=3) #plot de {stats}
#Homocedasticidad: Se refiere a que si los residuos tienen varianza constante a
#través de las variables explicativas (errores homocedásticos). 
#Un gráfico de localización-escala será usado. 
#Si la varianza de los residuos no es constante en función del valor ajustado, 
#hay evidencias de heterocedasticidad.


#Análisis exploratorio de datos
dragones %>% ggplot(aes(x = bodyLength, y = testScore, colour = mountainRange)) +
  geom_jitter(alpha=2) +
  theme_bw() 

dragones %>% ggplot(aes(bodyLength, testScore,colour = mountainRange))+
  geom_jitter(alpha=2) + 
  facet_wrap(~ mountainRange) +
  theme_bw()+
  theme(strip.background = element_rect(fill="white"))

dragones %>% ggplot(aes(x=mountainRange, y=testScore, colour=mountainRange)) + 
  geom_boxplot(alpha=.5) +
  coord_flip()+
  theme_bw()

#Hay indicios para indicar que debemos utilizar un modelo lineal mixto

#Cargar paquetes
library(broom.mixed) #Sigue la línea del paquete broom para modelos mixtos, sin embargo, también aplica para modelos lineales
library(lme4) #Paquete para modelamiento con efectos mixtos

#Ajustar  un modelo lineal mixto
ajuste_lmer <- lmer(testScore ~ bodyLength + (1|mountainRange), data = dragones)

#De ese modo incorporamos el efecto de agrupamiento

#Resumir información sobre las estimaciones del modelo
tidy(ajuste_lmer) #¡Sin p-valor! 

#RTA: ahora la pendiente es de 0,033 que es casi cercano a cero y el p valor permite aceptar a HO. La covariable deja de ser significativa, el largo del cuerpo no influye en la respuesta. 

#La pendiente 18.4 indica una variabilidad que la estan explicando las cadenas montaniosas y en el modelo anterior no se estaba considerando. 

#Resumir información sobre el modelo ajustado
glance(ajuste_lmer)

#Adicionando p-valor
#Cargar paquete
library(lmerTest)

#Ajustar el modelo
ajuste_lmer <- lmer(testScore ~ bodyLength + (1|mountainRange), data = dragones)

#Resumir información sobre las estimaciones del modelo
tidy(ajuste_lmer) #¡Con p-valor! 


#Graficar el ajuste del modelo
info_ajuste_lmer <- augment_columns(ajuste_lmer,dragones)

info_ajuste_lmer %>% ggplot(aes(x=bodyLength,y=testScore,colour=mountainRange))+ 
  geom_jitter(alpha=2)+ 
  facet_wrap(~ mountainRange)+
  geom_line(aes(x=bodyLength,y=.fitted),colour="black")+
  theme_bw()

#RTA: las lineas no tienen mucha pendiente y esto tiene sentido xq la estimacion era cercana a cero y en cada una el intercepto es diferente.  

#Verificar los supuestos

#Normalidad del error
y.ajuste <- fitted(ajuste_lmer)
res.ajuste <- residuals(ajuste_lmer)
qqnorm(res.ajuste) 
qqline(res.ajuste)

#Normalidad del efecto aleatorio
pred.ajuste <- ranef(ajuste_lmer)[[1]][[1]]
qqnorm(pred.ajuste) 
qqline(pred.ajuste)

#Linealidad y homocedasticidad
y.ajuste <- fitted(ajuste_lmer)
res.ajuste <- residuals(ajuste_lmer)
plot(y.ajuste, res.ajuste) 
abline(h=0, lty=2,col="red")

#Incorporación de los sitios (efectos anidados)

ajuste_lmer_2<- lmer(testScore ~ bodyLength + (1|mountainRange/site), data = dragones)

ajuste_lmer_2<- lmer(testScore ~ bodyLength + (1|mountainRange) + (1|mountainRange:site), data = dragones)

tidy(ajuste_lmer_2)

#RTA: los sitios montaniosos tmb incorporan variabilidad por el 18.1, sin embargo no cambia el p value. 

# Parte de la variabilidad viene explicado por las cadenas montaniosas 











