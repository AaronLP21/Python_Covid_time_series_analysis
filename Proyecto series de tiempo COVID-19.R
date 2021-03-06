"
PREYECTO DE SERIES DE TIEMPO

Programa que hace un analisis exploratorio de los datos que registran por entidad federativa y a nivel
nacional las murtes por covid en mx del 03-03-2020 al 21-01-2021. En particula se analiza y comparan los
esatos de Veracruz y CDMX, asi como ajustes a modelos de series de tiempo para predecir los fallecimientos.

@Date    : 12-02-2020
@Version : 1.0
"
# Bibliotecas  -----------------------------------------------------
library(readr)
library(reshape2)
library(tidyverse)
library(ggfortify)
library(forecast)
library(astsa)
library(pivottabler)
library(skimr)
library(knitr)

# Analisis exploratorio ---------------------------------------------------
setwd("C:/Users/Aaron/Desktop/2021-1/Modelos de supervivencia y series de tiempo/Proyectos/Sereies_COVID")
dir()
datos.covid <- read_csv("Casos_Diarios_Estado_Nacional_Defunciones_20210123.csv")
dim(datos.covid)
view(datos.covid)

skim(datos.covid)#Con skim() vemos que no hay datos faltantes

"
A continuacion vamos a manipular el dataset para calcular los totales de muertes para CDMX y Veracruz, de
esa manera visualizar las muertes y calcular las estadisticas basicas
"

datos.covid.reco <- datos.covid[,-c(1,2,3,4,330,331)]#quitamos fechas inecesarias, id y poblacion

datos.covid.totales <- data.frame(datos.covid$nombre,datos.covid.reco,#calculamos la suma de cada estado
                       total=rowSums(datos.covid.reco,na.rm=TRUE))
dim(datos.covid.reco)
dim(datos.covid.totales)
View(datos.covid.totales)
View(datos.covid.reco)
#Extraemos solo CDMX y VERACRUZ con sus respectivos totales, despues renombramos adecudamente
datos.covid.totales <- datos.covid.totales[c(7,30),c(1,327)]
datos.covid.totales$datos.covid.nombre <- c("CDMX","VERACRUZ")
colnames(datos.covid.totales) <- c("ENTIDAD","TOTAL")
View(datos.covid.totales)
knitr::kable(datos.covid.totales)#Visualizamos en consola

#Hacemos un barplot de las muertes por estado
ggplot(datos.covid.totales, aes(x=ENTIDAD, y=TOTAL,fill=as.factor(ENTIDAD))) +
  geom_bar(stat = "identity")+
  geom_text(aes(x=ENTIDAD, y=TOTAL,label=TOTAL))+
  labs(x = 'Entidades', y = 'Muertes',fill = 'Entidades',
       title ="NÃºmero de muertes por COVID-19 en MÃ©xico" ,
       caption = "Muertes contabilizadas del 03-03-2020 al 20-01-2021")+
        scale_fill_manual(values=c("#56B4E9", "#E69F00"))

#Vamos a reshapear los datos para facilitar su analisis
datos.covid.reco.2 <- datos.covid[c(7,30),-c(1,2,4,330,331)]#para armar los boxplots
view(datos.covid.reco.2)
datos.covid.reco.2$nombre <- c("CDMX","VERACRUZ")
datos.covid.reco.2.reshaped <-melt(datos.covid.reco.2,id="nombre")
colnames(datos.covid.reco.2.reshaped) <- c("ENTIDAD","FECHA","MUERTES")
View(datos.covid.reco.2.reshaped)

#Creamos un BOXPLOT para ver la dispersion de las muertes por estado
boxplot(MUERTES ~ ENTIDAD, data = datos.covid.reco.2.reshaped, col = c("#56B4E9", "#E69F00"),
        varwidth = TRUE, main = "MUERTES POR ESTADO",ylab = "MUERTES",xlab = "ENTIDAD")

#Calculamos algunas ESTADISTICAS BASICAS y las reunimos en un data frame
#VERACRUZ
dcr2r.media.ver <- datos.covid.reco.2.reshaped%>%filter(ENTIDAD=="VERACRUZ")%>%
                    summarise(MEDIA = mean(MUERTES))

dcr2r.max.ver <- datos.covid.reco.2.reshaped%>%filter(ENTIDAD=="VERACRUZ")%>%
                  summarise(MAX = max(MUERTES))

dcr2r.min.ver <- datos.covid.reco.2.reshaped%>%filter(ENTIDAD=="VERACRUZ")%>%
                  summarise(MIN = min(MUERTES))

dcr2r.mediana.ver <- datos.covid.reco.2.reshaped%>%filter(ENTIDAD=="VERACRUZ")%>%
                  summarise(MEDIANA = median(MUERTES))

dcr2r.var.ver <- datos.covid.reco.2.reshaped%>%filter(ENTIDAD=="VERACRUZ")%>%
                  summarise(VAR = var(MUERTES))

estadisticas.basicas.veracruz <- c("VERACRUZ","","","","","")
estadisticas.basicas.veracruz[2] <- dcr2r.min.ver;estadisticas.basicas.veracruz[3] <- dcr2r.max.ver
estadisticas.basicas.veracruz[4] <- dcr2r.mediana.ver;estadisticas.basicas.veracruz[5] <- round(dcr2r.media.ver,2)
estadisticas.basicas.veracruz[6] <- round(dcr2r.var.ver,2)

#CDMX
dcr2r.media.cdmx <- datos.covid.reco.2.reshaped%>%filter(ENTIDAD=="CDMX")%>%
                     summarise(MEDIA = mean(MUERTES))

dcr2r.max.cdmx <- datos.covid.reco.2.reshaped%>%filter(ENTIDAD=="CDMX")%>%
                     summarise(MAX = max(MUERTES))

dcr2r.min.cdmx <- datos.covid.reco.2.reshaped%>%filter(ENTIDAD=="CDMX")%>%
                     summarise(MIN = min(MUERTES))

dcr2r.mediana.cdmx <- datos.covid.reco.2.reshaped%>%filter(ENTIDAD=="CDMX")%>%
                      summarise(MEDIANA = median(MUERTES))

dcr2r.var.cdmx <- datos.covid.reco.2.reshaped%>%filter(ENTIDAD=="CDMX")%>%
                      summarise(VAR = var(MUERTES))

estadisticas.basicas.cdmx <- c("CDMX","","","","","")
estadisticas.basicas.cdmx[2] <- dcr2r.min.cdmx;estadisticas.basicas.cdmx[3] <- dcr2r.max.cdmx
estadisticas.basicas.cdmx[4] <- dcr2r.mediana.cdmx;estadisticas.basicas.cdmx[5] <- round(dcr2r.media.cdmx,2)
estadisticas.basicas.cdmx[6] <- round(dcr2r.var.cdmx,2)
#UNIMO SLOS VECTORES 
estadisticas <- as.data.frame(rbind(estadisticas.basicas.cdmx,estadisticas.basicas.veracruz))
colnames(estadisticas) <- c("ENTIDAD","MIN","MAX","MEDIANA","MEDIA","VAR")
View(estadisticas)#Obtenemos, min, max, media, mediana y la varianza de los dos estados


# Series de tiempo CDMX ---------------------------------------

# Importación de datos de fallecimientos por COVID19 en México hasta el 23 de enero de 2021
Casos_Nacional_20210123 <- 
  read_csv("Casos_Diarios_Estado_Nacional_Defunciones_20210123.csv")


# La base de datos se transforma para crear una variable de fecha, en
# los datos originales el nombre de las columnas indica las fechas,
Casos_Nacional_20210123.m<- melt(Casos_Nacional_20210123[,-2],id=c("cve_ent","nombre"))

# Etiquetas de la variables,
names(Casos_Nacional_20210123.m)<-c("cve_ent","nombre","fecha","casos")

#La variable fecha se convierte en caracter para después darle el formato de fecha
Casos_Nacional_20210123.m$fecha<- as.character(Casos_Nacional_20210123.m$fecha)


# NOTA. Se consideró el periodo hasta el 20 de enero para tener consistencia 
# de los datos
Casos_Nacional_20210123.m <- Casos_Nacional_20210123.m %>%
  filter(
    as.Date(fecha, "%d-%m-%Y") >= as.Date("03-03-2020", "%d-%m-%Y") &
      as.Date(fecha, "%d-%m-%Y") <= as.Date("20-01-2021", "%d-%m-%Y")
  )
Casos_Nacional_20210123.m

# El análisis solamente se hace para los datos del total del Distrito Federal, en
# este caso cve_ent=="09",
with(Casos_Nacional_20210123.m%>%filter(cve_ent=="09"),
     plot(as.Date(fecha, "%d-%m-%Y"),casos, type="l", xlab = "Fecha",
          
          main = "Fallecimientos por COVID19 en el Distrito Federal, 03-2020 a 01-2021"))
# El análisis solamente se hace para los datos del total de Veracruz, en
# este caso cve_ent=="30",
with(Casos_Nacional_20210123.m%>%filter(cve_ent=="30"),
     plot(as.Date(fecha, "%d-%m-%Y"),casos, type="l", xlab = "Fecha",
          main = "Fallecimientos por COVID19 enVeracruz, 03-2020 a 01-2021"))


# Se suman los casos por año y mes  
Agregado_mensual_df<- Casos_Nacional_20210123.m%>%filter(cve_ent=="09")%>%
  mutate(mes = format(as.Date(fecha, "%d-%m-%Y"), "%m"),
         agno = format(as.Date(fecha, "%d-%m-%Y"), "%Y")) %>%
  group_by(agno, mes) %>%
  summarise(total = sum(casos))

Agregado_mensual_veracruz<- Casos_Nacional_20210123.m%>%filter(cve_ent=="30")%>%
  mutate(mes = format(as.Date(fecha, "%d-%m-%Y"), "%m"),
         agno = format(as.Date(fecha, "%d-%m-%Y"), "%Y")) %>%
  group_by(agno, mes) %>%
  summarise(total = sum(casos))
# Gráfica
Agregado_mensual_df %>%
  ggplot(aes(x = mes, y = total)) +
  geom_bar(stat = "identity",fill = "darkblue") +
  facet_wrap(~ agno, ncol = 3) +
  labs(title = "Fallecimientos por COVID19-México en el Distrito Federal",
       subtitle = "Dato separados por año",
       y = "Fallecimientos",
       x = "Mes") + theme_bw(base_size = 15)

Agregado_mensual_veracruz %>%
  ggplot(aes(x = mes, y = total)) +
  geom_bar(stat = "identity",fill = "darkblue") +
  facet_wrap(~ agno, ncol = 3) +
  labs(title = "Fallecimientos por COVID19-México en Veracuz",
       subtitle = "Dato separados por año",
       y = "Fallecimientos",
       x = "Mes") + theme_bw(base_size = 15)


# Dada la fecha "27-02-2020" se identifica la semana que le corresponde
# semana 8 del rango 0 a 53
startU <- as.numeric(strftime(as.Date("03-03-2020", "%d-%m-%Y"), format = "%U"))

# Dada la fecha "27-02-2020" se identifica el día (número decimal) de la semana que le corresponde
# (0-6, domingo es 0)
startD <- as.numeric(strftime(as.Date("03-03-2020", "%d-%m-%Y") + 1, format =" %w")) 


#Análisis descriptivo de la serie de tiempo,

# A) Los datos como una serie de tiempo
tsCovCdMx<-ts(Casos_Nacional_20210123.m%>%filter(cve_ent=="09")%>%dplyr::select(casos),
              frequency=7, start = c(startU,startD))
tsCovVer<-ts(Casos_Nacional_20210123.m%>%filter(cve_ent=="30")%>%dplyr::select(casos),
             frequency=7, start = c(startU,startD))
# B)Los datos en un formato de semanas (renglones) y días de la semana (columnas)
print(tsCovCdMx, calendar = T)
print(tsCovVer, calendar = T)
# C) Datos descriptivos de la serie
summary(tsCovCdMx)
summary(tsCovVer)
# D) Inicio, final y número decimal de la semana asignada a cada día
start(tsCovCdMx)
end(tsCovCdMx)
time(tsCovCdMx)

start(tsCovVer)
end(tsCovVer)
time(tsCovVer)

# E) La serie de tiempo original con una recta de un ajuste de un modelo
#   lineal de regresión,
ts.plot(tsCovCdMx, ylab="Fallecimientos diarios", xlab="semanas", 
        main="Fallecimientos COVID19 en el Distrito Federal, del 03/03/2020 al 20/01/2021\n
        con una línea de tendencia de un modelo de regresión lineal")
abline(reg = lm(tsCovCdMx~time(tsCovCdMx)))

ts.plot(tsCovVer, ylab="Fallecimientos diarios", xlab="semanas", 
        main="Fallecimientos COVID19 en Veracruz, del 03/03/2020 al 20/01/2021\n
        con una línea de tendencia de un modelo de regresión lineal")
abline(reg = lm(tsCovVer~time(tsCovVer)))
# La función de autocorrelación indica una series no estacionaria.
acf(tsCovCdMx, lag.max = 365, col = terrain.colors(16), main="ACF(Fallecimientos diarios COVID19 en el Distrito Federal)")
acf(tsCovVer, lag.max = 365, col = terrain.colors(16), main="ACF(Fallecimientos diarios COVID19 en Veracruz)")
pacf(tsCovCdMx)
pacf(tsCovVer)
#====================================================
#      COMPONENTES DE LA SERIE DE TIEMPO
#====================================================


##Componentes de la serie generadas con R
plot(decompose(tsCovCdMx))
plot(decompose(tsCovVer))
#=============================================
#Predicción
#=============================================

# Modelo ARMA(1,0,0) 
ARMA <- arima(tsCovCdMx, order = c(1,0,0))
print(ARMA)
ts.plot(tsCovCdMx)
ARMA_fit <- tsCovCdMx - resid(ARMA)
print(ARMA_fit)
points(ARMA_fit, type = "l", col = 2, lty = 2)


predict_ARMA <- predict(ARMA)

predict_ARMA$pred[1]

predict_ARMA_15 <- predict(ARMA,n.ahead=15)


#Intervalos de confianza
ts.plot(tsCovCdMx, xlim = c(8, 55), ylim=c(0,1200),
        main="Fallecimientos COVID19 en el Distrito Federal, predicción 15 días con intervalos\n
        de 95% de confianza")
ARMA_forecasts <- predict(ARMA, n.ahead = 15)$pred
ARMA_forecast_se <- predict(ARMA, n.ahead = 15)$se
points(ARMA_forecasts, type = "l", col = 2)
points(ARMA_forecasts - 1.96*ARMA_forecast_se, type = "l", col = 2, lty = 2)
points(ARMA_forecasts + 1.96*ARMA_forecast_se, type = "l", col = 2, lty = 2)

# El paso final es verificar si el componente aleatorio corresponde a una serie
# de tiempos estacionaria,

# La gráfica de residuales estandarizados (Standarized Residuals)
# no debe mostrar ninguna tendencia, tiene
# una media=0 y varianza=sigma^2_e

# La gráfica de autocorrelaciones de los residuales (ACF of residuales)
# debe mostrar que las autocorrelaciones no superan las líneas de 
# después de el rezago 6 ARMA(1,0,6)

# Se asume distribución normal estándar del componente aleatorio por
# lo que la gráfica de Normal QQ Plot of Std Residuals debe ubicarse
# cerca de la línea de identidad.

#La Gráfica de p-values for Ljung-Box statistic indica los valores p
# de las hipótesis 
# Ho: Los residuales son independientemente distribuidos
# Ha: Los residuales NO son independientemente distribuidos, muestran autocorrelación
# en el rezago indicado.

sarima(tsCovCdMx,1,0,0,no.constant=F)


# Serie de tiempo para Veracruz -------------------------------------------

# Importacion de datos de fallecimientos por COVID-19 en Mexico hasta el 23 de enero de 2021
Casos_Nacional_20210123 <- 
  read_csv("Casos_Diarios_Estado_Nacional_Defunciones_20210123.csv")


# La base de datos se transforma para crear una variable de fecha, en
# los datos originales el nombre de las columnas indica las fechas,
Casos_Nacional_20210123.m<- melt(Casos_Nacional_20210123[,-2],id=c("cve_ent","nombre"))

# Etiquetas de la variables,
names(Casos_Nacional_20210123.m)<-c("cve_ent","nombre","fecha","casos")

#La variable fecha se convierte en caracter para despues darle el formato de fecha
Casos_Nacional_20210123.m$fecha<- as.character(Casos_Nacional_20210123.m$fecha)


# NOTA. Se considera el periodo hasta el 20 de enero para tener consistencia 
# de los datos
Casos_Nacional_20210123.m <- Casos_Nacional_20210123.m %>%
  filter(
    as.Date(fecha, "%d-%m-%Y") >= as.Date("03-03-2020", "%d-%m-%Y") &
      as.Date(fecha, "%d-%m-%Y") <= as.Date("20-01-2021", "%d-%m-%Y")
  )


# El analisis solamente se hace para los datos del total nacional, en
# este caso cve_ent=="000", para seleccionar Durango cve_ent=="10"
with(Casos_Nacional_20210123.m%>%filter(cve_ent=="30"),
     plot(as.Date(fecha, "%d-%m-%Y"),casos, type="l", xlab = "Fecha",
          main = "Fallecimientos por COVID-19 en Veracruz, 03-2020 a 01-2021"))

# Se suman los casos por anio y mes  
Agregado_mensual<- Casos_Nacional_20210123.m%>%filter(cve_ent=="30")%>%
  mutate(mes = format(as.Date(fecha, "%d-%m-%Y"), "%m"),
         agno = format(as.Date(fecha, "%d-%m-%Y"), "%Y")) %>%
  group_by(agno, mes) %>%
  summarise(total = sum(casos))

# Grafica
Agregado_mensual %>%
  ggplot(aes(x = mes, y = total)) +
  geom_bar(stat = "identity",fill = "blue") +
  facet_wrap(~ agno, ncol = 3) +
  labs(title = "Fallecimientos por COVID-19 en Veracruz",
       subtitle = "Dato separados por año",
       y = "Fallecimientos",
       x = "Mes") + theme_bw(base_size = 15)



# Dada la fecha "27-02-2020" se identifica la semana que le corresponde
# semana 8 del rango 0 a 53
startU <- as.numeric(strftime(as.Date("03-03-2020", "%d-%m-%Y"), format = "%U"))

# Dada la fecha "27-02-2020" se identifica el dia (numero decimal) de la semana que le corresponde
# (0-6, domingo es 0)
startD <- as.numeric(strftime(as.Date("03-03-2020", "%d-%m-%Y") + 1, format =" %w")) 


#Analisis descriptivo de la serie de tiempo,

# A) Los datos como una serie de tiempo
tsCovCdMx<-ts(Casos_Nacional_20210123.m%>%filter(cve_ent=="30")%>%dplyr::select(casos),
              frequency=7, start = c(startU,startD))

# B)Los datos en un formato de semanas (renglones) y dias de la semana (columnas)
print(tsCovCdMx, calendar = T)

# C) Datos descriptivos de la serie
summary(tsCovCdMx)

# D) Inicio, final y numero decimal de la semana asignada a cada dia
start(tsCovCdMx)
end(tsCovCdMx)
time(tsCovCdMx)

# E) La serie de tiempo original con una recta de un ajuste de un modelo
#   lineal de regresion,

ts.plot(tsCovCdMx, ylab="Fallecimientos diarios", xlab="Semanas", 
        main="Fallecimientos COVID-19 en Veracruz, del 03/03/2020 al 20/01/2021\n
        con una linea de tendencia de un modelo de regresion lineal")
abline(reg = lm(tsCovCdMx~time(tsCovCdMx)))

# La funcion de autocorrelaci0n indica una series no estacionaria.
acf(tsCovCdMx, lag.max = 365, col = terrain.colors(16), main="ACF(Fallecimientos diarios COVID-19 en Veracruz)")


#====================================================
#      COMPONENTES DE LA SERIE DE TIEMPO
#====================================================


##Componentes de la serie generadas con R
plot(decompose(tsCovCdMx))

#=============================================
#Prediccion
#=============================================

# Modelo ARMA(1,0,6) 
ARMA <- arima(tsCovCdMx, order = c(1,0,6))
print(ARMA)
ts.plot(tsCovCdMx)
ARMA_fit <- tsCovCdMx - resid(ARMA)
points(ARMA_fit, type = "l", col = 2, lty = 2)


predict_ARMA <- predict(ARMA)

predict_ARMA$pred[1]

predict_ARMA_15 <- predict(ARMA,n.ahead=15)


#Intervalos de confianza
ts.plot(tsCovCdMx, xlim = c(8, 55), ylim=c(0,1200),
        main="Fallecimientos COVID-19 en Mexico, prediccion 15 dias con intervalos\n
        de 95% de confianza")
ARMA_forecasts <- predict(ARMA, n.ahead = 15)$pred
ARMA_forecast_se <- predict(ARMA, n.ahead = 15)$se
points(ARMA_forecasts, type = "l", col = 2)
points(ARMA_forecasts - 1.96*ARMA_forecast_se, type = "l", col = 2, lty = 2)
points(ARMA_forecasts + 1.96*ARMA_forecast_se, type = "l", col = 2, lty = 2)

sarima(tsCovCdMx,1,0,6,no.constant=F)
