## -------------------------------------------------------------------------
## SCRIPT: PR2_AraceliMacia
## CURSO: Master en Big Data y Business Analytics
## ASIGNATURA: Análisis Estadístico
## ALUMNO: Araceli Macia Barrado
## FECHA: 08/12/2016
## DESCRIPCION : Script para la solucion del Segundo Ejercicio: Series Temporales.
## -------------------------------------------------------------------------



if (!require("forecast")){
  install.packages("forecast") 
  library(forecast)
}

setwd("~/Documents/3.CIFF/18.ANALISIS ESTADISTICO/Prac2 anaEstadistico")
 
## Lectura de datos.
datos=read.csv("monthly-milk-production-pounds-p.csv",head=TRUE,sep=";")
colnames(datos) <- c("Fecha","Produccion")  #cambio de nombre las columnas
str(datos)
head(datos)
tail(datos) 
#La ultima fila tengo que eliminarla.
datos<-datos[1:nrow(datos)-1,]
tail(datos) 

plot(datos) # en el grafico no se va nada, hay que convertir los datos a serie temporal.

Datosts=ts(datos$Produccion,start=1962,frequency=12)
plot(Datosts) #se ve que hay una tendencia y patron anual

acf(Datosts,lag.max=48) 
pacf(Datosts,lag.max=48) 
tsdisplay(Datosts)  # se ve clara la tendencia y el patron anual.


#Ejecutamos el modelo automatico para la prediccion
# que encontrará el mejor modelo ARIMA para nuestros datos.

Auto_milk_model=auto.arima(Datosts,trace=TRUE) # esto lo hace todo
#ha encontrado elmejor modelo Arima :  Best model: ARIMA(0,1,1)(0,1,1)[12]   
summary(Auto_milk_model)
tsdisplay(residuals(Auto_milk_model)) #para ver los residuos de la prediccion.
hist(residuals(Auto_milk_model))
qqnorm(residuals(Auto_milk_model)); qqline(residuals(Auto_milk_model),col=2)


plot(forecast(Auto_milk_model,h=24)) #plot de la prediccion
forecast(Auto_milk_model,h=24)

Ventas_arima=arima(Datosts,order=c(0,1,1),seasonal=list(order = c(0, 1, 1)))
plot(forecast(Ventas_arima,h=24))
summary(Ventas_arima)
