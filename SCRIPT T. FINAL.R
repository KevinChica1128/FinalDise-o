#Diseño y análisis de experimentos
#Trabajo final
#Kevin García
#Alejandro Vargas


#Problema 1




#----------------------------------------------------------#
#Problema 2
secado<-as.factor(c(rep("S",18),rep("A",18),rep("SA",18)))
rallanderia<-as.factor(c(rep(c(rep("R1",9),rep("R2",9)),3)))
trabajador<-as.factor(c(rep(c(rep("T1",3),rep("T2",3),rep("T3",3)),6)))
rendimiento<-c(34.5,40.7,42,48,45.2,49.6,40.8,43,41.7,33.6,35.2,36.8,44,44.4,43.9,36.5,40.4,38.8,
               41.4,43.8,42.6,52.2,51.5,51.7,39.4,45.3,48.6,41.9,42.6,43.7,47,46.5,47.5,40.8,40.9,44.8,
               58,54.8,56.7,62.1,62.5,60.3,51.4,49.9,48.5,52.3,48.5,55.8,58.8,62,63.8,44.9,45.9,49)

#Análisis descriptivo
#Estadisticas descriptivas
#Método de secado:
#Sol
summary(rendimiento[1:18])
sd(rendimiento[1:18])
#Aire caliente
summary(rendimiento[19:36])
sd(rendimiento[19:36])
#Sol y Aire caliente
summary(rendimiento[37:54])
sd(rendimiento[37:54])

#Rallanderia:
#Rallanderia 1
summary(rendimiento[c(1:9,19:27,37:45)])
sd(rendimiento[c(1:9,19:27,37:45)])
#Rallanderia 2
summary(rendimiento[c(10:18,28:36,46:54)])
sd(rendimiento[c(10:18,28:36,46:54)])

#Trabajador:
#Rallanderia 1:
#Trabajador 1
summary(rendimiento[c(1:3,19:21,37:39)])
sd(rendimiento[c(1:3,19:21,37:39)])
#Trabajador 2
summary(rendimiento[c(4:6,22:24,40:42)])
sd(rendimiento[c(4:6,22:24,40:42)])
#Trabajador 3
summary(rendimiento[c(7:9,25:27,43:45)])
sd(rendimiento[c(7:9,25:27,43:45)])

#Rallanderia 2:
#Trabajador 1
summary(rendimiento[c(10:12,28:30,46:48)])
sd(rendimiento[c(10:12,28:30,46:48)])
#Trabajador 2
summary(rendimiento[c(13:15,31:33,49:51)])
sd(rendimiento[c(13:15,31:33,49:51)])
#Trabajador 3
summary(rendimiento[c(16:18,34:36,52:54)])
sd(rendimiento[c(16:18,34:36,52:54)])

#Interacción método de secado - rallanderia:
#Rallanderia 1:
#Sol
summary(rendimiento[1:9])
sd(rendimiento[1:9])
#Aire caliente
summary(rendimiento[19:27])
sd(rendimiento[19:27])
#Sol y Aire caliente
summary(rendimiento[37:45])
sd(rendimiento[37:45])
#Rallanderia 2:
#Sol
summary(rendimiento[10:18])
sd(rendimiento[10:18])
#Aire caliente
summary(rendimiento[28:36])
sd(rendimiento[28:36])
#Sol y Aire caliente
summary(rendimiento[46:54])
sd(rendimiento[46:54])


#Boxplots
#Método de secado
x11()
boxplot(rendimiento~secado,main="Gráfico de cajas del rendimiento(Kg) por método de secado")
#Rallanderia
x11()
boxplot(rendimiento~rallanderia)
#método de secado-rallanderia
x11()
boxplot(rendimiento~secado*rallanderia)
#Rallanderia-trabajador
x11()
boxplot(rendimiento~rallanderia*trabajador)

#Todos juntos
x11()
par(mfrow=c(2,2))
boxplot(rendimiento~secado,main="Gráfico de cajas del rendimiento(Kg) por método de secado")
boxplot(rendimiento~rallanderia,main="Gráfico de cajas del rendimiento(Kg) por rallanderia")
boxplot(rendimiento~secado*rallanderia,main="Gráfico de cajas del rendimiento(Kg) por la interacción método de secado-rallanderia")
boxplot(rendimiento~trabajador*rallanderia,main="Gráfico de cajas del rendimiento(Kg) por trabajador dentro de cada rallanderia")

#Gráfico con todos los factores
x11()
interaction.plot(rallanderia:trabajador,secado,rendimiento,lwd=2,main="Gráfico de lineas del rendimiento(Kg) por método de secado, rallanderia y trabajador")

#Anova
anova<-aov(rendimiento~secado*rallanderia*trabajador%in%rallanderia)
summary(anova)


#Supuestos de los errores del modelo
#Obtenemos los residuales
residuales=residuals(anova)

#Normalidad
#QQ plot e histograma con curva normal superpuesta
x11()
par(mfcol=c(1,2))
hist(residuales, density=5, freq=FALSE, main="Histograma residuos del modelo")
curve(dnorm(x, mean=mean(residuales), sd=sd(residuales)), col="red",
      lwd=2, add=TRUE, yaxt="n")
qqnorm(residuales, main="Q-Q Plot residuos modelo") ## el típico de R
qqline(residuales)

#Test Shapiro wilk(<30 datos)
shapiro.test(residuales)

#Anderson-Darling(>30 datos)
library("nortest")
ad.test(residuales)

#Homocedasticidad de los residuos
datos <- interaction(secado,rallanderia,trabajador)
bartlett.test(residuales~datos)

#Independencia(no correlación) en los errores
#Prueba de rachas: H0:Los residuales se distribuyen de manera aleatoria
library("tseries")
residuales1<-residuales[-31]
residualesfactor<-c()
for (i in 1:length(residuales1)) {
  if (residuales1[i]>0){
    residualesfactor[i]=1
  }
  if (residuales1[i]<0){
    residualesfactor[i]=-1
  }
}
runs.test(factor(residualesfactor))

#Comparaciones múltiples
library(multcompView)
library(lsmeans)
leastsquare3 = lsmeans(anova, ~secado|rallanderia,  adjust="tukey")
cld(leastsquare3, alpha=.05, Letters=letters)

leastsquare2 = lsmeans(anova, ~trabajador|rallanderia,  adjust="tukey")
cld(leastsquare2, alpha=.05, Letters=letters)

