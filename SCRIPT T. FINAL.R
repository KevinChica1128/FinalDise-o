#Diseño y análisis de experimentos
#Trabajo final
#Kevin García
#Alejandro Vargas


#Problema 1
library(readxl)
CAMPANA1 <- read_excel("CAMPANA1.xlsx")
View(CAMPANA1)
attach(CAMPANA1)
#Variables de respuesta
Tallos=CAMPANA1$TALLOS
Altura=CAMPANA1$ALTURA
HVerdes=CAMPANA1$`HOJAS VERDES`

#Factores
Tratamiento=as.factor(CAMPANA1$TRATAMIENTO)
Bloque=as.factor(CAMPANA1$BLOQUE)
Parcela=as.factor(CAMPANA1$PARCELA)

#descriptivas por bloque
sum(Altura)
j=c()
a=c("B1","B2","B3","B4","B5")
se=data.frame(c(1:80))
for (d in 1:length(a)) {
  j=c()
  for (i in 1:length(Bloque)) {
    if(Bloque[i]==a[d]){
      j=c(j,Altura[i])
    }
    
  }
  se=data.frame(se,j)
}#ciclo que me saca los datos por niveles del factor
colnames(se)=c("#","B1","B2","B3","B4","B5")
summary(se[-1]) #descriptivas por cada nivel
des=sd(se$B1)
cv=(des/mean(se$B1))*100

#Descriptivas por tratamiento
j=c()
a=c("T1","T2","T3","T4")
se=data.frame(c(1:100))
for (d in 1:length(a)) {
  j=c()
  for (i in 1:length(Tratamiento)) {
    if(Tratamiento[i]==a[d]){
      j=c(j,Altura[i])
    }
    
  }
  se=data.frame(se,j)
}#ciclo que me saca los datos por niveles del factor
colnames(se)=c("#","T1","T2","T3","T4")
summary(se[-1]) #descriptivas por cada nivel
des=sd(se$T1)
cv=(des/mean(se$T1))*100

#Descriptivas por interacción
CAMPANA1$int <- interaction( Bloque,Tratamiento)
inter=CAMPANA1$int
j=c()
a=c("B1.T1","B2.T1","B3.T1","B4.T1","B5.T1","B1.T2","B2.T2","B3.T2","B4.T2","B5.T2","B1.T3","B2.T3","B3.T3","B4.T3","B5.T3","B1.T4","B2.T4","B3.T4","B4.T4","B5.T4")
se=data.frame(c(1:20))
for (d in 1:length(a)) {
  print(j)
  j=c()
  for (i in 1:length(inter)) {
    if(inter[i]==a[d]){
      j=c(j,HVerdes[i])
      
    }
    
  }
  se=data.frame(se,j)
}
colnames(se)=c("#","B1.T1","B2.T1","B3.T1","B4.T1","B5.T1","B1.T2","B2.T2","B3.T2","B4.T2","B5.T2","B1.T3","B2.T3","B3.T3","B4.T3","B5.T3","B1.T4","B2.T4","B3.T4","B4.T4","B5.T4")
summary(se[-1])
des=sd(se$B4.T4)
cv=(des/mean(se$B1.T1))*100

mean(Tallos)
mean(Altura)
mean(HVerdes)
#Tratamiento-Bloque
#Altura
x11()
boxplot(Altura~Bloque)
x11()
boxplot(Altura~Tratamiento)

#Tallos
x11()
boxplot(Tallos~Bloque)
x11()
boxplot(Tallos~Tratamiento)

#Hojas verdes
x11()
boxplot(HVerdes~Bloque)
x11()
boxplot(HVerdes~Tratamiento)

#Modelos:
#Modelo 1
mod1<-aov(Tallos~Tratamiento+Bloque+Error(Parcela))
summary(mod1)
ad.test(residuals(mod1$Parcela))
ad.test(residuals(mod1$Within))
#Modelo 2
mod2<-aov(Altura~Tratamiento+Bloque+Error(Parcela))
summary(mod2)
ad.test(residuals(mod2$Parcela))
ad.test(residuals(mod2$Within))
#Modelo 3
mod3<-aov(HVerdes~Tratamiento+Bloque+Error(Parcela))
summary(mod3)
ad.test(residuals(mod3$Parcela))
ad.test(residuals(mod3$Within))

#Supuestos
#Modelo 1
residuales1=residuals(mod1)

#Normalidad
#QQ plot e histograma con curva normal superpuesta
x11()
par(mfcol=c(1,2))
hist(residuales1, density=5, freq=FALSE, main="Histograma residuos del modelo")
curve(dnorm(x, mean=mean(residuales1), sd=sd(residuales1)), col="red",
      lwd=2, add=TRUE, yaxt="n")
qqnorm(residuales1, main="Q-Q Plot residuos modelo") ## el típico de R
qqline(residuales1)

#Test Shapiro wilk(<30 datos)
shapiro.test(residuales1)

#Anderson-Darling(>30 datos)
library("nortest")
ad.test(residuales1)

#Homocedasticidad de los residuos
datos1 <- interaction(Tratamiento,Bloque)
bartlett.test(residuales1~datos1)

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

#Modelo 2
residuales2=residuals(mod2)

#Normalidad
#QQ plot e histograma con curva normal superpuesta
x11()
par(mfcol=c(1,2))
hist(residuales2, density=5, freq=FALSE, main="Histograma residuos del modelo")
curve(dnorm(x, mean=mean(residuales2), sd=sd(residuales2)), col="red",
      lwd=2, add=TRUE, yaxt="n")
qqnorm(residuales2, main="Q-Q Plot residuos modelo") ## el típico de R
qqline(residuales2)

#Test Shapiro wilk(<30 datos)
shapiro.test(residuales2)

#Anderson-Darling(>30 datos)
library("nortest")
ad.test(residuales2)

#Homocedasticidad de los residuos
datos1 <- interaction(Tratamiento,Bloque)
bartlett.test(residuales2~datos1)

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

#Modelo 3
residuales3=residuals(mod3)

#Normalidad
#QQ plot e histograma con curva normal superpuesta
x11()
par(mfcol=c(1,2))
hist(residuales3, density=5, freq=FALSE, main="Histograma residuos del modelo")
curve(dnorm(x, mean=mean(residuales3), sd=sd(residuales3)), col="red",
      lwd=2, add=TRUE, yaxt="n")
qqnorm(residuales3, main="Q-Q Plot residuos modelo") ## el típico de R
qqline(residuales3)

#Test Shapiro wilk(<30 datos)
shapiro.test(residuales3)

#Anderson-Darling(>30 datos)
library("nortest")
ad.test(residuales3)

#Homocedasticidad de los residuos
datos1 <- interaction(Tratamiento,Bloque)
bartlett.test(residuales3~datos1)

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

#POSTANOVA
#MODELO 1
library(multcompView)
library(lsmeans)
leastsquare = lsmeans(mod1, ~Bloque,  adjust="tukey")
cld(leastsquare, alpha=.05, Letters=letters)
#MODELO 2
leastsquare1 = lsmeans(mod2, ~Bloque,  adjust="tukey")
cld(leastsquare1, alpha=.05, Letters=letters)
#MODELO 3
leastsquare2 = lsmeans(mod3, ~Bloque,  adjust="tukey")
cld(leastsquare2, alpha=.05, Letters=letters)


#------------------------------------------------------------#
#DADO QUE NO SE CUMPLE LA NORMALIDAD PARA LOS MODELOS 1 Y 3, 
#SE DEBE APLICAR UNA TRANSFORMACIÓN A LOS DATOS
#con box cox
library(car)
summary(powerTransform(Tallos))
summary(powerTransform(HVerdes))
#con jhonson
install.packages("jtrans")
library(jtrans)
library(nortest)
jh=jtrans(Tallos, test="ad.test")

trTallos=jh$transformed
trHojas=HVerdes^2
trTallos2=sqrt(Tallos)

#Descriptivas paras los datos transformados
CAMPANA1$int <- interaction( Bloque,Tratamiento)
inter=CAMPANA1$int
j=c()
a=c("B1.T1","B2.T1","B3.T1","B4.T1","B5.T1","B1.T2","B2.T2","B3.T2","B4.T2","B5.T2","B1.T3","B2.T3","B3.T3","B4.T3","B5.T3","B1.T4","B2.T4","B3.T4","B4.T4","B5.T4")
se=data.frame(c(1:20))
for (d in 1:length(a)) {
  print(j)
  j=c()
  for (i in 1:length(inter)) {
    if(inter[i]==a[d]){
      j=c(j,trHojas[i])
      
    }
    
  }
  se=data.frame(se,j)
}
colnames(se)=c("#","B1.T1","B2.T1","B3.T1","B4.T1","B5.T1","B1.T2","B2.T2","B3.T2","B4.T2","B5.T2","B1.T3","B2.T3","B3.T3","B4.T3","B5.T3","B1.T4","B2.T4","B3.T4","B4.T4","B5.T4")
summary(se[-1])
des=sd(se$B4.T4)
cv=(des/mean(se$B1.T1))*100


#Modelos nuevos
#Modelo 1 transformado con jhonson
mod1T<-aov(trTallos~Tratamiento*Bloque)
summary(mod1T)
  
#Transformado con box cox
mod1T2<-aov(trTallos2~Tratamiento*Bloque)
summary(mod1T2)

#Modelo 3 transformado
mod3T<-aov(trHojas~Tratamiento*Bloque)
summary(mod3T)

#Supuestos para los nuevos modelos
#Modelo 1 transformado
residuales1T=residuals(mod1T)

#La transformación de box cox no funciona muy bien:
residuales1T2=residuals(mod1T2)
ad.test(residuales1T2)
x11()
par(mfcol=c(1,2))
hist(residuales1T2, density=5, freq=FALSE, main="Histograma residuos del modelo")
curve(dnorm(x, mean=mean(residuales1T2), sd=sd(residuales1T2)), col="red",
      lwd=2, add=TRUE, yaxt="n")
qqnorm(residuales1T2, main="Q-Q Plot residuos modelo") ## el típico de R
qqline(residuales1T2)

#Normalidad
#QQ plot e histograma con curva normal superpuesta
x11()
par(mfcol=c(1,2))
hist(residuales1T, density=5, freq=FALSE, main="Histograma residuos del modelo")
curve(dnorm(x, mean=mean(residuales1T), sd=sd(residuales1T)), col="red",
      lwd=2, add=TRUE, yaxt="n")
qqnorm(residuales1T, main="Q-Q Plot residuos modelo") ## el típico de R
qqline(residuales1T)

#Test Shapiro wilk(<30 datos)
shapiro.test(residuales1T)

#Anderson-Darling(>30 datos)
library("nortest")
ad.test(residuales1T)

#Homocedasticidad de los residuos
datos1 <- interaction(Tratamiento,Bloque)
bartlett.test(residuales1T~datos1)

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

#Modelo 3 transformado
residuales3T=residuals(mod3T)

#Normalidad
#QQ plot e histograma con curva normal superpuesta
x11()
par(mfcol=c(1,2))
hist(residuales3T, density=5, freq=FALSE, main="Histograma residuos del modelo")
curve(dnorm(x, mean=mean(residuales3T), sd=sd(residuales3T)), col="red",
      lwd=2, add=TRUE, yaxt="n")
qqnorm(residuales3T, main="Q-Q Plot residuos modelo") ## el típico de R
qqline(residuales3T)

#Test Shapiro wilk(<30 datos)
shapiro.test(residuales3T)

#Anderson-Darling(>30 datos)
library("nortest")
ad.test(residuales3T)

#Homocedasticidad de los residuos
datos1 <- interaction(Tratamiento,Bloque)
bartlett.test(residuales1T~datos1)

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
#-----------------------------------------------------------#




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
#Sol:
#Rallanderia 1
summary(rendimiento[1:9])
sd(rendimiento[1:9])
#Rallanderia 2
summary(rendimiento[10:18])
sd(rendimiento[10:18])
#Aire caliente:
#Rallanderia 1
summary(rendimiento[19:27])
sd(rendimiento[19:27])
#Rallanderia 2
summary(rendimiento[28:36])
sd(rendimiento[28:36])
#Sol y Aire caliente:
#Rallanderia 1
summary(rendimiento[37:45])
sd(rendimiento[37:45])
#Rallanderia 2:
summary(rendimiento[46:54])
sd(rendimiento[46:54])

#Interacción método de secado - trabajador(rallanderia):
#Sol:
#Rallanderia 1:
#Trabajador 1
summary(rendimiento[1:3])
sd(rendimiento[1:3])
#Trabajador 2
summary(rendimiento[4:6])
sd(rendimiento[4:6])
#Trabajador 3
summary(rendimiento[7:9])
sd(rendimiento[7:9])
#Rallanderia 2:
#Trabajador 1
summary(rendimiento[10:12])
sd(rendimiento[10:12])
#Trabajador 2
summary(rendimiento[13:15])
sd(rendimiento[13:15])
#Trabajador 3
summary(rendimiento[16:18])
sd(rendimiento[16:18])
#Aire caliente:
#Rallanderia 1:
#Trabajador 1
summary(rendimiento[19:21])
sd(rendimiento[19:21])
#Trabajador 2
summary(rendimiento[22:24])
sd(rendimiento[22:24])
#Trabajador 3
summary(rendimiento[25:27])
sd(rendimiento[25:27])
#Rallanderia 2:
#Trabajador 1
summary(rendimiento[28:30])
sd(rendimiento[28:30])
#Trabajador 2
summary(rendimiento[31:33])
sd(rendimiento[31:33])
#Trabajador 3
summary(rendimiento[34:36])
sd(rendimiento[34:36])
#Sol y Aire caliente:
#Rallanderia 1:
#Trabajador 1
summary(rendimiento[37:39])
sd(rendimiento[37:39])
#Trabajador 2
summary(rendimiento[40:42])
sd(rendimiento[40:42])
#Trabajador 3
summary(rendimiento[43:45])
sd(rendimiento[43:45])
#Rallanderia 2:
#Trabajador 1
summary(rendimiento[46:48])
sd(rendimiento[46:48])
#Trabajador 2
summary(rendimiento[49:51])
sd(rendimiento[49:51])
#Trabajador 3
summary(rendimiento[52:54])
sd(rendimiento[52:54])

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
leastsquare4 = lsmeans(anova, ~secado|rallanderia,  adjust="tukey")
cld(leastsquare4, alpha=.05, Letters=letters)

leastsquare5 = lsmeans(anova, ~trabajador|rallanderia,  adjust="tukey")
cld(leastsquare5, alpha=.05, Letters=letters)

leastsquare6 = lsmeans(anova, ~rallanderia,  adjust="tukey")
cld(leastsquare6, alpha=.05, Letters=letters)
