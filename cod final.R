#Trabajo final 
#Alejandro Vargas Franco - 1525953
#Kevin Garcia Chika - 1525953

#datos
library(readxl)
CAMPANA1 <- read_excel("d:/Desktop/trabajo final diseño/CAMPANA1.xlsx")
View(CAMPANA1)
str(CAMPANA1)
#codificacion
Altura=CAMPANA1$ALTURA
Tallos=CAMPANA1$TALLOS
trTallos=sqrt(Tallos)
Hojas=CAMPANA1$`HOJAS VERDES`
trHojas=sqrt(Hojas)
fertilizacion=CAMPANA1$TRATAMIENTO
bloque=CAMPANA1$BLOQUE

x11()
hist(Hojas)
#descriptivas
#forma mejorada (AAAAAA)
sum(Altura)
j=c()
a=c("B1","B2","B3","B4","B5")
se=data.frame(c(1:5))
for (d in 1:length(a)) {
  j=c()
  for (i in 1:length(bloque)) {
    if(bloque[i]==a[d]){
      j=c(j,Altura[i])
    }
    
  }
  se=data.frame(se,j)
}#ciclo que me saca los datos por niveles del factor
colnames(se)=c("#","B1","B2","B3","B4","B5")
summary(se) #descriptivas por cada nivel
des=sqrt(var(se$B1)*((length(se$B1)-1)/length(se$B1)))
cv=(des/mean(se$B1))*100
#grafico
library("ggplot2")
x11()
p<-ggplot(CAMPANA1, aes(bloque, Altura)) + geom_point()
p + scale_x_discrete(name="Bloque",
                     labels=c("1","2","3","4","5")) + labs(y="Altura (cm)") +
  stat_summary(fun.y=mean, colour="red", geom="point", shape=19, size=2)

#boxplot
x11()
boxplot(Altura~fertilizacion+bloque, ylab="Altura (cm)")

#anova
mod<-lm(Altura~fertilizacion+bloque+fertilizacion:bloque, data=CAMPANA1)
anova(mod)

mod2<-lm(Tallos~fertilizacion+bloque+fertilizacion:bloque, data=CAMPANA1)
anova(mod2)

mod3<-lm(Hojas~fertilizacion+bloque+fertilizacion:bloque, data=CAMPANA1)
anova(mod3)

#como para tallos se incumple un supuesto realizamos una trasformacion
#Hacemos la trasformacion de la varianza
library(MASS)
bc1=boxcox(Hojas~1)

lam1=bc1$x[which.max(bc1$y)]#intruccion que nos digaa en x cual es el maximo en y
#Coeficiente de variaicion para el experimento

(sqrt(3.1385)/mean(Altura))*100

#validacion de los supuestos
#supuestos
resid<-residuals(mod3)

#Normalidad:
#Pruebas gráficas:
library(car)
x11()
par(mfrow=c(1,2))
hist(resid, freq=FALSE,main = "Histograma residuos (Tallos)")
curve(dnorm(x,mean(resid), sd(resid)), xlim=c(-20,20), add=TRUE, col=2)
qqPlot(resid, pch=20,main="QQ-Plot de los residuos (Tallos)")

#Prueba formal
#Shapiro
shapiro.test(resid) 

#Anderson-Darling(>30 datos)
library("nortest")
ad.test(resid)

#Media Cero
t.test(resid, mu = 0, alternative = c("two.sided"))


#Homogeneidad de Varianzas
CAMPANA1$int <- interaction(fertilizacion, bloque)
CAMPANA1$int
bartlett.test(resid~CAMPANA1$int, data=CAMPANA1)

#Independencia en los errores
#Prueba de rachas: H0:Los residuales se distribuyen de manera aleatoria
library("tseries")
residualesfactor<-c()
for (i in 1:length(residuals(mod3))) {
  if (residuals(mod3)[i]>0){
    residualesfactor[i]=1
  }
  if (residuals(mod3)[i]<0){
    residualesfactor[i]=-1
  }
}
runs.test(factor(residualesfactor))

#contrastes
library(lsmeans)
leastsquare3 = lsmeans(mod, ~fertilizacion|bloque,  adjust="tukey")
cld(leastsquare3, alpha=.05, Letters=letters)

eastsquare4 = lsmeans(mod, ~bloque|fertilizacion,  adjust="tukey")
cld(eastsquare4, alpha=.05, Letters=letters)

