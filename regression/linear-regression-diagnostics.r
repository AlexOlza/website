library(RcmdrMisc)
library(ggcorrplot)
set.seed(3)

n<-50 #sample size
x1<-30+1:n 
x2<-round(log(x1)+rnorm(n,0,7),1)
x2[n]<-x2[n]*2.5 
x3<-round(rnorm(n,50,10),1) 
x3[n]<-x3[n]*1.5
x4<-rbinom(n,7,0.5) 
x5<-rexp(n,rate=0.2)
y<-40+4*x1**2-x2+2*x3+rnorm(n,0,8)

#modelo de regresion teorico (MUESTRA de y)
#y<-40+4*x1**2-x2+2*x3+rnorm(n,0,8)

#combinamos los datos en un fichero de R
D<-data.frame(cbind(x1,x2,x3,x4,x5,y))
summary(D) 

correlation<-cor(D,method="pearson")
correlation
ggcorrplot(correlation, type = "lower",lab = TRUE)

scatterplotMatrix(~x1+x2+x3+x4+x5+y, regLine=FALSE, smooth=FALSE, diagonal=list(method="density"), data=D)

model1 <- lm(y~x1+x2+x3+x5, data=D)
summary(model1)

oldpar <- par(oma=c(0,0,3,0), mfrow=c(2,2))
plot(model1)
par(oldpar)

x6<-x1**2
model2<-lm(y~x1+x6+x2+x3+x5, data=D)
oldpar <- par(oma=c(0,0,3,0), mfrow=c(2,2))
plot(model2)
par(oldpar)
D<-data.frame(cbind(x1,x6,x2,x3,x5,y))
correlation<-cor(D,method="pearson")
ggcorrplot(correlation, type = "lower",lab = TRUE)
scatterplotMatrix(~x1+x7+x2+x3+x5+y, regLine=FALSE, smooth=FALSE, diagonal=list(method="density"), data=D)

summary(model2) #decido quitar x5

model3<-lm(y~x1+x6+x2+x3, data=D)
summary(model3)#decido quitar x1
oldpar <- par(oma=c(0,0,3,0), mfrow=c(2,2))
plot(model3)


model4<-lm(y~x6+x2+x3, data=D)
summary(model4)
oldpar <- par(oma=c(0,0,3,0), mfrow=c(2,2))
plot(model4)

shapiro.test(model4$residuals) #normality
lmtest::bptest(model4) #homoscedasticity

#a continuacion vemos una forma automatizada de seleccionar
#las variables que participan en un modelo
#------------------------------
#forward
#este metodo va añadiendo variables una a una y realizando contrastes de hipotesis
#la hipotesis: "esta nueva variable introduce diferencias signif al modelo"
#backward mete todas y va quitando una a una las que tienen coef de regresion pequeño
#stepwise
#meto x1. en la segunda etapa entra x3. compruebo de nuevo si x1 sigue teniendo interes
#meto otra. compruebo si x1,x3 siguen teniendo interes
#--------------------------------------
library(MASS, pos=16)
#doing this before removing specification error leads to disaster
stepwise(model1, direction='backward/forward', criterion='BIC')#THIS IS WRONG!!!
stepwise(model3, direction='backward/forward', criterion='BIC')#this is good
finalmodel<-lm(y~x7 + x2 + x3, data = D)
summary(finalmodel)
plot(finalmodel)
