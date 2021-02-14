#SUPERVIVENCIA DE PACIENTES CON BRONQUITIS
#Pablo Suárez y Alexander Olza
#Diciembre de 2020

library(survival)
library(ggfortify)#contiene autoplot
library(gridExtra)#para poner las autoplot en una grid, no vale hacer par(mfrow...)
library(dplyr)#contiene la funcion mutate
#REFERENCIAS DE LA PARTE DESCRIPTIVA: https://rviews.rstudio.com/2017/09/25/survival-analysis-with-r/
#-----------------------------------------

#bronquitis<- read.table("C:/Users/Pablo/Desktop/MASTER/Modelizacion_estadistica_2/trabajo/bronquitis.txt",header=TRUE)
bronquitis<- read.table("/home/alex/Downloads/master/modelizacion-estadistica/parte2/bronquitis/bronquitis.txt",header=TRUE)
names(bronquitis)
bronquitis$ENCUESTA_N_<-factor(bronquitis$ENCUESTA_N_)
bronquitis$sexon<-factor(bronquitis$sexon) 
bronquitis$ejercicio<-factor(bronquitis$ejercicio) 
bronquitis$disnea<-factor(bronquitis$disnea)  

##############################################################################
############################ Depuracion de los datos #########################
##############################################################################


#comprobamos si hay valores NA...  
for ( i in names(bronquitis)){        
  print(which(is.na(bronquitis$i)))  #...y vemos que no hay ninguno
}
attach(bronquitis)
#Haciendo print de los niveles de las variables cuantitativas comprobamos si hay valores fuera de los indicados por el enunciado.
levels(sexon) ; levels(ejercicio) ; levels(disnea) #Vemos que no hay ningún valor discordante para las variables cualitativas.

#Hacemos lo mismo para las variables cuantitativas haciendo summary:
summary(bmi) ; summary(WDist) ; summary(fev) #Vemos que (en principio) no hay ningún valor discordante.

############################################################################
####################    ESTADÍSTICA DESCRIPTIVA     ########################
############################################################################
summary(disnea)#Solo 17 con disnea 4 y 11 con disnea 5
summary(sexon)#20 mujeres
length(which(fallecido==1))#murieron 128 en total
hist(tiempo_superv[which(fallecido==1)],breaks =50)
tiempo_superv[which(fallecido==0)]#todas las censuras ocurrieron en t=1825
sort(tiempo_superv[which(fallecido==1)])#Después de las censuras murieron los 5 restantes

#Supervivencia de grupos con distintas características:

names(bronquitis)
summary(edad)#La mediana es 70, partimos por ahí
summary(bmi)#partimos en 28
summary(WDist)#414
summary(fev)#58

bronquitis2<-mutate(bronquitis,edad70=ifelse(edad<70,'Menor70','Mayor70'),
                    bmi28=ifelse(bmi<28,'Menor28','Mayor28'),
                    WDist414=ifelse(WDist<414,'Menor414','Mayor414'),
                    fev58=ifelse(fev<58,'Menor58','Mayor58'))
#Población general
kaplanmeier <-  survfit(Surv(tiempo_superv,fallecido)~1, conf.type="log-log" )
summary(kaplanmeier)
autoplot(kaplanmeier,xlab = 'Tiempo',ylab='Supervivencia',censor.shape = '*', censor.size = 10,censor.colour = 'red')
#Distintos estratos
km.edad <-  survfit(Surv(tiempo_superv,fallecido)~edad70, conf.type="log-log",data = bronquitis2 )
km.bmi <-  survfit(Surv(tiempo_superv,fallecido)~bmi28, conf.type="log-log",data = bronquitis2 )
km.WDist <-  survfit(Surv(tiempo_superv,fallecido)~WDist414, conf.type="log-log",data = bronquitis2 )
km.fev <-  survfit(Surv(tiempo_superv,fallecido)~fev58, conf.type="log-log",data = bronquitis2 )
km.sexo<-  survfit(Surv(tiempo_superv,fallecido)~sexon, conf.type="log-log" )
km.disnea<-survfit(Surv(tiempo_superv,fallecido)~disnea, conf.type="log-log" )
km.ejercicio<-survfit(Surv(tiempo_superv,fallecido)~ejercicio, conf.type="log-log" )
autoplot(kaplanmeier,main='Estimación Kaplan-Meier para toda la población',xlab = 'Tiempo',ylab='Supervivencia',censor.shape = '*', censor.size = 10,censor.colour = 'red')
p1<-autoplot(km.edad,main='Efecto de la edad',xlab = 'Tiempo',ylab='Supervivencia'
             ,censor.shape = '*', censor.size = 5,censor.colour = 'red')
p2<-autoplot(km.sexo,main='Efecto del sexo',xlab = 'Tiempo',ylab='Supervivencia'
             ,censor.shape = '*', censor.size = 5,censor.colour = 'red')
p3<-autoplot(km.disnea,main='Efecto de la disnea',xlab = 'Tiempo',ylab='Supervivencia'
             ,censor.shape = '*', censor.size = 5,censor.colour = 'red')
p4<-autoplot(km.ejercicio,main='Efecto del ejercicio',xlab = 'Tiempo',ylab='Supervivencia'
             ,censor.shape = '*', censor.size = 5,censor.colour = 'red')
p5<-autoplot(km.bmi,main='Efecto del BMI',xlab = 'Tiempo',ylab='Supervivencia'
             ,censor.shape = '*', censor.size = 5,censor.colour = 'red')
p6<-autoplot(km.WDist,main='Efecto de WDist',xlab = 'Tiempo',ylab='Supervivencia'
             ,censor.shape = '*', censor.size = 5,censor.colour = 'red')
p7<-autoplot(km.fev,main='Efecto de FEV',xlab = 'Tiempo',ylab='Supervivencia'
             ,censor.shape = '*', censor.size = 5,censor.colour = 'red')
grid.arrange(p1,p2,p3,p4,p5,p6,p7,ncol=2)
summary(sexon)#TENEMOS SOLO 20 MUJERES, un 4%, no se puede estudiar bien el efecto



############################################################################
############################ Análisis univariantes #########################
############################################################################

edad.lm<-coxph(Surv(tiempo_superv,fallecido)~edad)
summary(edad.lm) #La variable edad es significativa
mod0<-coxph(Surv(tiempo_superv,fallecido)~1)
anova(mod0,edad.lm)


sexo.lm<-coxph(Surv(tiempo_superv,fallecido)~sexon)
summary(sexo.lm) #Sexo considerar al final (p-valor=0.3 no es significativa por si sola, pero puede serlo en el modelo con las demás variables.)
anova(mod0,sexo.lm)
bmi.lm<-coxph(Surv(tiempo_superv,fallecido)~bmi)
summary(bmi.lm) # bmi NO significativo (Aún así lo meteremos igualmente en el AIC)
anova(mod0,bmi.lm)
WDist.lm<-coxph(Surv(tiempo_superv,fallecido)~WDist)
summary(WDist.lm) #WDist Significativo
anova(mod0,WDist.lm)

fev.lm<-coxph(Surv(tiempo_superv,fallecido)~fev)
summary(fev.lm) #significativo
anova(mod0,fev.lm)

ejercicio.lm<-coxph(Surv(tiempo_superv,fallecido)~ejercicio)
summary(ejercicio.lm) #NO significativo (Aún así lo meteremos igualmente en el AIC)
anova(mod0,ejercicio.lm)

disnea.lm<-coxph(Surv(tiempo_superv,fallecido)~disnea)
summary(disnea.lm) #significativa
anova(mod0,disnea.lm)
disnea.lm$concordance[6]


############################################################################
######################## Test AIC ######################################
############################################################################

#Para el modelo el test AIC utilizaremos todas las variables aunque no sean significativas por si solas. 
#Una vez tengamos el mejor modelo comprobaremos si las variables que hay en el son significativas.


modelo.all<-coxph(Surv(tiempo_superv,fallecido)~edad+sexon+bmi+WDist+fev+ejercicio+disnea) 

AIC.step<-step(modelo.all,
                    scope=list(upper=~ edad+sexon+bmi+WDist+fev+ejercicio+disnea, #Con upper decimos que empezamos por el modelo completo.
                               lower=~WDist)) #Con lower le decimos que al menos nos deje el modelo univariante con WDist (era el que tenía menor p-valor univariante)

#Según el criterio de AIC, el mejor modelo es el que tiene en cuenta las variables edad, WDist y fev. 
#Comprobemos si son significativas:
#Para ello primero creemos los diferentes modelos:

modelo.edad.WDist.fev<-coxph(Surv(tiempo_superv,fallecido)~edad+WDist+fev) 
summary(modelo.edad.WDist.fev)
modelo.edad.WDist<-coxph(Surv(tiempo_superv,fallecido)~edad+WDist) 
summary(modelo.edad.WDist)
modelo.edad.fev<-coxph(Surv(tiempo_superv,fallecido)~edad+fev) 
summary(modelo.edad.fev)
modelo.WDist.fev<-coxph(Surv(tiempo_superv,fallecido)~WDist+fev)
summary(modelo.WDist.fev)

#Con la funcion anova haremos el test de razón de verosimilitud para ver si las variables son significativas.

anova(modelo.edad.WDist.fev,modelo.edad.WDist) #La variable fev es significativa
anova(modelo.edad.WDist.fev,modelo.edad.fev) #La variable WDist es significativa.
anova(modelo.edad.WDist.fev,modelo.WDist.fev) #La variable edad es significativa. 



######################################################################################################
####################### Relacion lineal entre covariables continuas y log-hazard #####################
######################################################################################################

#Haciendo un spline veremos si hay alguna covariable que tiene relacion no lineal con el log-hazard.
modelo.spline.edad<-coxph(Surv(tiempo_superv,fallecido)~pspline(edad)+WDist+fev)
modelo.spline.edad #El término no lineal no es significativo (por poco)

#Hacemos la gráfica para asegurarnos ver si la relacion es lineal o no. 

#Asumo que es lineal porque no tengo datos suficientes para decir que no lo es. 

modelo.spline.WDist<-coxph(Surv(tiempo_superv,fallecido)~edad+pspline(WDist)+fev) 
modelo.spline.WDist #Término no lineal no significativo 

modelo.spline.fev<-coxph(Surv(tiempo_superv,fallecido)~edad+WDist+pspline(fev)) 
modelo.spline.fev #Término no lineal no significativo 
par(mfrow=c(1,3))
termplot(modelo.spline.edad,se=TRUE,terms=1,xlabs="Edad",ylabs="Log hazard")
#Vemos que en a priori es lineal (excepto al principio, pero tenemos mucha desviación típica)
termplot(modelo.spline.WDist,se=TRUE,terms=2,xlabs="WDist",ylabs="") 
termplot(modelo.spline.fev,se=TRUE,terms=3,ylabs="",xlabs="fev")
#Los tres termplot permiten trazar una recta dentro de los intervalos de confianza


########################################################################################################
######################### Suposición de riesgos proporcionales #########################################
########################################################################################################

result.res <- cox.zph(modelo.edad.WDist.fev, transform="rank")
result.res #Obtenemos p-valores mayores que 0.05 en todos los casos, por lo que la suposicion de proporcionalidad es correcta.


#################################################################################################
#######################       VEROSIMILITUD DE LOS DISTINTOS MODELOS      #######################
#################################################################################################
logLik(modelo.edad.WDist.fev)
logLik(modelo.edad.fev)
logLik(modelo.edad.WDist)
logLik(modelo.WDist.fev)
logLik(modelo0)
#El más verosímil es modelo.edad.WDist.fev. 
#Esto ya se ha tenido en cuenta en el AIC

#################################################################################################
#######################   CAPACIDAD DISCRIMINATIVA  EN LA  MUESTRA       ########################
#################################################################################################

par(mfrow=c(1,1))
modelo.edad.WDist.fev$concordance[6]
#71%, no está mal. Pero estará sobreestimada, hay que validar
modelo.edad.WDist$concordance[6]#Peor
modelo.edad.fev$concordance[6]#peor
modelo.WDist.fev$concordance[6]#peor
#################################################################################
####################          VALIDACIÓN DEL MODELO       #######################
#################################################################################
optimismo_bootstrap<-function(datos,B,formula_modelo){
  optimismo<-0
  for (b in 1:B){
  set.seed(314)
  muestra<-datos[sample(1:nrow(datos),size=nrow(datos),replace=TRUE),]
  modelo_boot<-coxph(formula_modelo,data=muestra) 
  fixbeta<-modelo_boot$coefficients
  #esto aplica el mismo modelo_boot a la muestra original:
  #usa los mismos betas (init=fixbeta), 
  #y no hace ninguna iteración para maximizar la verosimilitud parcial (iter=0), 
  #así que no cambia los betas
  modelo_o<-coxph(formula_modelo , data=datos,
                  init=fixbeta, iter=0) 
  #le sumo al optimismo la diferencia de c_index para esta iteración:
  optimismo<-optimismo+modelo_boot$concordance[6]-modelo_o$concordance[6]
  }
  optimismo<-optimismo/B #al final del todo divido el optimismo por B
  return('optimismo'=optimismo)
}

optimismo<-optimismo_bootstrap(bronquitis,100,Surv(tiempo_superv,fallecido)~edad+WDist+fev)
c_index_aparente<-modelo.edad.WDist.fev$concordance[6]
c_index_corregido<-c_index_aparente-optimismo
optimismo
c_index_corregido
#Nuestro modelo tiene probabilidad de concordancia corregida  0.70361:
