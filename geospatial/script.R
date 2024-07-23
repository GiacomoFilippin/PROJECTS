rm(list=ls())
install.packages('survival')
library(survival)
library(SurvRegCensCov)
source("C:/Users/giaco/Desktop/uni/modelli stat. applicati/lab/Lab-R-2/ConvertExpWeibull.R")
dati <- read.csv("C:/Users/giaco/Desktop/uni/modelli stat. applicati/progetto/conflict10.csv")

dati$EventType = as.factor(dati$EventType)
dati$Actor1 = as.factor(dati$Actor1)
dati$Actor2 = as.factor(dati$Actor2)
  
mean(dati[dati$EventType == 'Battles',]$Year)
mean(dati[dati$EventType == 'Riots',]$Year)
mean(dati[dati$EventType == 'Violence against civilians',]$Year)
levels(as.factor(dati$Location)) # tanti
levels(as.factor(dati$Country))  # solo uno
levels(as.factor(dati$Actor1))   # troppi
levels(as.factor(dati$Actor2))   # troppi
levels(as.factor(dati$Region))   # troppi
levels(as.factor(dati$EventType)) # ok
dati$EventType = as.factor(dati$EventType)
levels(as.factor(dati$TempCat))   # ok
dati$TempCat = as.factor(dati$TempCat)

library(survival)
Surv(dati$Year)

attach(dati)

boxplot(Year ~ EventType)

status = rep(1, length(dati[,1]))

Shat <- survfit(Surv(Year,status) ~ 1 + EventType, stype=1, ctype=1, se.fit=TRUE, data=dati)
summary(Shat)
plot(Shat, conf.int=F, mark.time=TRUE,xlab="tempo (anni)", ylab="Shat(t)", xlim = c(1995, 2012), col = 1:3)
Shatna <- survfit(Surv(Year,status) ~ 1 + EventType, stype=2, data=dati)
lines(Shatna, lty=2, col = 1:3)

plot(Shat,  col=c("black","red","blue","green"), fun="cloglog",  xlab="Log(t)", ylab= expression("log(-log("* hat(S)*"(t)))"))


f1<-survreg(Surv(Year,status) ~ 1 + EventType, dist = 'exponential')
summary(f1)
fa <- exp(beta1)   # calcoliamo il fattore di accelerazione

f3<-survreg(Surv(Year-1995,status) ~ 1 + EventType, scale=1)
summary(f3)
# calcoliamo il fattore di accelerazione
fa <- exp(beta1) 
####################################################################################
#######  modello di regressione esponenziale A RISCHI MOLTIPLICATIVI ###############

# vediamo la rappresentazione a rischi moltiplicativi a partire dal modello AFT
f3_prop = ConvertExpWeibull(f3, scale=1)
f3_prop
lambda <- f3_prop$vars[1,1]
gamma1 <- f3_prop$vars[3,1]

HR = exp(gamma1)  # interpretazione!



###### Costruiamo le curve di sopravvivenza stimate:

# curva di riferimento per Z=0 (ossia voltage=20)
curve( exp(-lambda*t), 0, max(Year-1995), ylim=c(0,1),xname="t",xlab="Tempo (anni)" , ylab=expression(hat(S)*"(t)"))
# curva di riferimento per Z=6  (ossia voltage=26)
HR26 <- exp(gamma1*5)
curve( ( exp(-lambda*t) )^HR26 , 0, max(Year-1995), xname="t", add=TRUE,col="red")
HR29 <- exp(gamma1*10)
curve( ( exp(-lambda*t) )^HR29 , 0, max(Year-1995), xname="t", add=TRUE,col="blue")
HR32 <- exp(gamma1*15)
curve( ( exp(-lambda*t) )^HR32 , 0, max(Year-1995), xname="t", add=TRUE,col="green")

### Possiamo sovraimporre le curve stimate nonparametric. con K-M
s2 <-survfit(Surv(Year-1995,status)~1 + EventType, data=dati)
lines(s2, conf.int=FALSE, xlab="Tempo (giorni)", col=c("black","red","blue","green"), ylab=expression(hat(S)*"(t)"))



### svolgiamo una verifica grafica dell'adattamento del modello esponenziale
# confrontando le stime di log H(t) = log(-log(S(t))) nonpar. e sotto il modello param
# per i 4 gruppi
# s2[1] contiene le informazioni relative al primo gruppo con voltage =20
# s2[2] contiene le informazioni relative al secondo gruppo con voltage =26
# etc...
# H(20)= 0  non ci sono rotture (S(t)=1 nel gruppo voltage=20), quindi non possiamo calcolare logH
logH_26 <- log(-log(s2[2]$surv))
logH_29 <- log(-log(s2[3]$surv))
logH_32 <- log(-log(s2[4]$surv))
plot(log(s2[2]$time), logH_26, xlim=c(-0.4,5.7), ylim=c(-4,2), type="s",col="red",xlab="log Tempo", ylab= "log H(t)" )
lines(log(s2[3]$time), logH_29, type="s",col="blue")
lines(log(s2[4]$time), logH_32, type="s",col="green")

###  aggiungo le curve parametriche H(t) già calcolate sopra per i due gruppi
curve( log(lambda*t),  0, max(log(days)),xname="t", add=TRUE)
curve( log(HR26) +log(lambda) +logt, 0.0001, max(log(days)),
       xname="logt", log="x", add=TRUE, col="red")
## per il gruppo con voltage=26, il modello esponenziale sembra appropriato.
curve( log(HR29) +log(lambda) +logt, 0.0001, max(log(days)),
       xname="logt", log="x", add=TRUE, col="blue")
curve( log(HR32) +log(lambda) +logt, 0.0001, max(log(days)),
       xname="logt", log="x", add=TRUE, col="green")
# andamento peggiore per i gruppi con voltaggio 29 e 32



###  Il criterio d'informazione di Akaikie (AIC) è
AIC(f3)
AIC_exp <- - 2*f3$loglik + 2*(1+1)
anova(f3)






#################  modello di regressione Weibull  A TEMPI ACCELERATI  #################
## il modello log-lineare è:  Y= \mu + \beta1 * voltage + \sigma W
voltage_ref <- voltage - 20   ## voltaggio di riferimento è 20
f4<-survreg(Surv(days,event)~voltage_ref, data=cap)
summary(f4)



# calcoliamo il fattore di accelerazione



#################  modello di regressione Weibull  A RISCHI MOLTIPLICATIVI   #################
f4_prop = ConvertExpWeibull(f4)
f4_prop
lambda <- f4_prop$vars[1,1]
alpha <- f4_prop$vars[2,1]
gamma1 <- f4_prop$vars[3,1]






## Costruiamo le curve di sopravvivenza stimate:
# curva di riferimento per Z=0 (ossia voltage=20)
curve( exp(-lambda*t^alpha), 0, max(days), ylim=c(0,1),xname="t",xlab="Tempo (giorni)" , ylab=expression(hat(S)*"(t)"))
# curva di riferimento per Z=6  (ossia voltage=26)
curve( ( exp(-lambda*t^alpha) )^HR26 , 0, max(days), xname="t", add=TRUE,col="red")
# curva di riferimento per Z=9  (ossia voltage=29)
curve( ( exp(-lambda*t^alpha) )^HR29 , 0, max(days), xname="t", add=TRUE,col="blue")
# curva di riferimento per Z=12  (ossia voltage=32)
curve( ( exp(-lambda*t^alpha) )^HR32 , 0, max(days), xname="t", add=TRUE,col="green")


### svolgiamo una verifica grafica dell'adattamento del modello Weibull, 
# log(H(t))=log(-log S(t)) vs log(t)
s2 <-survfit(Surv(days,event)~voltage, data=cap)
plot(s2,  col=c("black","red","blue","green"), fun="cloglog",  xlab="Log(t)", ylab= expression("log(-log("* hat(S)*"(t)))"))
# aggiungo la curva parametrica di log(H(t)) per ciascun gruppo di voltaggio 
# osservo il min e max di sort(s2$time)
tt <- seq(0,300,by=0.01)
volt <- unique(voltage_ref)
HR <- exp(gamma1*volt)   ## HR per i quattro valori di voltage_ref
logH1 <-log(HR[1] * lambda*(tt^alpha)) 
logH2 <-log(HR[2] * lambda*(tt^alpha)) 
logH3 <-log(HR[3] * lambda*(tt^alpha)) 
logH4 <-log(HR[4] * lambda*(tt^alpha)) 
lines(tt, logH1, type="l", col="black")
lines(tt, logH2, type="l", col="red")
lines(tt, logH3, type="l", col="blue")
lines(tt, logH4, type="l", col="green")
## il modello Weibull sembra abbastanza appropriato, tuttavia l'assunzione di rischi proporzionali sembrerebbe violata (curva blue)


######### CONFRONTO tra modelli
anova(f3, f4)
W = 2*(f4$loglik[2] -f3$loglik[2])
1-pchisq(W, df=1)


###  Il criterio d'informazione di Akaikie (AIC) è
AIC_wei <- - 2*f4$loglik + 2*(1+2)
AIC_exp[2] 




######## tempi mediani per i quattro valori di voltaggio, 
#   e per un nuovo valore di voltaggio pari a 30

median4 <- predict(f4,newdata=list(voltage_ref=c(0,6,9,12)), type="quantile",p=0.5)


## Verifica grafica: il Q-Q plot dei quantili

# quantile temporale corrispondente ad una S(t) del 70% nei due gruppi:
percs = (1:99)/100
qWeib = predict(f4, newdata=list(voltage_ref=12), type="quantile", p=percs, se=TRUE)
csurv =survfit(Surv(days,event)~1, data=cap, subset=(voltage==32))
qKM =quantile(csurv,probs=percs, conf.int = FALSE)
plot(qWeib$fit,qKM)
abline(a=0,b=1,col=2)



# DATI SPAZIALI----

install.packages('geoRglm')
library(geoR)      
library(geoRglm)

dati.geo <- read.geodata("C:/Users/giaco/Desktop/uni/modelli stat. applicati/progetto/conflict10.csv",
                         coords.col = 8:9, data.col = 1, header = T, sep = ',')
summary(dati.geo)
plot(dati.geo)

with(dati.geo, hist(dati.geo$data, main = "", xlab = "dati")) 

########### variogramma empirico  #################
vc <- variog(dati.geo,option="cloud")  # variogramma a nuvola
plot(vc)

par(mfrow = c(1,2))
########### variogramma campionario  #################
varele = variog(dati.geo, uvec = seq(0, 6, by = 0.5), option="bin")
plot(variog(dati.geo, uvec = seq(0, 6, by = 0.5), option="bin"),type = "b")
plot(variog(dati.geo, uvec = seq(0, 5, by = 0.5)),type = "b")
plot(variog(dati.geo))

res1.v <- variog(dati.geo, trend = "1st", uvec = seq(0, 5, by = 0.5))
plot(res1.v, type = "b")
res2.v <- variog(dati.geo, trend = "2nd", uvec = seq(0, 5, by = 0.5))
lines(res2.v, type = "b", lty = 2)

res1.v$u
res1.v$v
plot(res1.v$u,res1.v$v, type="l")
points(res1.v$u,res1.v$v)
res1.v$n  # numero coppie in ciascun sottointervallo di distanza 
round(res1.v$bins.lim,3)
res1.v$beta.ols  # nel trend, stime dei beta con metodo minimi quadrati ordinari
st1 <- lm(dati.geo$data~ dati.geo$coords[,1] + dati.geo$coords[,2])
summary(st1)
st2 <- lm(dati.geo$data~ dati.geo$coords[,1] + dati.geo$coords[,2] +
            I(dati.geo$coords[,1]^2) + I(dati.geo$coords[,2]^2) )
summary(st2) # molto meglio il modello lineare.

####  proviamo a cambiare l'ampiezza dei bin nel variogramma
plot(variog(dati.geo), type="b")
