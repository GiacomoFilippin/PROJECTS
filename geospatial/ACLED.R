#ACLED
dati <- read.csv("C:/Users/giaco/Desktop/uni/modelli stat. applicati/progetto/conflict10.csv")
View(dati)
weeklydata <- read.delim("C:/Users/giaco/Desktop/uni/modelli stat. applicati/progetto/weeklydata.txt")
View(weeklydata)
#weeklydata
#conflict10
attach(dati)
library(survival)
source("C:/Users/giaco/Desktop/uni/modelli stat. applicati/lab/Lab-R-2/ConvertExpWeibull.R")


### ANALISI ESPLORATIVA ###
View(dati)
#controllo le dimensioni del dataset
dim(dati)
#tipologia di variabile
str(dati)
#statistiche riassuntive delle variabili
summary(dati)

#Distribuzioni bivariate - relazioni tra variabili
boxplot(dati$Year ~ dati$EventType, 
        col= c('blue', 'pink', 'yellow'), main= 'boxplot')
legend("topright", legend=c('Battles', 'Violence against civilians',
                            'Riots'), col=c("blue","pink", 'yellow'), pch=19)
table(dati$Year, dati$EventType)

# Per vedere se ci sono variabili correlate tra loro potrebbe essere utile visualizzare i diagrammi
# a dispersione a coppia:
pairs(dati[,-c(2:7, 13, 16)])

#prova a fare ggpairs sotto ggplot2

### ANALISI DI SOPRAVVIVENZA ?? ###
status=rep(1, length(dati[,1]))
Shat <- survfit(Surv(Year, status) ~ 1 + EventType,data=dati)
summary(Shat)

#graficamente
plot(Shat, conf.int=T, mark.time=TRUE,xlab="tempo (anni)", ylab="Shat(t)", xlim= c(1995,2012), col = 1:3)

####################################################################################

#################  modello di regressione Weibull  A TEMPI ACCELERATI  #################
## il modello log-lineare è:  Y= \mu + \beta1 * voltage + \sigma W
f1<-survreg(Surv(Year, dati$status) ~ TempCat, data=dati)
summary(f1)
# calcoliamo il fattore di accelerazione

#################  modello di regressione Weibull  A RISCHI MOLTIPLICATIVI   #################
f1_prop = ConvertExpWeibull(f1)
f1_prop
lambda <- f1_prop$vars[1,1]
alpha <- f1_prop$vars[2,1]
gamma1 <- f1_prop$vars[3,1]

#######  modello di regressione esponenziale  A TEMPI ACCELERATI ###############

# trattiamo voltage come una variabile quantitativa continua
f2<-survreg(Surv(Year, dati$status) ~ TempCat, scale=1)
summary(f2)

# calcoliamo il fattore di accelerazione
fa <- exp(f2$coefficients)   

#######  modello di regressione esponenziale A RISCHI MOLTIPLICATIVI ###############

# vediamo la rappresentazione a rischi moltiplicativi a partire dal modello AFT
f2_prop = ConvertExpWeibull(f2, scale=1)
f2_prop
lambda <- f2_prop$vars[1,1]
gamma1 <- f2_prop$vars[3,1]

HR = exp(gamma1)  # interpretazione!

anova(f1, f2)
anova(f2_prop)
########  MODELLO SEMI-PARAMETRICO DI COX
f1 <- coxph(Surv(Year, status) ~ EventType, ties="breslow",data=dati)
summary(f1)  # si
# Akaike information criterion: AIC = -2 log L + 2* p

AIC1 <- -2*f1$loglik +2*c(0,1)
AIC1
lrt1 <- 2*(f1$loglik[2] - f1$loglik[1])
lrt1

dati$status = rep(0, length(dati$Year))
dati[dati$EventType=='Riots',]$status = 1
f1 <- coxph(Surv(Year, dati$status)~ TempCat, ties="breslow",data=dati)
f2 <- coxph(Surv(Year, dati$status)~ TempCat + MaxTemp, ties="breslow",data=dati)
summary(f2)  # si
AIC2 <- -2*f2$loglik +2*c(0,2)
AIC2

lrt1 <- 2*(f1$loglik[2] - f1$loglik[1])

lrt2 <- 2*(f2$loglik[2] - f2$loglik[1])
lrt2
## local test per beta_2 =0
lrt2 - lrt1   #  20.3568
1 -pchisq( 20.3568,df=1)

dati$status = rep(0, length(dati$Year))
dati[dati$EventType=='Violence against civilians',]$status = 1
dati$status = rep(0, length(dati$Year))
dati[dati$EventType=='Battles',]$status = 1
dati$status = rep(0, length(dati$Year))
dati[dati$EventType=='Riots',]$status = 1
library(survival)
library(MASS)
library(tidyverse)
ff <- coxph(Surv(Year, dati$status) ~ TempCat + MaxTemp, ties="breslow",data=dati)
dropterm(ff,test = "Chisq")
mres <- resid(ff, type="martingale")
csres <- dati$status-mres
r.surv <- survfit(Surv(csres,status) ~1, stype=2,data=dati)
plot(r.surv$time, -log(r.surv$surv), type="s", xlab="Cox-snell Residual",
     ylab="Estimated Cum Hazards")
abline(a=0, b=1, col=2)


rm(list = ls())
############# GEOSTATISTICA: analisi esplorativa dei dati   ##################
library(geoR)     
dati <- read.csv("C:/Users/giaco/Desktop/uni/modelli stat. applicati/progetto/conflict10.csv")
datiGeo <- read.geodata("C:/Users/giaco/Desktop/uni/modelli stat. applicati/progetto/conflict10.csv", 
                      header = T, coords.col = 8:9, data.col = 1, sep = ',')

# Identifica le coordinate duplicate
duplicates <- dup.coords(datiGeo$coords)
oks = rep(NA, length(duplicates))
for (i in seq(1:length(duplicates))) {
  oks[i] = duplicates[[i]][1]
}
oks
datiG = as.geodata(dati[oks,], coords.col = 8:9, data.col = 1, covar.col = 2, covar.names = 'EvenType')

points(datiG, cex.min = 0.5, cex.max = 1)
#da una prima analisi si evince che le guerre stanno un po' calando nel tempo
#sembrano molto più concentrate nel sud piuttosto che nel nord

#informazioni sintetiche sui dati 
summary(datiG)
plot(datiG, lowess=T)

#variogramma empirico  
vc <- variog(datiG,option="cloud")  # variogramma a nuvola
plot(vc)

#variogramma campionario
par(mfrow = c(1,2))

varele = variog(datiG, uvec = seq(0, 10, by = 0.5))
varele2 <- variog(datiG, trend = "1st", uvec = seq(0, 10, by = 0.5))
varele3 <- variog(datiG, trend = "2nd", uvec = seq(0, 10, by = 0.5))
v.fit <- variofit(varele, ini = c(15,2), cov.model = "matern", fix.nugget = TRUE)
v.fit2 <- variofit(varele2, ini = c(15,2), cov.model = "matern", fix.nugget = TRUE)
v.fit3 <- variofit(varele3, ini = c(15,2), cov.model = "matern", fix.nugget = TRUE)

summary(v.fit)
plot(varele)
lines.variomodel(seq(0, 10, l = 1000), cov.pars = v.fit$cov.pars, cov.model = 
                   "matern", nugget = v.fit$nugget, kappa = 2) #molto meglio questa
lines.variomodel(seq(0, 10, l = 1000), cov.pars = v.fit$cov.pars, cov.model = 
                   "exponential", nugget = v.fit$nugget, col = 2) #molto meglio questa
plot(varele2, main = 'trend 1st')
lines.variomodel(seq(0, 10, l = 1000), cov.pars = v.fit2$cov.pars, cov.model = 
                   "matern", nugget = v.fit2$nugget, kappa = 2) #molto meglio questa
lines.variomodel(seq(0, 10, l = 1000), cov.pars = v.fit2$cov.pars, cov.model = 
                   "exponential", nugget = v.fit2$nugget, col = 2) #molto meglio questa

plot(varele3, main = 'trend 2nd')
lines.variomodel(seq(0, 10, l = 1000), cov.pars = v.fit3$cov.pars, cov.model = 
                   "exponential", nugget = v.fit3$nugget, kappa = 2, col = 2) #molto meglio questa
lines.variomodel(seq(0, 10, l = 1000), cov.pars = v.fit3$cov.pars, cov.model = 
                   "matern", nugget = v.fit3$nugget, kappa = 2) #molto meglio questa


#residui
res0.v <- variog(datiG, uvec = seq(0, 5, by = 0.5))
res1.v <- variog(datiG, trend = "1st", uvec = seq(0, 5, by = 0.5))
plot(res1.v, type = "b")
res2.v <- variog(datiG, trend = "2nd", uvec = seq(0, 5, by = 0.5), col = 2)
lines(res2.v, type = "b", lty = 2, col = 2, pch = 19)

#studiamo i residui res1.v 
res1.v$u
res1.v$v
plot(res1.v$u,res1.v$v, type="l")
points(res1.v$u,res1.v$v)
lines(res2.v, type = "b", lty = 2, col = 2, pch = 19)

res1.v$n  # numero coppie in ciascun sottointervallo di distanza 
round(res1.v$bins.lim,3)
res1.v$beta.ols  # nel trend, stime dei beta con metodo minimi quadrati ordinari
st1 <- lm(datiG$data~ datiG$coords[,1] + datiG$coords[,2])
summary(st1)

st2 <- lm(datiG$data~ datiG$coords[,1] + datiG$coords[,2] +
            I(datiG$coords[,1]^2) + I(datiG$coords[,2]^2) )
summary(st2)
par(mfrow = c(2,2))
plot(st1)
plot(st2)


#st2 non buono, pvalue troppo bassi, st1 invece buono

#questo non so cosa sia
v1 <- variog(datiG, trend = "1st", uvec = seq(0,10, by = 0.5))
v0 = variog(datiG, uvec = seq(0,10, by = 0.5))
v2 = variog(datiG, trend = "2nd", uvec = seq(0,10, by = 0.5))
v.fit <- variofit(v1, ini = c(15,2), cov.model = "exponential", fix.nugget = TRUE)
v.fit0 <- variofit(v0, ini = c(15,2), cov.model = "exponential", 
                  fix.nugget = TRUE)
v.fit2 <- variofit(v2, ini = c(15,2), cov.model = "exponential", 
                   fix.nugget = TRUE)
v.fit
v.fit2
v.fit$beta.ols  # stima dell'anno medio (mu)
v.fit$cov.pars   #(sigmasq, phi)
v.fit$nugget   # nugget = tausq = 0
plot(v1)
lines.variomodel(seq(0, 10, l = 1000), cov.pars = v.fit$cov.pars, cov.model = 
                   "exponential", nugget = v.fit$nugget) #molto meglio questa
plot(v0)
lines.variomodel(seq(0, 10, l = 1000), cov.pars = v.fit0$cov.pars, cov.model = 
                   "exponential", nugget = v.fit0$nugget) 
plot(v2)
lines.variomodel(seq(0, 10, l = 1000), cov.pars = v.fit2$cov.pars, cov.model = 
                   "exponential", nugget = v.fit2$nugget) 

# direi meglio exp a occhio




















########### carico i dati   ####################
class(datiG)


#############   previsione spaziale, kriging
plot(datiG)
###### kriging semplice   #######

## usando il  modello geostatistico Gaussiano con media costante  
ml1 <- likfit(datiG, ini = c(15, 2), cov.model = "exponential") 
locs <- pred_grid(c(0, 20), c(20, 40), by = 0.5)
KC <- krige.control(type = "SK", obj.mod = ml1)
sk <- krige.conv(datiG, krige = KC, locations = locs)
sk$pred   # calcio predetta ( That_0)
sk$krige.var    # stima della varianza predetta (varianza var(T_0 | Y))

## grafico della mappa di previsione 
# valori predetti
par(mfrow=c(1,1))
pred.lim <- range(sk$pred)
image(sk, col = gray(seq(1, 0, l = 1000)), zlim = pred.lim, main="kriging estimates")
plot(locs, ty="n", asp=1)
contour(sk, add = T, nlev = 10, col = 2, drawlabels = F)
points(datiG, add = TRUE, cex.max = 2)
points(datiG,  cex.max = 2,borders=datiG$coords, add.to.plot=TRUE)

# standard error delle previsioni
image(sk, val=sqrt(sk$krige.var), col = gray(seq(1, 0, l = 51)), main="kriging std. errors")




###### kriging ordinario   #######

## usando il  modello geostatistico Gaussiano con media costante  
locs <- pred_grid(c(0, 20), c(20, 40), by = 0.5)
KC1 <- krige.control(type = "OK", obj.mod = ml1)
sk1 <- krige.conv(datiG, krige = KC1, locations = locs)
## grafico della mappa di previsione 
# valori predetti
pred.lim <- range(sk1$pred)
plot(locs, ty="n", asp=1)
contour(sk1, add = T, nlev = 6)
points(datiG, add = TRUE, cex.max = 2)
points(datiG,  cex.max = 2,borders=ca20$reg1, add.to.plot=TRUE)
points(datiG,  cex.max = 2,borders=ca20$reg2, add.to.plot=TRUE)
# standard error delle previsioni
image(sk1, val=sqrt(sk1$krige.var), col = gray(seq(1, 0.5, l = 1001)), main="kriging std. errors")








##  usando il modello geostatistico Gaussiano con trend lineare della media  
ml6 <- likfit(datiG, ini = c(15, 2), trend="1st", cov.model = "exponential", kappa = 0.5) 
summary(ml6)
KCtl <- krige.control(type = "OK", trend.d ="1st", trend.l= "1st", obj.mod = ml6)
sktl <- krige.conv(datiG, krige = KCtl, locations = locs)
pred.lim <- range(sktl$pred)
image(sktl, col = gray(seq(1, 0, l = 51)), zlim = pred.lim)





### previsione puntuale in x1=c(5000,5000) con kriging ordinario 
x1=c(10,30)  # x0
summary(datiG)
plot(c(0, 20), c(20, 40), ty="n", asp=1)
w1 =krweights(datiG$coords, x1, KCtl)
points(rbind(datiG$coords,x1))
text(datiG$coords[,1], 1+datiG$coords[,2], round(w1, dig=3))










































