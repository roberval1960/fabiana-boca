##Ajuste de modelos carbono castanheira
dados<-read.table("C:\\Users\\Karen\\Documents\\Artigos_preparação\\Manuscrito_Carbono_CdA\\Laboratorio\\Static\\dados_total.txt",header=T,dec=",")
dados
attach(dados)

#Equação1
x1<-dap
reg_eq01<-lm(ms~1+x1)
summary(reg_eq01)
library(car)
vif(reg_eq01)
plot(fitted(reg_eq01),resid(reg_eq01)*100,pch=1,ylab="Resíduos (%)",xlab=" Biomassa estimada (kg)")
abline(h=0,col="black",lty=5)
title("Modelo 1")
data.frame(fitted(reg_eq01))##Bio estimada pelo modelo


##Equação2
x1<-ht
reg_eq02<-lm(ms~1+x1)
summary(reg_eq02)
library(car)
vif(reg_eq02)
plot(fitted(reg_eq02),resid(reg_eq02)*100,pch=1,ylab="Resíduos (%)",xlab=" Biomassa estimada (kg)")
abline(h=0,col="black",lty=5)
title("Modelo 2")
data.frame(fitted(reg_eq02))

##Equação3
x1<-dap
x2<-ht
reg_eq03<-lm(ms~1+x1+x2)
summary(reg_eq03)
library(car)
vif(reg_eq03)
plot(fitted(reg_eq03),resid(reg_eq03)*100,pch=1,ylab="Resíduos (%)",xlab=" Biomassa estimada (kg)")
abline(h=0,col="black",lty=5)
title("Modelo 3")
data.frame(fitted(reg_eq03))


##Equação4
x1<-dap^2*ht
reg_eq04<-lm(ms~1+x1)
summary(reg_eq04)
library(car)
vif(reg_eq04)
plot(fitted(reg_eq04),resid(reg_eq04)*100,pch=1,ylab="Resíduos (%)",xlab=" Biomassa estimada (kg)")
abline(h=0,col="black",lty=5)
title("Modelo 4")
data.frame(fitted(reg_eq04))

##Equação5
x1<-dap
x2<-dap^2*ht
reg_eq05<-lm(ms~1+x1+x2)
summary(reg_eq05)
library(car)
vif(reg_eq05)
plot(fitted(reg_eq05),resid(reg_eq05)*100,pch=1,ylab="Resíduos (%)",xlab=" Biomassa estimada (kg)")
abline(h=0,col="black",lty=5)
title("Modelo 5")
data.frame(fitted(reg_eq05))

##Equação6
x1<-dap^2
x2<-dap^2*ht
reg_eq06<-lm(ms~1+x1+x2)
summary(reg_eq06)
library(car)
vif(reg_eq06)
plot(fitted(reg_eq06),resid(reg_eq06),pch=1,ylab="Resíduos (%)",xlab=" Biomassa estimada (kg)")
abline(h=0,col="black",lty=5)
title("Modelo 6")
data.frame(fitted(reg_eq06))
resid(reg_eq06)

##Equação7
x1<-dap^3
reg_eq07<-lm(ms~1+x1)
summary(reg_eq07)
library(car)
vif(reg_eq07)
plot(fitted(reg_eq07),resid(reg_eq07),pch=1,ylab="Resíduos (%)",xlab=" Biomassa estimada (kg)",ylim=)
abline(h=0,col="black",lty=5)
title("Modelo 6")
data.frame(fitted(reg_eq07))


##Equação8
x1<-dap
x2<-dap^2
x3<-dap^2*ht
reg_eq08<-lm(log(ms)~1+x1+x2)
summary(reg_eq08)
library(car)
vif(reg_eq08)
plot(fitted(reg_eq08),resid(reg_eq08)*100,pch=1,ylab="Resíduos (%)",xlab=" Biomassa estimada (kg)")
abline(h=0,col="black",lty=5)
data.frame(fitted(reg_eq08))

##Equação9
x1<-dap^2
x2<-dap^2*ht
x3<-dap*ht^2
x4<-ht^2
reg_eq09<-lm(ms~1+x1+x2+x3+x4)
summary(reg_eq09)
library(car)
vif(reg_eq09)
plot(fitted(reg_eq09),resid(reg_eq09)*100,pch=1,ylab="Resíduos (%)",xlab=" Biomassa estimada (kg)")
abline(h=0,col="black",lty=5)
data.frame(fitted(reg_eq09))

##Equação10
x1<-dap
x2<-dap^2
x3<-ht
x4<-dap^2*ht
reg_eq10<-lm(ms~1+x1+x2+x3+x4)
summary(reg_eq10)
library(car)
vif(reg_eq10)
plot(fitted(reg_eq10),resid(reg_eq10)*100,pch=1,ylab="Resíduos (%)",xlab=" Biomassa estimada (kg)")
abline(h=0,col="black",lty=5)
data.frame(fitted(reg_eq10))

##Equação11
x1<-dap^2
x2<-ht^2
x3<-dap^2*ht
reg_eq11<-lm(ms~1+x1+x2+x3)
summary(reg_eq11)
library(car)
vif(reg_eq11)
plot(fitted(reg_eq11),resid(reg_eq11)*100,pch=1,ylab="Resíduos (%)",xlab=" Biomassa estimada (kg)")
abline(h=0,col="black",lty=5)
data.frame(fitted(reg_eq11))

##Equação12
x1<-dap
x2<-dap^2
x3<-dap^2*ht
reg_eq12<-lm(ms~1+x1+x2+x3)
summary(reg_eq12)
library(car)
vif(reg_eq12)
plot(fitted(reg_eq12),resid(reg_eq12)*100,pch=1,ylab="Resíduos (%)",xlab=" Biomassa estimada (kg)")
abline(h=0,col="black",lty=5)
data.frame(fitted(reg_eq12))

##Equação13
x1<-log(dap)
reg_eq13<-lm(log(ms)~1+x1)
summary(reg_eq13)
library(car)
vif(reg_eq13)
plot(fitted(reg_eq13),resid(reg_eq13)*100,pch=1,ylab="Resíduos (%)",xlab=" Biomassa estimada (kg)")
abline(h=0,col="black",lty=5)
data.frame(fitted(reg_eq13))

##Equação14
x1<-log(dap^2*ht)
reg_eq14<-lm(log(ms)~1+x1)
summary(reg_eq14)
library(car)
vif(reg_eq14)
plot(fitted(reg_eq14),resid(reg_eq14)*100,pch=1,ylab="Resíduos (%)",xlab=" Biomassa estimada (kg)")
abline(h=0,col="black",lty=5)
data.frame(fitted(reg_eq14))


##Equação15
x1<-log(dap)
x2<-log(ht)
reg_eq15<-lm(log(ms)~1+x1+x2)
summary(reg_eq15)
library(car)
vif(reg_eq15)
plot(fitted(reg_eq15),resid(reg_eq15)*100,pch=1,ylab="Resíduos (%)",xlab=" Biomassa estimada (kg)")
abline(h=0,col="black",lty=5)
data.frame(fitted(reg_eq15))

