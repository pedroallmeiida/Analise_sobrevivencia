### Bibliotecas
library(survival)
library(dplyr)
library(magrittr)

### Dataset
dados = read.csv( 'C:\\Users\\pedro\\Dropbox\\disciplinas_UEPB\\Analise_sobrevivencia\\Exemplos_R\\dialise.csv' )
View(dados)
names(dados)

dados$unidade <- as.factor(dados$unidade)


table( dados$cdiab )

glimpse(dados)


## ajuste kaplan-Meier
mod = survfit( Surv( tempo, status   )~1, data = dados  )
mod

plot(mod)

### Ajuste dos modelos parametricos
ajuste_ex <-survreg(Surv(tempo, status)~idade + grande  , data = dados,  dist="exponential")
summary(ajuste_ex)

ajuste_w<-survreg(Surv(tempo, status)~idade + grande  , data = dados,  dist="weibull")
summary(ajuste_w)

ajuste_ln<-survreg(Surv(tempo, status)~idade + grande  , data = dados,  dist="lognormal")
summary(ajuste_ln)

### Exponencial 
time = dados$tempo
xB = -(ajuste_ex$coefficients[1] + ajuste_ex$coefficients[2]*dados$idade + ajuste_ex$coefficients[3]*dados$grande)
res = time*exp(xB)
ekm <- survfit( Surv( res, dados$status )~1 )
res = sort( res )
ste = exp( -res )

par(mfrow=c(1,2))
plot( ekm , conf.int = F)
lines(res, ste, lty = 3  )
legend( 1, 0.9, lty = c(1,3), c( 'Kaplan-Meier', 'Exponencial'  ), lwd = 1, bty = 'n', cex = 0.8  )        

### Weibull
time = dados$tempo
gama = 1/ajuste_w$scale
xB = exp( -(ajuste_w$coefficients[1] + ajuste_w$coefficients[2]*dados$idade + ajuste_w$coefficients[3]*dados$grande)  )
res = (time*xB)^gama
ekm <- survfit( Surv( res, dados$status )~1 )
res = sort(res)
stw = exp( - (res)^gama )

plot( ekm , conf.int = F)
lines(res, stw, lty = 3  )
legend( 1.5, 0.9, lty = c(1,3), c( 'Kaplan-Meier', 'Weibull'  ), lwd = 1, bty = 'n', cex = 0.8  )      

### Analise de residuo 

plot(residuals(ajuste_w, type = 'deviance'), ylim = c(-3,3))


##### usando o pacote rms


library(survival)
library(rms)

ajusteE <- psm( Surv(tempo, status)~idade + grande  , data = dados,dist="exponential")
residE <- residuals(ajusteE)
ajusteW <- psm(Surv(tempo, status)~idade + grande  , data = dados, dist="weibull")
residW <- residuals(ajusteW)
ajusteLN <- psm(Surv(tempo, status)~idade + grande  , data = dados,dist="lognormal")
residLN <- residuals(ajusteLN)



### Escolha da melhor distribuição
par(mfrow=c(1,3))
survplot(residE,main="Exponential",ylab="Complement of residual CDF")
survplot(residW,main="Weibull",ylab="Complement of residual CDF")
survplot(residLN,main="Lognormal",ylab="Complement of residual CDF")

### residuos deviance
residE_dev =  residuals(ajusteE, type = 'deviance')
residW_dev =  residuals(ajusteW, type = 'deviance')
residLN_dev =  residuals(ajusteLN, type = 'deviance')



par(mfrow=c(1,3))
plot( residE_dev, ylim = c(-3,3) )
plot( residW_dev, ylim = c(-3,3) )
plot( residLN_dev, ylim = c(-3,3) )


### residuos ldresp: pontos influentes sobre os valores preditos
residE_ldresp =  residuals(ajusteE, type = 'ldresp')
residW_ldresp =  residuals(ajusteW, type = 'ldresp')
residLN_ldresp =  residuals(ajusteLN, type = 'ldresp')



par(mfrow=c(1,3))
plot( residE_ldresp )
plot( residW_ldresp )
plot( residLN_ldresp )


### residuos ldcase: pontos influentes sobre os parâmetros
residE_ldcase =  residuals(ajusteE, type = 'ldcase')
residW_ldcase =  residuals(ajusteW, type = 'ldcase')
residLN_ldcase =  residuals(ajusteLN, type = 'ldcase')



par(mfrow=c(1,3))
plot( residE_ldcase )
plot( residW_ldcase )
plot( residLN_ldcase )




#### Modelo proporcional de cox 

library(ggfortify)
library(survminer)

mod_cox = coxph( Surv(tempo, status)~idade + grande, data = dados  )
summary(mod_cox)


test.ph = cox.zph(mod_cox)
test.ph

### testando proporcionalidade
ggcoxzph(test.ph)
ggcoxdiagnostics( mod_cox, type = 'deviance', linear.predictions = F )
