library(survival)
library(dplyr)
library(magrittr)

TEMPO = c( 3,18,29,54,60,84,110,112, 116,123,134,145,151,151,158,173,194,214,329,331,371,408,490,514,541,555,688,780,801,858,887,998  )
STATUS  = rep( 1, length(TEMPO) )

bd = data.frame(TEMPO, STATUS)
surv = Surv( TEMPO, STATUS   )
mod = survfit( Surv( TEMPO, STATUS   )~1, bd  )
summary(mod)

plot(mod)



### EXEMPLO 2 

### Criando o dataset
Grupos = c( rep( "Controle", 15 ), rep( "Esteroide", 14 )  )
Tempo = c( 1,2,3,3,3,5,5,16, 16,16,16,16,16,16,16,1,1,1,1,4,5,7,8,10,10,12,16,16,16  )
Censura = c( 0,0,1,1,0,0,0, rep(1, 11), 0,0,1,1,1,1,0,0,1,1,1  )

dados = data.frame( Grupos, Tempo, Censura )
View(dados)

## Selecionando apenas dados sem censura

dados <- dados %>% filter( Censura == 1  )

mod = survfit( Surv( Tempo, Censura   )~Grupos, dados  )
mod

dados %>% group_by( Grupos ) %>% summarise( mediana = median(Tempo), media = mean(Tempo) )

summary(mod)

plot(mod, xlab = 'Tempo', ylab = 'S(t)', lty = c(1,2))
legend( x = 1, y = 0.2, legend = c('Controle', 'Esteroide'), lty = c(1,2) )



