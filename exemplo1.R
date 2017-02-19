# Pacotes -----------------------------------------------------------------
library(readxl)
library(dplyr)
library(ggplot2)

# Ler os dados ------------------------------------------------------------
download.file('https://github.com/curso-r/pu.modelos/blob/master/data/BodyFat.xls?raw=true', 'aula5/bodyfat.xls')
bodyfat <- read_excel('aula5/bodyfat.xls')

View(bodyfat)
# Análise -----------------------------------------------------------------
ggplot(bodyfat, aes(x = WEIGHT, y = BODYFAT)) + geom_point()

#lm para ajustar funcao linear
ajuste <- lm(BODYFAT ~ WEIGHT, data = bodyfat)
summary(ajuste)

#(Intercept) -9.99515    2.38906 seria alpha 
#WEIGHT       0.16171    0.01318 seria beta

str(ajuste, max.level = 1)
bodyfat$predito <- predict(ajuste, newdata = bodyfat)

#erro quadratico numero -> para saber o quanto de erro eu tenho
mse <- mean((bodyfat$BODYFAT - bodyfat$predito)^2)

#melhor "chute"
erro_usando_media <- mean((bodyfat$BODYFAT - mean(bodyfat$BODYFAT))^2)
View(erro_usando_media)


#se fosse ajustar o modelo
ajuste <- lm(BODYFAT ~ WEIGHT + HEIGHT, data = bodyfat)






# separar modelo ----------------------------------------------------------


##=========================================================##

#separar a base em treino e teste
#ctrl shift R
# TESTE -------------------------------------------------------------------
#gerando vetor de teste com quantidade de 75%
id_treino <- sample(1:nrow(bodyfat), 0.75*nrow(bodyfat))

treino <- bodyfat[id_treino,]
teste <- bodyfat[-id_treino,] 

ajuste <- lm(BODYFAT ~ HEIGHT + WEIGHT +ABDOMEN  , data = bodyfat)
summary(ajuste)
teste$predito <- predict(ajuste, newdata = teste)


calcular_erro <- function(banco, modelo) {
  banco$predito <- predict(ajuste, newdata = banco)
  mse <-  mean((banco$BODYFAT - banco$predito)^2)
  return (mse)
}

calcular_erro(teste, ajuste)
calcular_erro(treino, ajuste)
calcular_erro(bodyfat, ajuste)


#BEST CASE
https://rstudio-pubs-static.s3.amazonaws.com/65314_c0d1e5696cdd4e93a3784ea67f9e3d34.html

#===
library(dplyr)
bodyfat2 <- mutate(bodyfat, MassIndex = Weight/(Height/100)^2)
