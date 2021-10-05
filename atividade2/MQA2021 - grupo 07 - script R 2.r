######################### Regress??o Linear M??ltipla #########################

# Passo 1: Carregar os pacotes que ser??o usados

if(!require(pacman)) install.packages("pacman")
library(pacman)

pacman::p_load(dplyr, car, rstatix, lmtest, ggpubr,
               QuantPsyc, psych, scatterplot3d)


# Passo 2: Carregar o banco de dados

# Importante: selecionar o diret??rio de trabalho (working directory)
# Isso pode ser feito manualmente: Session > Set Working Directory > Choose Directory

dados <- read.table("MQA2021 - grupo 07 - dataset.csv", header = TRUE, sep = ';', stringsAsFactors = FALSE) # Carregamento do arquivo csv
View(dados)                                 # Visualiza????o dos dados em janela separada
glimpse(dados)                              # Visualiza????o de um resumo dos dados




## Constru????o do modelo:
mod <- lm(Tempo_de_sono ~ Tempo_de_Aulas_Online + Tempo_em_estudo_individual, dados)


## An??lise gr??fica:
par(mfrow=c(2,2))

plot(mod)

par(mfrow=c(1,1))


## Normalidade dos res??duos:
shapiro.test(mod$residuals)


## Outliers nos res??duos:
summary(rstandard(mod))


## Independ??ncia dos res??duos (Durbin-Watson):
durbinWatsonTest(mod)


## Homocedasticidade (Breusch-Pagan):
bptest(mod)


## Aus??ncia de Multicolinearidade
vif(mod)
### Multicolinearidade: VIF > 10


###### Cria????o de um segundo modelo
mod2 <- lm(Tempo_de_sono ~ Tempo_de_Aulas_Online, dados)


# Passo 4: An??lise do modelo
summary(mod)
summary(mod2)


## Obten????o dos coeficientes padronizados
lm.beta(mod)
lm.beta(mod2)


## Obten????o do IC 95% para os coeficientes
confint(mod)
confint(mod2)


# Compara????o de modelos

## AIC e BIC - Compara????o entre quaisquer modelos
AIC(mod, mod2)
BIC(mod, mod2)


## Para compara????o entre modelos aninhados

anova(mod, mod2)

### O melhor ser?? o com menor valor de RSS (residual sum of squares)



# Passo 5: Gr??fico de dispers??o
graph <- scatterplot3d(dados$Tempo_de_sono ~ dados$Tempo_de_Aulas_Online + dados$Tempo_em_estudo_individual,
                       pch = 16, angle = 70, color = "steelblue", box = FALSE,
                       xlab="Tempo_de_Aulas_Online", ylab="Tempo_em_estudo_individual", zlab="Tempo_de_sono")
graph$plane3d(mod, col="black", draw_polygon = TRUE)



##################### M??todos de sele????o de modelos ###########################

pacman::p_load(MASS)


mod.inicial <- lm(Notas ~ Tempo_Rev + Tempo_Sono, data = dados)
mod.simples <- lm(Notas ~ 1, data = dados)

stepAIC(mod.inicial, scope = list(upper = mod.inicial,
                                  lower = mod.simples), direction = "backward")


# Material consultado: https://rpubs.com/bensonsyd/385183