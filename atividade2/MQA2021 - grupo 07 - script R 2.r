######################## Regress??o Linear Simples #########################


# Passo 1: Carregar os pacotes que ser??o usados

if(!require(pacman)) install.packages("pacman")
library(pacman)

pacman::p_load(dplyr, ggplot2, car, rstatix, lmtest, ggpubr)


# Passo 2: Carregar o banco de dados

# Importante: selecionar o diret??rio de trabalho (working directory)
# Isso pode ser feito manualmente: Session > Set Working Directory > Choose Directory

dados <- read.table("MQA2021 - grupo 07 - dataset.csv", header = TRUE, sep = ';', stringsAsFactors = FALSE) # Carregamento do arquivo csv
View(dados)                                 # Visualiza????o dos dados em janela separada
glimpse(dados)                              # Visualiza????o de um resumo dos dados



# Passo 3: Verifica????o dos pressupostos para a regress??o linear


## Rela????o linear entre a VD e a VI:
### VD: Vendas
### VI: Publicidade

plot(dados$Tempo_em_exercicios_fisicos, dados$Tempo_de_sono)


## Constru????o do modelo:
mod <- lm(Tempo_de_sono ~ Tempo_em_exercicios_fisicos, dados)


## An??lise gr??fica:

par(mfrow=c(2,2))

plot(mod)

### Interpreta????o: https://data.library.virginia.edu/diagnostic-plots/

par(mfrow=c(1,1))


## Normalidade dos res??duos:
shapiro.test(mod$residuals)


## Outliers nos res??duos:
summary(rstandard(mod))


## Independ??ncia dos res??duos (Durbin-Watson):
durbinWatsonTest(mod)


## Homocedasticidade (Breusch-Pagan):
bptest(mod)


# Passo 4: An??lise do modelo
summary(mod)





ggplot(data = dados, mapping = aes(x = Publicidade, y = Vendas)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  stat_regline_equation(aes(label = paste(..eq.label.., ..adj.rr.label..,
                                          sep = "*plain(\",\")~~")),
                        label.x = 0, label.y = 400) +
  theme_classic()


# https://pt.stackoverflow.com/questions/6979/como-colocar-a-equa%C3%A7%C3%A3o-da-regress%C3%A3o-em-um-gr%C3%A1fico
