install.packages("pacman")

library(pacman)

pacman::p_load(dplyr, car, rstatix, lmtest, ggpubr,
               QuantPsyc, psych, scatterplot3d)

dados <- read.table("MQA2021 - grupo 07 - dataset.csv", header = TRUE, sep = ';', stringsAsFactors = FALSE) # Carregamento do arquivo csv


mod <- lm(Tempo_de_sono ~ Tempo_de_Aulas_Online + Tempo_em_estudo_individual, dados)

par(mfrow=c(2,2))

plot(mod)


# Testes de pressupostos
shapiro.test(mod$residuals)

summary(rstandard(mod))

bptest(mod)

vif(mod)


#analise do modelo
summary(mod)

#grafico de dispersao
graph <- scatterplot3d(dados$Tempo_de_sono ~ dados$Tempo_de_Aulas_Online + dados$Tempo_em_estudo_individual,
                       pch = 16, angle = 50, color = "red", box = FALSE,
                       xlab="Tempo em Aulas Online", ylab="Tempo em Estudo", zlab="Tempo de Sono")
graph$plane3d(mod, col="black", draw_polygon = TRUE)

