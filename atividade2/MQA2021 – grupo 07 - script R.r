# Carrega o dataset
df <- read.table("MQA2021 - grupo 07 - dataset.csv", header = TRUE, sep = ';', stringsAsFactors = FALSE)

split <- df[, 1:10]
equips <- list(split$Equipamentos_para_Aulas_Online)

# Variavel dependente -> tempo de estudo individual
dependent <- split[, 4]

global_mean <- mean(dependent)
global_sd <- sd(dependent)

equip_mean <- aggregate(dependent, equips, mean)
equip_sd <- aggregate(dependent, equips, sd)

boxplot(dependent~split$Equipamentos_para_Aulas_Online)

y <- dnorm(dependent, global_mean, global_sd)

# plot do grafico para verificar se a coluna desejada tem distribuicao normal
plot(dependent, y)

anova <- aov(Tempo_em_estudo_individual ~ Equipamentos_para_Aulas_Online, data = df)

plot(anova)

print(summary(anova))
