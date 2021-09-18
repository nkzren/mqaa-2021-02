# Carrega o dataset
df <- read.table("atividade2/dataset_PT.csv", header = TRUE, sep = ';', stringsAsFactors = FALSE)

split <- df[, 1:10]
equips <- list(split$Equipamentos_para_Aulas_Online)

equips_col <- split[, 7]

global_mean <- mean(equips_col)
global_sd <- sd(equips_col)

# equip_mean <- aggregate(equips_col, equips, mean)
# equip_sd <- aggregate(equips_col, equips, sd)

y <- dnorm(equips_col, global_mean, global_sd)

# plot do grafico para verificar se a coluna desejada tem distribuicao normal
plot(equips_col, y)

anova <- aov(Tempo_em_estudo_individual ~ Equipamentos_para_Aulas_Online, data = df)

print(summary(anova))

