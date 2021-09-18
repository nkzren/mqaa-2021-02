df <- read.table("atividade2/dataset_PT.csv", header = TRUE, sep = ';', stringsAsFactors = FALSE)

split <- df[1:100, 1:10]
anova <- aov(Tempo_em_estudo_individual ~ Equipamentos_para_Aulas_Online, data = df)

summary(anova)
# print(aggregate(split[, 4], list(split$Equipamentos_para_Aulas_Online), mean))
# print(list(df$Regiao_de_Residencia))
# print(df[1:10, 2:6]$Regiao_de_Residencia)


