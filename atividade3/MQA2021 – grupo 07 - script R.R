if(!require(pacman)) install.packages("pacman")
library(pacman)

if(!require(nortest)) install.packages("nortest")
library(nortest)

if(!require(dgof)) install.packages("dgof")
library(dgof)

pacman::p_load(dplyr, car, rstatix, lmtest, ggpubr,
               QuantPsyc, psych, scatterplot3d)

setwd("C:/Users/Tapps/mqaa-2021-02/atividade2")
rawData <- read.table("clima.csv", header = TRUE, sep = ';', stringsAsFactors = FALSE)#read.csv("clima.csv")

dados <- filter(rawData, Month == "12")
glimpse(dados)
View(dados)

rls1 <- lm(Apparent.Temperature..C. ~ Humidity, dados)

summary(rls1)

rls2 <- lm(Apparent.Temperature..C. ~ Wind.Speed..km.h., dados)

summary(rls2)

rls3 <- lm(Apparent.Temperature..C. ~ Temperature..C., dados)

summary(rls3)

rls4 <- lm(Apparent.Temperature..C. ~ Visibility..km., dados)

summary(rls4)

rls5 <- lm(Apparent.Temperature..C. ~ Pressure..millibars., dados)

summary(rls5)

rlm <- lm(Apparent.Temperature..C. ~ Visibility..km. + Humidity, dados)

summary(rlm)

par(mfrow=c(2,2))
plot(rlm)

par(mfrow=c(1,1))

#shapiro.test(rlm$residuals)

ad.test(rlm$residuals)
ks.test(rlm$residuals,"pnorm",mean(rlm$residuals),sd(rlm$residuals))

bptest(rlm)

avPlots(rlm)
