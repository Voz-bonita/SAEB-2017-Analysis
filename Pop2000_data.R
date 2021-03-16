# Title     : Population data
# Objective : Separate "population" data from sample data
# Created by: Jose Vitor
# Created on: 16/03/2021

library(dplyr)

population <- read.csv("SAEB.sample.csv")
n.true <- length(population[,1])

# Sexo (Gender) and Localização (Localization) are both categorical data
# This data need to be summarized

population$SEXO <- recode(population$SEXO, "A" = "Male", "B" = "Female")

gender <- population %>%
  group_by(SEXO) %>%
  count()
gender.prop <- gender$n[which(gender$SEXO == "Female")]/2000
gender.var <- gender.prop*(1-gender.prop)/n.true
gender.sd <- sqrt(gender.var)


population$LOCALIZACAO <- recode(population$LOCALIZACAO, "1" = "Urban", "2" = "Rural")

local <- population %>%
  group_by(LOCALIZACAO) %>%
  count()
local.prop <- local$n[which(local$LOCALIZACAO == "Rural")]/2000
local.var <- local.prop*(1-local.prop)/n.true
local.sd <- sqrt(local.var)

# LP stands for Língua Portugesa (Portuguese Language)
mean_lp <- mean(population$NOTA_LP)
var_lp <- var(population$NOTA_LP)
sd_lp <- sqrt(var_lp)

# MT stands for Matemática (Math)
mean_mt <- mean(population$NOTA_MT)
var_mt <- var(population$NOTA_MT)
sd_mt <- sqrt(var_mt)






