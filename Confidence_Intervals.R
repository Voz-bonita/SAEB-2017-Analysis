# Title     : Confidence Intervals
# Objective : Calculate Confidence Intervals
# Created by: Jose Vitor
# Created on: 13/03/2021

population <- read.csv("SAEB.sample.csv")

n1 <- 30

samples_30 <- list()
for (i in 1:50){
  sample_i <- population[sample(nrow(population), size = n1, replace = FALSE),]
  samples_30[[i]] <- sample_i
}

rm(`sample_i`)
rm(`i`)


mean_lp <- mean(population$NOTA_LP)
var_lp <- var(population$NOTA_LP)
sd_lp <- sqrt(var_lp)

mean_mt <- mean(population$NOTA_MT)
var_mt <- var(population$NOTA_MT)
sd_mt <- sqrt(var_mt)

quantile_sup <- qnorm(0.975)

CI <- function (data, confidence = 0.95, sd){

  n <- length(data)
  sample_mean <- mean(data)

  sup_quant <- (1-confidence)/2 + confidence
  error <- qnorm(sup_quant) * sd/sqrt(n)

  inf_lim <- sample_mean - error
  sup_lim <- sample_mean + error

  returnValue(c(inf_lim,sup_lim))
}

