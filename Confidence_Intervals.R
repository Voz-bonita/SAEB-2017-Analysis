# Title     : Confidence Intervals
# Objective : Calculate Confidence Intervals
# Created by: Jose Vitor
# Created on: 13/03/2021

library(ggplot2)

population <- read.csv("SAEB.sample.csv")

CI <- function (data, confidence = 0.95, sd){

  data <- as.data.frame(data)
  n <- length(data[,1])
  sample_mean <- mean(data[,1])

  sup_quant <- (1-confidence)/2 + confidence
  error <- qnorm(sup_quant) * sd/sqrt(n)

  inf_lim <- sample_mean - error
  sup_lim <- sample_mean + error

  returnValue(c(inf_lim,sup_lim))
}

n1 <- 30
max_samples <- 50

samples_30 <- list()

set.seed(1234)

for (i in 1:max_samples){
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



math_df <- lp_df <- data.frame(matrix(ncol = 3, nrow = max_samples))
math_df[,1] <- lp_df[,1] <- seq(1,max_samples)


category <- list()
category[["NOTA_MT"]] <- list(math_df, mean_mt, var_mt, sd_mt)
category[["NOTA_LP"]] <- list(lp_df, mean_lp, var_lp, sd_lp)

# Calculating category
for (name in names(category)){
  names(category[[name]]) <- c("Interval_df", "Mean", "Var", "SD")

  for (i in 1:max_samples){
    Interval_i <- CI(samples_30[[i]][name], sd = category[[name]]$SD)
    category[[name]]$Interval_df[i,2:3] <- Interval_i
  }

  names(category[[name]]$Interval_df) <- c("Index", "Inf_lim", "Sup_lim")
}


ggplot(data = category[[name]]$Interval_df, aes(x = category[[name]]$Mean,y = `Index`)) +
  geom_vline(xintercept=category[[name]]$Mean, linetype="dashed") +
  geom_errorbar(xmin = category[[name]]$Interval_df$Inf_lim, xmax = category[[name]]$Interval_df$Sup_lim) +
  xlim(min(category[[name]]$Interval_df$Inf_lim)-5, max(category[[name]]$Interval_df$Sup_lim)+5)
