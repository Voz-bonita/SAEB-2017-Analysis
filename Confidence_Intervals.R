# Title     : Confidence Intervals
# Objective : Calculate Confidence Intervals
# Created by: Jose Vitor
# Created on: 13/03/2021

population <- read.csv("SAEB.sample.csv")

CI <- function (data, confidence = 0.95, sd){

  n <- length(data)
  sample_mean <- mean(data)

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

quantile_sup <- qnorm(0.975)


intervals <- list()
math_df <- lp_df <- data.frame(matrix(ncol = 3, nrow = max_samples))
math_df[,1] <- seq(1,max_samples)
for (i in 1:max_samples){

  Interval_i <- CI(samples_30[[i]]$NOTA_MT, sd = sd_mt)
  math_df[i,2:3] <- Interval_i

}

names(math_df) <- c("Index", "Inf_lim", "Sup_lim")

ggplot(data = math_df, aes(x = `mean_mt`,y = `Index`)) +
  geom_vline(xintercept=`mean_mt`, linetype="dashed") +
  geom_errorbar(xmin = math_df$Inf_lim, xmax = math_df$Sup_lim) +
  xlim(min(math_df$Inf_lim)-5, max(math_df$Sup_lim)+5)
