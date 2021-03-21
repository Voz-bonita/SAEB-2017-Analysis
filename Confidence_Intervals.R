# Title     : Confidence Intervals
# Objective : Calculate Confidence Intervals
# Created by: Jose Vitor
# Created on: 13/03/2021

library(ggplot2)

# population <- read.csv("SAEB.sample.csv")

# Hash that changes variable names into proper variable names when graphing
name_hash <- function(name){
  if (name == "SEXO") {
    returnValue("Proporção de alunos do sexo feminio")
  } else if (name == "LOCALIZACAO") {
    returnValue("Proporção de alunos cuja escola está situada em área rural")
  } else if (name == "NOTA_LP"){
    returnValue("Nota em Língua Portuguesa")
  } else if (name == "NOTA_MT") {
    returnValue("Nota em Matemática")
  } else {
    returnValue(name)
  }
}

CI <- function (data, confidence = 0.95, sd, variable = NULL){

  data <- as.data.frame(data)

  n <- length(data[,1])

  sup_quant <- (1-confidence)/2 + confidence


  if (is.character(data[,1])){
    names(data) <- "Class"
    data <- data %>%
      group_by(Class) %>%
      count()


    sample.prop <- data$n[which(data$Class == variable)]/n
    sd <- sqrt(sample.prop * (1 - sample.prop) / n)
    error <- qnorm(sup_quant) * sd

    inf_lim <- sample.prop - error
    sup_lim <- sample.prop + error

  }else{
    sample_mean <- mean(data[,1])
    error <- qnorm(sup_quant) * sd/sqrt(n)

    inf_lim <- sample_mean - error
    sup_lim <- sample_mean + error


  }
  returnValue(c(inf_lim,sup_lim))
}

n1 <- 30
n2 <- 100
max_samples <- 50

samples_30 <- list()
samples_100 <- list()

set.seed(1234)

for (i in 1:max_samples){
  sample_i <- population[sample(nrow(population), size = n1, replace = FALSE),]
  samples_30[[i]] <- sample_i

  sample_j <- population[sample(nrow(population), size = n2, replace = FALSE),]
  samples_100[[i]] <- sample_j
}

rm(`i`)
rm(`sample_i`)
rm(`sample_j`)


math_df <- lp_df <- local_df <- gender_df <- data.frame(matrix(ncol = 4, nrow = max_samples))
math_df[,1] <- lp_df[,1] <- local_df[,1] <- gender_df[,1] <- seq(1,max_samples)


category_30 <- list()
category_30[["NOTA_MT"]] <- list(math_df, mean_mt, var_mt, sd_mt, "")
category_30[["NOTA_LP"]] <- list(lp_df, mean_lp, var_lp, sd_lp, "")
category_30[["SEXO"]] <- list(gender_df, gender.prop, gender.var, gender.sd, "Female")
category_30[["LOCALIZACAO"]] <- list(local_df, local.prop, local.var, local.sd, "Rural")

category_100 <- list()
category_100[["NOTA_MT"]] <- list(math_df, mean_mt, var_mt, sd_mt, "")
category_100[["NOTA_LP"]] <- list(lp_df, mean_lp, var_lp, sd_lp, "")
category_100[["SEXO"]] <- list(gender_df, gender.prop, gender.var, gender.sd, "Female")
category_100[["LOCALIZACAO"]] <- list(local_df, local.prop, local.var, local.sd, "Rural")

# Calculating Confidence Intervals for each sample
for (name in names(category_30)){
  names(category_30[[name]]) <- c("Interval_df", "Mean", "Var", "SD", "Main_Variable")
  names(category_100[[name]]) <- c("Interval_df", "Mean", "Var", "SD", "Main_Variable")
  for (i in 1:max_samples){
    Interval_i <- CI(samples_30[[i]][name], sd = category_30[[name]]$SD, variable = category_30[[name]]$Main_Variable)
    category_30[[name]]$Interval_df[i,2:3] <- Interval_i
    category_30[[name]]$Interval_df[i,4] <- between(category_30[[name]]$Mean, Interval_i[1], Interval_i[2])
  }

  for (i in 1:max_samples){
    Interval_i <- CI(samples_100[[i]][name], sd = category_100[[name]]$SD, variable = category_100[[name]]$Main_Variable)
    category_100[[name]]$Interval_df[i,2:3] <- Interval_i
    category_100[[name]]$Interval_df[i,4] <- between(category_100[[name]]$Mean, Interval_i[1], Interval_i[2])
  }

  names(category_30[[name]]$Interval_df) <- c("Index", "Inf_lim", "Sup_lim", "Contido")
  names(category_100[[name]]$Interval_df) <- c("Index", "Inf_lim", "Sup_lim", "Contido")
}

for (name in names(category_30)){
  # Calculating xaxis lmits
  extreme_inf <- min(category_30[[name]]$Interval_df$Inf_lim)
  extreme_sup <- max(category_30[[name]]$Interval_df$Sup_lim)

  ggplot(data = category_30[[name]]$Interval_df, aes(x = category_30[[name]]$Mean, y = `Index`, color=`Contido`)) +
    geom_vline(xintercept=category_30[[name]]$Mean, linetype="dashed") +
    geom_errorbar(xmin = category_30[[name]]$Interval_df$Inf_lim,
                  xmax = category_30[[name]]$Interval_df$Sup_lim) +
    xlim(extreme_inf - abs(extreme_inf)/10, abs(extreme_sup + extreme_sup/10)) +
    labs(x = name_hash(name)) +
    theme_classic() +
    ggsave(paste0(name, "-30.png"))
}

for (name in names(category_100)){
  extreme_inf <- min(category_100[[name]]$Interval_df$Inf_lim)
  extreme_sup <- max(category_100[[name]]$Interval_df$Sup_lim)


  ggplot(data = category_100[[name]]$Interval_df, aes(x = category_100[[name]]$Mean, y = `Index`, color=`Contido`)) +
    geom_vline(xintercept=category_100[[name]]$Mean, linetype="dashed") +
    geom_errorbar(xmin = category_100[[name]]$Interval_df$Inf_lim,
                  xmax = category_100[[name]]$Interval_df$Sup_lim) +
    xlim(extreme_inf - abs(extreme_inf)/10, abs(extreme_sup + extreme_sup/10)) +
    labs(x = name_hash(name)) +
    theme_classic() +
    ggsave(paste0(name, "-100.png"))
}

