# Title     : TODO
# Objective : TODO
# Created by: Jose Vitor
# Created on: 29/03/2021

library(dplyr)
library(tibble)
library(EnvStats)

set.seed(1234)

sample30 <- samples_30[[sample(length(samples_30),1)]]
sample100 <- samples_100[[sample(length(samples_100),1)]]

classification<- function(X){
  if (X < 125) {
    return(1)
  } else if (X < 150){
    return(2)
  } else if (X < 175) {
    return(3)
  } else if (X < 200) {
    return(4)
  } else if (X < 225) {
    return(5)
  } else if (X < 250) {
    return(6)
  } else if (X < 275) {
    return(7)
  } else if (X < 300) {
    return(8)
  } else if (X < 325) {
    return(9)
  } else if (X < 350) {
    return(10)
  } else if (X < 375) {
    return(11)
  } else {
    return(12)
  }
}

classification <- Vectorize(classification)

################# Question 1

notas <- sample100["NOTA_MT"]
classified <- classification(notas)

Freq_dist <- tibble(
  Classes = contado) %>%
  group_by(Classes) %>%
  count(Classes) %>%
  mutate(fri = n/nrow(sample100)) %>%
  ungroup() %>%
  mutate(Fri = cumsum(fri))

Freq_dist[,1] <- c("0 |--- 125", "125 |--- 150", "150 |--- 175", "175 |--- 200", "200 |--- 225",
                      "225 |--- 250", "250 |--- 275", "275 |--- 300", "300 |--- 325", "325+")

names(Freq_dist)[2] <- "fi"

peso_medio <- c(125/2) %>%
  append(seq(137.5, 362.5, 25))
peso_medio_ajustado <- peso_medio[1:nrow(Freq_dist)]

Freq_dist <- add_column(classificado, peso_medio_ajustado, .before = "fi")

names(Freq_dist)[2] <- "Mid_W"

Chisq <- EnvStats::gofTest(Freq_dist$fi, test = "chisq")
Shapiro <- EnvStats::gofTest(Freq_dist$fi, test = "sw")
Lillie <- EnvStats::gofTest(Freq_dist$fi, test = "lillie", distribution = "norm")
Anderson <- EnvStats::gofTest(Freq_dist$fi, test = "ad")
