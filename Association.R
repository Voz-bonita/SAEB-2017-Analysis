library(dplyr)
library(ggplot2)

population <- read.csv("SAEB.sample.csv")

MT_LOC <- tibble(
  NOTA_MT = population$NOTA_MT,
  LOCAL = population$LOCALIZACAO
)

MT_LOC$LOCAL <- recode(MT_LOC$LOCAL, "1" = "Urbana", "2" = "Rural")

ggplot(MT_LOC, aes(x = LOCAL, y = NOTA_MT, fill = LOCAL)) +
  geom_boxplot() +
  xlab("Localização") + ylab("Nota em matemática") +
  guides(fill=guide_legend(title = "Localização"))
# ggsave("MT_LOC.png")

# Medidas de posicao

tabela1 <- MT_LOC %>%
  group_by(LOCAL) %>%
  summarise(Media = mean(NOTA_MT),
            Mediana = median(NOTA_MT),
            Amplitude = (max(NOTA_MT)-min(NOTA_MT)),
            Pri_Quartil = quantile(NOTA_MT, .25),
            Seg_Quartil = quantile(NOTA_MT, .50),
            Ter_Quartil = quantile(NOTA_MT, .75),
            DesvioPadrao = sqrt(var(NOTA_MT)),
            n = n())

LP_ADM <- tibble(
  NOTA_LP = population$NOTA_LP,
  ADM = population$DEPENDENCIA_ADM
)

LP_ADM$ADM <- recode(LP_ADM$ADM, "1" = "Estadual+Federal", "2" = "Estadual+Federal", "3" = "Municipal")

ggplot(LP_ADM, aes(x = ADM, y = NOTA_LP, fill = ADM)) +
  geom_boxplot() +
  xlab("Dependência administrativa") + ylab("Nota em Língua Portuguesa") +
  guides(fill=guide_legend(title = "Dependência\nadministrativa"))
# ggsave("LP_ADM.png")

tabela2 <- LP_ADM %>%
  group_by(ADM) %>%
  summarise(Media = mean(NOTA_LP),
            Mediana = median(NOTA_LP),
            Amplitude = (max(NOTA_LP)-min(NOTA_LP)),
            Pri_Quartil = quantile(NOTA_LP, .25),
            Seg_Quartil = quantile(NOTA_LP, .50),
            Ter_Quartil = quantile(NOTA_LP, .75),
            DesvioPadrao = sqrt(var(NOTA_LP)),
            n = n())

# Teste de homocedasticidade

set.seed(2.2020)

sam30 <- sample_n(population, 30)
sam100 <- sample_n(population, 100)

sumarizar <- function(data, notas) {
  summarise(data, n = n(),
            sd = sqrt(var(notas)),
            media = mean(notas))
}

mtloc30 <- tibble(
  notas = sam30$NOTA_MT,
  local = sam30$LOCALIZACAO) %>%
  group_by(local)

mtloc100 <- tibble(
  notas = sam100$NOTA_MT,
  local = sam100$LOCALIZACAO) %>%
  group_by(local)

lpadm30 <- tibble(
  notas = sam30$NOTA_LP,
  adm = sam30$DEPENDENCIA_ADM) %>%
  group_by(adm)

lpadm100 <- tibble(
  notas = sam100$NOTA_LP,
  adm = sam100$DEPENDENCIA_ADM) %>%
  group_by(adm)

caso1mt <- sumarizar(mtloc30, notas)
caso2mt <- sumarizar(mtloc100, notas)
caso1lp <- sumarizar(lpadm30, notas)
caso2lp <- sumarizar(lpadm100, notas)

homocedasticidade <- function (conf.side = 0.975, sd, n) {
  sd <- as.vector(sd)
  lsup <- round(qf(conf.side, n[1]-1, n[2]-1),4)
  linf <- round(1/lsup,4)
  W <- round((sd[1]/sd[2])^2,4)
  print(paste("Estatística =",W))
  print(paste(linf, lsup))
  if (between(W, linf, lsup)) {
    print("Aceite H0")
  } else {
    print("Rejeite H0")
  }

}

homocedasticidade(sd = caso1mt$sd, n = caso1mt$n)
homocedasticidade(sd = caso2mt$sd, n = caso2mt$n)
homocedasticidade(sd = caso1lp$sd, n = caso1lp$n)
homocedasticidade(sd = caso2lp$sd, n = caso2lp$n)

shapiro.test(mtloc30$notas)
shapiro.test(mtloc100$notas)
shapiro.test(lpadm30$notas)
shapiro.test(lpadm100$notas)
