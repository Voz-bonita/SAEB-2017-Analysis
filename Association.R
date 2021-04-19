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
            Maior_nota = max(NOTA_MT),
            Amplitude = (max(NOTA_MT)-min(NOTA_MT)),
            Pri_Quartil = quantile(NOTA_MT, .25),
            Seg_Quartil = quantile(NOTA_MT, .50),
            Ter_Quartil = quantile(NOTA_MT, .75),
            DesvioPadrao = sqrt(var(NOTA_MT)))

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
            Maior_nota = max(NOTA_LP),
            Amplitude = (max(NOTA_LP)-min(NOTA_LP)),
            Pri_Quartil = quantile(NOTA_LP, .25),
            Seg_Quartil = quantile(NOTA_LP, .50),
            Ter_Quartil = quantile(NOTA_LP, .75),
            DesvioPadrao = sqrt(var(NOTA_LP)))