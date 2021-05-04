library(pacman)
p_load(dplyr, tibble, DescTools, ggplot2, PMCMR, tidyr)


set.seed(2.2020)
amostra <- read.csv("SAEB.sample.csv") %>%
  sample_n(500, replace = F)


amostra_mt <- amostra %>%
  select(NOTA_MT, REGIAO)

amostra_mt$REGIAO <- amostra_mt$REGIAO %>%
  factor(levels = c("1","2","3","4","5")) %>%
  recode("1"="Norte", "2"="Nordeste", "3"="Sudeste", "4"="Sul", "5"="Centro-Oeste")

# Existem observacoes faltantes em uso do tempo de telas
amostra_lp <- amostra %>%
  select(NOTA_LP, USO_TEMPO_TELAS) %>%
  filter(USO_TEMPO_TELAS != " ")

amostra_lp$USO_TEMPO_TELAS <- amostra_lp$USO_TEMPO_TELAS %>%
  recode("E" = "A") %>%
  factor(levels = c("A","B","C","D")) %>%
  recode("A" = "Menos de 1 hora", "B"="Entre 1 e 2 horas",
         "C" = "Mais de 2 horas, até 3 horas", "D" = "Mais de 3 horas")


## Testes

# Homocedasticidade
LeveneTest(amostra_mt$NOTA_MT, amostra_mt$REGIAO)
LeveneTest(amostra_lp$NOTA_LP, amostra_lp$USO_TEMPO_TELAS)

# ANOVA (Um fator)
anova <- aov(amostra_mt$NOTA_MT ~ amostra_mt$REGIAO)
summary(anova)

anova <- aov(amostra_lp$NOTA_LP ~ amostra_lp$USO_TEMPO_TELAS)
summary(anova)

# Nao parametrico
kruskal.test(amostra_mt$NOTA_MT, amostra_mt$REGIAO)
kruskal.test(amostra_lp$NOTA_LP, amostra_lp$USO_TEMPO_TELAS)


# Entre quais
posthoc.kruskal.conover.test(amostra_mt$NOTA_MT, amostra_mt$REGIAO)
posthoc.kruskal.conover.test(amostra_lp$NOTA_LP, amostra_lp$USO_TEMPO_TELAS)


ggplot(amostra_mt, aes(x=REGIAO, y=NOTA_MT, fill=REGIAO)) +
  geom_boxplot(width = 0.5) +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white") +
  guides(fill = guide_legend(title = "Região")) +
  labs(x="Região", y="Notas") +
  theme_bw()
# ggsave("MT_X_REGIAO.png")

ggplot(amostra_lp, aes(x=USO_TEMPO_TELAS, y=NOTA_LP, fill=USO_TEMPO_TELAS)) +
  geom_boxplot(width = 0.5) +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white") +
  guides(fill = guide_legend(title = "Uso do tempo de tela")) +
  labs(x="Uso do tempo de tela", y="Notas") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = -10, vjust = 0.1))
# ggsave("LP_X_TELAS.png")