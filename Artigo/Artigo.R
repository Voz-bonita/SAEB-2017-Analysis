library(pacman)
p_load(dplyr, tibble, DescTools, ggplot2, PMCMR, tidyr)


# Os dados tem observacoes faltantes em forma de " "
space_to_na <- function (tbl) {
  tbl[tbl == " "] <- NA
  return(tbl)
}

# Amostra escolhida aleatoriamente
set.seed(2/2020)
amostra <- read.csv("SAEB.sample.csv") %>%
  sample_n(100) %>%
  space_to_na() %>%
  na.omit() %>%
  select("NOTA_MT", "AFAZERES_DOM",
         "REGIAO", "SEXO",
         "RACA_COR", "IDADE")

## Corrigindo Labels
# TRUE = Defasagem
amostra$IDADE <- amostra$IDADE > "D"


amostra$REGIAO <- amostra$REGIAO %>%
  factor(levels = c("1","2","3","4","5")) %>%
  recode("1"="Norte", "2"="Nordeste", "3"="Sudeste", "4"="Sul", "5"="Centro-Oeste")


Reg_Sexo <- amostra[c("NOTA_MT", "AFAZERES_DOM", "REGIAO", "SEXO")]


## Normalidade
shapiro.test(Reg_Sexo$NOTA_MT)

## Homocedasticidade
bartlett.test(amostra$NOTA_MT ~ amostra$REGIAO)

## ANOVA
anova <- aov(amostra$NOTA_MT ~ amostra$REGIAO)
summary(anova)

## Aceita diferenca, mas entre quais

# Tabular
pairwise.t.test (amostra$NOTA_MT, amostra$REGIAO)

# Grafico
ggplot(amostra, aes(x=REGIAO, y=NOTA_MT, fill=REGIAO)) +
  geom_boxplot(width = 0.5) +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white") +
  guides(fill = guide_legend(title = "Região")) +
  labs(x="Região", y="Notas") +
  theme_bw()
# ggsave("Artigo/MT_X_REGIAO.png")