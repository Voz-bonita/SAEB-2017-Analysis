library(pacman)
p_load(dplyr, tibble, DescTools, ggplot2, PMCMR, tidyr, reshape2)


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
amostra$IDADE <- as.character(amostra$IDADE > "D") %>%
  recode("FALSE" = "Não tem defasagem", "TRUE" = "Tem defasagem")

amostra$SEXO <- recode(amostra$SEXO, "A"="Masculino", "B"="Feminino")

amostra$RACA_COR <- recode(amostra$RACA_COR, "A"="Branca", "B"="Preta",
                           "C"="Parda", "D"="Amarela",
                           "E"="Indígena", "F"="Não quero declarar")

amostra$REGIAO <- amostra$REGIAO %>%
  factor(levels = c("1","2","3","4","5")) %>%
  recode("1"="Norte", "2"="Nordeste", "3"="Sudeste", "4"="Sul", "5"="Centro-Oeste")

amostra$AFAZERES_DOM <- recode(amostra$AFAZERES_DOM, "A"="Menos de 1 hora",
                               "B"="Entre 1 e 2 horas", "C"="Mais de 2 horas, até 3 horas",
                               "D"="Mais de 3 horas", "E"="Não faço trabalhos domésticos")

# write.csv(amostra, "Artigo_amostra.csv", row.names=FALSE)


#### Regiao
Reg_Sexo <- amostra[c("NOTA_MT", "AFAZERES_DOM", "REGIAO", "SEXO")]


## Normalidade
shapiro.test(Reg_Sexo$NOTA_MT)

## Homocedasticidade
bartlett.test(Reg_Sexo$NOTA_MT ~ Reg_Sexo$REGIAO)

## ANOVA
anova <- aov(Reg_Sexo$NOTA_MT ~ Reg_Sexo$REGIAO)
summary(anova)

## Aceita diferenca, mas entre quais

# Tabular
pairwise.t.test (Reg_Sexo$NOTA_MT, Reg_Sexo$REGIAO)

# Grafico
ggplot(Reg_Sexo, aes(x=REGIAO, y=NOTA_MT, fill=REGIAO)) +
  geom_boxplot(width = 0.5) +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white") +
  guides(fill = guide_legend(title = "Região")) +
  labs(x="Região", y="Notas") +
  theme_bw()
# ggsave("Artigo/MT_X_REGIAO.png")


#### Sexo

# shapiro.test(filter(Reg_Sexo, SEXO == "Masculino")$NOTA_MT)$p.value
## Homocedasticidade
bartlett.test(Reg_Sexo$NOTA_MT ~ Reg_Sexo$SEXO)

## ANOVA
anova <- aov(Reg_Sexo$NOTA_MT ~ Reg_Sexo$SEXO)
summary(anova)
# Nao aceita diferencas

# Grafico
ggplot(Reg_Sexo, aes(x=SEXO, y=NOTA_MT, fill=SEXO)) +
  geom_boxplot(width = 0.5) +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white") +
  guides(fill = guide_legend(title = "Região")) +
  labs(x="Região", y="Notas") +
  theme_bw()
# ggsave("Artigo/MT_X_SEXO.png")

### E se aceitasse
SEXO_AFAZERES <- Reg_Sexo %>%
  group_by(SEXO) %>%
  count(AFAZERES_DOM) %>%
  dcast(AFAZERES_DOM ~ SEXO)

chisq.test(SEXO_AFAZERES[c("Feminino", "Masculino")])
