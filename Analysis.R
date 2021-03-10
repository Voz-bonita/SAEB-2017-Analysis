library(dplyr)
library(ggplot2)
library(reshape2)
library(kableExtra)
library(hash)
library(tibble)
library(readr)
library(purrr)

amostra <- read.csv("amostra_190089971.csv")

############ Questao 3

idade <- amostra$IDADE
regiao <- amostra$REGIAO
sexo <- amostra$SEXO
reunioes <- amostra$REUNI�.ES_ESCOLARES
biblioteca <- amostra$BIBLIOTECA

amostra_simplificada <- tibble(
  "Idade" = idade,
  "Regiao" = regiao,
  "Sexo" = sexo,
  "Reunioes" = reunioes,
  "Biblioteca" = biblioteca
)

age_hash <- hash()
age_hash$A <- "8 anos ou menos"
age_hash$B <- "9 anos" 
age_hash$C <- "10 anos"
age_hash$D <- "11 anos"
age_hash$E <- "12 anos"
age_hash$F <- "13 anos"
age_hash$G <- "14 anos"
age_hash$H <- "15 anos ou mais"
age_hash$" " <- "N�o informado"

reg_hash <- hash()
reg_hash$"1" <- "Norte"
reg_hash$"2" <- "Nordeste" 
reg_hash$"3" <- "Sudeste" 
reg_hash$"4" <- "Sul" 
reg_hash$"5" <- "Centro-Oeste"
  
sexo_hash <- hash()
sexo_hash$A <- "Masculino"
sexo_hash$B <- "Feminino"

reu_hash <- hash()
reu_hash$A <- "Sempre ou quase sempre" 
reu_hash$B <- "De vez em quando" 
reu_hash$C <- "Nunca ou quase nunca"
reu_hash$" " <- "N�o informado"

bib_hash <- hash()
bib_hash$A <- "Sempre ou quase sempre"
bib_hash$B <- "De vez em quando" 
bib_hash$C <- "Nunca ou quase nunca" 
bib_hash$D <- "A escola n�o possui biblioteca ou sala de leitura"

amostra_simplificada$Idade <- unlist(map(amostra_simplificada$Idade, ~age_hash[[.x]]))
amostra_simplificada$Regiao <- unlist(map(amostra_simplificada$Regiao, ~reg_hash[[as.character(.x)]]))
amostra_simplificada$Sexo <- unlist(map(amostra_simplificada$Sexo, ~sexo_hash[[.x]]))
amostra_simplificada$Reunioes <- unlist(map(amostra_simplificada$Reunioes, ~reu_hash[[.x]]))
amostra_simplificada$Biblioteca <- unlist(map(amostra_simplificada$Biblioteca, ~bib_hash[[.x]]))

categorias <- names(amostra_simplificada)

################# Tabelas

total <- length(amostra_simplificada[[1]])

for (nome in categorias) {
  print(nome)

  simp_tbl <- group_by(amostra_simplificada, get(nome)) %>%
    count(get(nome)) %>%
    mutate(fri = (n/total))

  simp_tbl <- mutate(as.data.frame(simp_tbl), "Fri" = cumsum(fri))

  names(simp_tbl) <- c(nome, "Frequ�ncia absoluta", "Frequ�ncia relativa", "Frequ�ncia relativa acumulada")

  if (nome %in% c("Idade", "Reunioes")) {
    simp_tbl[1,1] <- NA
  }

  file_name <- paste("Q3-Tbl-", nome, ".png", sep="")

  kable(simp_tbl) %>%
    kable_styling("striped") %>%
    save_kable(
      file = file_name
    )

}

################# Gr�ficos 

for (nome in categorias) {
  print(nome)
  amostra_grafica <- amostra_simplificada %>%
    group_by(get(nome)) %>%
    count() 
    
  names(amostra_grafica) <- c(nome, "n")  
  
  amostra_grafica <- mutate(as.data.frame(amostra_grafica), ypos = cumsum(n) - 0.5 * n) %>%
    mutate(labels = paste(as.character(round(n/total*100,2)), "%", sep = ""))
  
  if (nome == "Idade") {
    amostra_grafica$Idade = factor(amostra_grafica$Idade,
                                        levels = c("8 anos ou menos","9 anos","10 anos",
                                                   "11 anos",
                                                   "12 anos", 
                                                   "13 anos",
                                                   "14 anos",
                                                   "15 anos ou mais",
                                                   "N�o informado"), ordered = TRUE)
  }else if (nome == "Reunioes") {
    amostra_grafica$Reunioes = factor(amostra_grafica$Reunioes,
                                      levels = c("Sempre ou quase sempre",
                                                 "De vez em quando",
                                                 "Nunca ou quase nunca",
                                                 "N�o informado"    ))
  }
  
  
  if (nome %in% c("Biblioteca", "Sexo")) {
    plot <- ggplot(data = amostra_grafica, aes(x="", y=n, fill=get(nome))) +
      geom_bar(stat="identity", width=1)  + 
      coord_polar("y", start = 0) +
      theme_void() +
      theme(legend.title  = element_blank()) +
      geom_text(aes(y = ypos, label = labels), color = "white", size=5) 
  }else{
    plot <- ggplot(data = amostra_grafica, aes(x=get(nome),y=n)) +
      geom_bar(stat="identity") +
      labs(x=nome,
           y="Frequ�ncia") +
      coord_flip()
  }
  
  plot + ggsave(paste("Q3-", nome, ".png", sep = ""))
  
}




ggplot(data = amostra_grafica, aes(x="", y=n, fill=get(nome))) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text(aes(y = ypos, label = labels), color = "white", size=6) +
  scale_fill_brewer(palette="Dark2") 




############# Quest�o 4

##### Preparativos

tipo_notas <- c("NOTA_LP", "NOTA_MT")
full_label <- c("L�ngua Portuguesa", "Matem�tica") 

medidas <- c()

medias <- c()
medianas <- c()
modas <- c()

desvios_medios <- c()
variancias <- c()
desvios_padroes <- c()
c_de_variacao <- c()

classificacao <- function(X){
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

classificacao <- Vectorize(classificacao)

################# a)

peso_medio <- c(125/2) %>%
  append(seq(137.5, 362.5, 25))

for (label in tipo_notas) {
  notas <- amostra[label]

  contado <- classificacao(notas)

  classificado <- tibble(
    Classes = contado,
  ) %>%
    group_by(Classes) %>%
    count(Classes) %>%
    mutate(fri = n/total)

  classificado <- mutate(as.data.frame(classificado), Fri = cumsum(fri))

  classificado[,1] <- c("0 |--- 125", "125 |--- 150", "150 |--- 175", "175 |--- 200", "200 |--- 225",
                        "225 |--- 250", "250 |--- 275", "275 |--- 300", "300 |--- 325", "325 |--- 350",
                        "350 |--- 375", "375+")[1:length(classificado$Classes)]

  names(classificado)[1:2] <- c("Nota", "fi")

  peso_medio_ajustado <- peso_medio[1:length(classificado$Nota)]

  classificado <- add_column(classificado, peso_medio_ajustado, .before = "fi")

  names(classificado)[2] <- "Peso Medio"

  
  ######## Preparativos para o item c)

  obs <- classificado$fi

  parcelas <- (classificado$`Peso Medio`*obs)
  media <- sum(parcelas)/total
  medias <- append(medias, media)


  classe_mediana <- median(contado)
  mediana <- peso_medio[classe_mediana]
  medianas <- append(medianas, mediana)

  moda.i <- which(obs == max(obs))
  moda <- peso_medio[moda.i]
  modas <- append(modas, moda)
  
  desvio_medio <- sum(abs(classificado$`Peso Medio`-media))/total
  desvios_medios <- append(desvios_medios, desvio_medio)
  
  variancia <- sum((((classificado$`Peso Medio`-media)^2)*obs))/total
  variancias <- append(variancias, variancia)
  
  desvio_padrao <- sqrt(variancia)
  desvios_padroes <- append(desvios_padroes, desvio_padrao)
  
  coeficiente_variacao <- desvio_padrao / media
  c_de_variacao <- append(c_de_variacao, coeficiente_variacao)
  
  minimo <- classificado$`Peso Medio`[1]
  maximo <- classificado$`Peso Medio`[length(classificado$`Peso Medio`)]
  
  coef_assimetria <- 3*(media-mediana)/desvio_padrao
  
  
  quartis <- quantile(contado)
  prim_quartil <- classificado$`Peso Medio`[quartis[[2]]]
  terc_quartil <- classificado$`Peso Medio`[quartis[[4]]]
  prim_decil <- classificado$`Peso Medio`[quantile(contado, 0.1)[[1]]]
  non_decil <- classificado$`Peso Medio`[quantile(contado, 0.9)[[1]]]
  
  coef_curtose <- (terc_quartil - prim_quartil)/(2*(non_decil-prim_decil))
  
  medidas <- c(media,mediana,moda,minimo,maximo,variancia,coeficiente_variacao,coef_assimetria,coef_curtose)
  medidas <- round(medidas, digits=4)
  
  estatisticas <- c("Media","Mediana","Moda","M�nimo","Maximo","Variancia","Varia��o","Assimetria","Curtose") 
  
  dados_res <- data.frame(
    "Estat�stica" = estatisticas,
    "Notas" = medidas
  )
  
  names(classificado)[3:5] <- c("Frequ�ncia absoluta", "Frequ�ncia relativa", "Frequ�ncia relativa acumulada")
  
  kable(classificado) %>%
    kable_styling("striped") %>%
    save_kable(
      paste("Q4a-", label, ".png", sep = "")
    )
  
  
  kable(dados_res) %>%
    kable_styling("striped", position = "center") %>%
    save_kable(
      paste("Q4c-", label, ".png", sep = "")

    )

  ggplot(data = classificado, aes(y = `Frequ�ncia absoluta`)) +
    geom_boxplot(fill="#A4A4A4", color="black") +
    scale_x_continuous(breaks = NULL) +
    labs(x = full_label[match(label, tipo_notas)],
         y = "Notas") +
    ggsave(paste("Q4d-", label, ".png", sep = ""))
  
}

################# b) 

sequencia <- c(0) %>%
  append(seq(125, 375, 25))

for (label in tipo_notas) {
  ggplot(data = amostra, aes(x=get(label))) +
    geom_histogram(breaks=sequencia,
                   col="green",
                   fill="dark blue") +
    scale_x_continuous(breaks = sequencia) +
    labs(x = "Notas",
         y = "Frequ�ncia") +
    ggsave(paste("Q4b-", label, ".png", sep = ""))

}

################# c)

medianas <- format(round(medianas, digits=4), nsmall=4)
modas <- format(round(modas, digits=4), nsmall=4)
desvios_medios <- format(round(desvios_medios, digits=4), nsmall=4)
variancias <- format(round(variancias, digits=4), nsmall=4)
desvios_padroes <- format(round(desvios_padroes, digits=4), nsmall=4)
c_de_variacao <- format(round(c_de_variacao, digits=4), nsmall=4)


NOTAS.df <- data.frame(
  Medidas = c("Media--", "Mediana", "Moda---", "Des-med", "Var----", "Sta-dev", "C-d-var"),
  "NOTA_LP" = c(medias[1],medianas[1], modas[1], desvios_medios[1], variancias[1], desvios_padroes[1], c_de_variacao[1]),
  "NOTA_MT" = c(medias[2],medianas[2], modas[2], desvios_medios[2], variancias[2], desvios_padroes[2], c_de_variacao[2])
)

write.table(NOTAS.df, "Q4-c-posicao.txt",sep="\t",row.names=FALSE)


