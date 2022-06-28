# Analytics Web App Para Grandes Volumes de Dados: Como o PIB e a Desigualdade Social Influenciam no Crescimento da Netflix?


# Configurando o diretório de trabalho
setwd("C:/Users/Eric/Documents/Bigdatanapratica/BigData")
getwd()


########## Script de Carga e Limpeza dos Dados ##########

# Carrega os pacotes
library(dplyr)
library(tidyr)
library(readxl)
library(readr)


##### Carga dos Dados ##### 

# Carregando os dados da Netflix
dados_netflix <- read.csv("datasets_originais/dados_netflix_Dec_2021.csv")
View(dados_netflix)

# Carregando os dados do World Bank
dados_pib <- read.csv("datasets_originais/dados_world_bank.csv", header = FALSE)
View(dados_pib)

# Carregando os dados de desigualdade salarial
dados_salario <- read.csv("datasets_originais/dados_desigualdade_salarial_harvard.csv")
View(dados_salario)

# Carregando dados do IMDB: 
dados_IMDB <- read_tsv("datasets_originais/dados_imdb.tsv")
View(dados_IMDB)

# Carregando dados dos Top 10 shows da Netflix por país
dados_top10 <- read_excel("datasets_originais/top_10_shows_netflix.xlsx")
View(dados_top10)

# Carregando dados de assinantes da Netflix em Julho/2021
dados_sub <- read.csv("datasets_originais/assinantes_netflix_jul_2021.csv")
View(dados_sub)

# Carregando dados de códigos ISO dos países
countrycode <- read.csv("datasets_originais/wikipedia-iso-country-codes.csv")
View(countrycode)


##### Limpeza e Preparação do Primeiro Dataset Combinado ##### 

# Cria uma coluna com a diferença de dados para o gráfico de barras (plano standard - plano básico)
dados_netflix$basic_standard_diff = (dados_netflix$Cost.Per.Month...Standard.... - dados_netflix$Cost.Per.Month...Basic....)

# Cria uma coluna com a diferença de dados para o gráfico de barras (plano premium - plano standard)
dados_netflix$standard_premium_diff = (dados_netflix$Cost.Per.Month...Premium.... - dados_netflix$Cost.Per.Month...Standard....)

# Combina os dados anteriores com dados do PIB
names(dados_pib)[names(dados_pib) == 'V1'] <- 'Country'
dados_netflix_pib <- merge(dados_netflix, dados_pib, by = "Country")

# Extrai o PIB de 2020
dados_netflix_pib2020 <- dados_netflix_pib[-c(11:72, 74, 75)] 
names(dados_netflix_pib2020)[names(dados_netflix_pib2020) == 'V64'] <- "2020 GDP (World Bank)"

# Limpeza do dataframe de desigualdade salarial
dados_salario <- dados_salario[, c(1:3)]
dados_salario_ano <- dados_salario %>% group_by(country) %>% summarise(max = max(year, na.rm = TRUE))

# Combina os dataframes
dados_salario <- merge(dados_salario, dados_salario_ano, by.x = c("country", "year"), by.y = c("country", "max"))
dados_netflix_pib_salario2020 <- merge(dados_netflix_pib2020, dados_salario, by.x=c("Country"), by.y=c("country"))

# Limpa o dataset de faturamento e subscrição e combina com o dataframe anterior
dados_sub <- dados_sub[,c(1, 23,24)]
complete <- merge(dados_netflix_pib_salario2020, dados_sub, by=c("Country"))

# Faz o merge do countrycode para o choropleth map
countrycode <- countrycode[,c(1, 3)]
complete <- merge(complete, countrycode, by.x=c("Country"), by.y=c("English.short.name.lower.case"))
View(complete)
?merge

# Salva o dataframe produzido até aqui
write.csv(complete, "datasets_limpos/dataset1.csv", row.names = FALSE)


##### Limpeza e Preparação do Segundo Dataset Combinado ##### 

# Limpa e filtra o dataframe IMDB
genero <- dados_IMDB[,-c(1, 4:8)]
View(genero)
names(genero)[names(genero) == 'primaryTitle'] <- 'show_title'
View(genero)

# Associa o genêro com os Top 10 shows
topgenero <- merge(dados_top10, genero, by = "show_title")
View(topgenero)

# Limpa o dataframe anterior para manter apenas 1 entrada para cada top 10
topgenero <- topgenero[(topgenero$category == "Films" & topgenero$titleType == "movie") | (topgenero$category == "TV" & topgenero$titleType == "tvSeries"), ] 
topgenero <- distinct(topgenero, show_title, week, country_name, category, titleType,cumulative_weeks_in_top_10, .keep_all= TRUE)
View(topgenero)

# Mantém apenas informação de gênero de filme por país
topgeneropaises <- topgenero[,-c(1, 3:9)]
View(topgeneropaises)

# Pivot do dataframe
topgeneropaises <- separate(topgeneropaises, c("genres") , c("genero1", "genero2", "genero3"), sep = ",")
topgeneropaises <- pivot_longer(topgeneropaises, c("genero1", "genero2", "genero3"), names_to = "genero123", values_to = "genres")
View(topgeneropaises)

# Conta o número de gêneros
generocount <- count(topgeneropaises, country_name, genres)
generocount <- na.omit(generocount)
generocount <-subset(generocount, genres!="\\N")
generocount$n <- as.numeric(generocount$n)
View(generocount)

# Salva em disco
write.csv(generocount, "datasets_limpos/dataset2.csv", row.names = FALSE)


##### Limpeza e Preparação do Terceiro Dataset Combinado ##### 

# Renomeia o dataframe anterior
sunburst <- rename(generocount, label = country_name)

# Remove os traços 
sunburst$genres = sub("-", " ", sunburst$genres)

# Ajusta o nome
sunburst$parent = c("total  - ")
sunburst$parent <- paste(sunburst$parent, sunburst$genres)
sunburst$id = c(" - ")
sunburst$id <- paste(sunburst$parent, sunburst$id)
sunburst$id <- paste(sunburst$id, sunburst$label)
sunburst$n <- as.numeric(sunburst$n)
View(sunburst)

# Agregação
added <- aggregate(sunburst$n, list(sunburst$genres), FUN=sum)
added <- rename(added, label = Group.1)
added <- rename(added, n = x)
added$n <- as.numeric(added$n)
added$genres <- c(NA)
added$parent <- c("total")
added$id <- c(" - ")
added$id <- paste(added$parent, added$id)
added$id <- paste(added$id, added$label)
View(added)

# Calcula soma
total = sum(added$n)
total

# Combina tudo para o dataframe final
sunburst <- rbind(added, sunburst)
sunburst <- rbind(c("total", total, NA, NA, "total"), sunburst)
sunburst <- sunburst[,-c(3)]
sunburst$n <- as.numeric(sunburst$n)
View(sunburst)

# Salva em disco
write.csv(sunburst, "datasets_limpos/dataset3.csv", row.names = FALSE)


##### Limpeza e Preparação do Quarto Dataset Combinado ##### 

# Vamos trabalhar com top 10 para evitar problemas de performance nos gráficos
top10sunburst <- sunburst[-c(1:28),]
top10sunburst$n <- as.numeric(top10sunburst$n)
View(top10sunburst)

# Top 10 gêneros por país
top10sunburst <- top10sunburst %>% 
  group_by(label) %>%
  top_n(10,n)
View(top10sunburst)

# Recalcula os totais, ajusta e combina o dataframe
top10add <- aggregate(top10sunburst$n, list(top10sunburst$parent), FUN = sum)
top10add <- rename(top10add, id = Group.1)
top10add <- rename(top10add, n = x)
top10add$label = sub("total  -  ", "", top10add$id)
top10add$parent = c("total")
top10add$n <- as.numeric(top10add$n)
total = sum(top10add$n)
top10sunburst <- rbind(top10add, top10sunburst)
top10sunburst <- rbind(c("total", total, NA, NA, "total"), top10sunburst)
top10sunburst$n <- as.numeric(top10sunburst$n)
View(top10sunburst)

# Salva em disco
write.csv(top10sunburst, "datasets_limpos/dataset4.csv", row.names = FALSE)


##### Limpeza e Preparação do Quinto Dataset Combinado ##### 

# Filtra o dataframe anterior e cria um novo
nototal <- sunburst[-c(1),]
nototal$parent = sub("total  -  ", "", nototal$parent)
nototal$parent = sub("total", NA, nototal$parent)
nototal$id = sub("total  -  ", "", nototal$id)
View(nototal)

# Salva em disco
write.csv(nototal, "datasets_limpos/dataset5.csv", row.names = FALSE)


##### Limpeza e Preparação do Sexto Dataset Combinado ##### 

# Filtra o dataframe anterior e cria um novo
countrytree <- nototal[-c(1:28),]
countrytree <- rename(countrytree, parents = label)
countrytree <- rename(countrytree, labels = parent)
countrytree$id = c(" - ")
countrytree$id <- paste(countrytree$parent, countrytree$id)
countrytree$id <- paste(countrytree$id, countrytree$label)
countries <- aggregate(countrytree$n, list(countrytree$parents), FUN = sum)
countries <- rename(countries, labels = Group.1)
countries <- rename(countries, n = x)
countries$n <- as.numeric(countries$n)
countries$id <- countries$label
countries$parents <- c(NA)
countrytree <- rbind(countrytree, countries)
View(countrytree)

# Salva em disco
write.csv(countrytree, "datasets_limpos/dataset6.csv", row.names = FALSE)



