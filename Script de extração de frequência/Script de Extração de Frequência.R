library(tidyverse)
library(rstatix)
library(kableExtra)
library(ggpubr)
library(readxl)

# Variável artigo antes de possessivo


art <- read_excel("det.xlsx")


# Taxa por falante
art_1 <- art %>% 
  filter(VD == "ausência") %>% 
  group_by(informante, VD) %>% 
  summarise(n = n()) %>%
  mutate(aplic = sum(n)) %>% 
  ungroup()  %>% 
  select(informante, aplic)

art_2 <- art %>% 
  group_by(informante, VD) %>% 
  summarise(n = n()) %>%
  mutate(total = sum(n)) %>% 
  ungroup()  %>% 
  filter(VD == "presença") %>% 
  select(informante, total)

taxa_art <- inner_join(art_1, art_2)

art.taxas <- taxa_art %>% 
  mutate(art = (aplic / total)*100) %>% 
  unique()

# Extrair informações sociais
sociais <- art %>% 
  select(informante, amostra, deslocamento, tempo, genero, idade) %>% 
  distinct()

# Juntar no novo arquivo
taxas.art <- inner_join(sociais, art.taxas)

# Ordenar níveis
taxas.art$tempo <- factor(taxas.art$tempo, levels = c("início" , "final"))
taxas.art$deslocamento <- factor(taxas.art$deslocamento, levels = c("Deslocamento 1", "Deslocamento 2", "Deslocamento 3", "Deslocamento 4", "Alagoas", "Bahia"))
taxas.art <- taxas.art[order(taxas.art$informante), ]


# Variável pronome pessoal de 2P


voce <- read_excel("pro.xlsx")

# Taxa por falante
voce_1 <- voce %>% 
  filter(VD == "você") %>% 
  group_by(informante, VD) %>% 
  summarise(n = n()) %>%
  mutate(aplic = sum(n)) %>% 
  ungroup()  %>% 
  select(informante, aplic)

voce_2 <- voce %>% 
  group_by(informante, VD) %>% 
  summarise(n = n()) %>%
  mutate(total = sum(n)) %>% 
  ungroup()  %>% 
  filter(VD == "você") %>% 
  select(informante, total)

taxa_voce <- inner_join(voce_1, voce_2)

voce.taxas <- taxa_voce %>% 
  mutate(voce = (aplic / total)*100) %>% 
  unique()


# Juntar os resultados das duas variáveis

dados = left_join(taxas.art[,c("informante", "amostra", "deslocamento", "tempo", "genero", "idade", "art")], 
                  voce.taxas[,c("informante", "voce")],
                  by = "informante")

# Reordenar
dados <- dados[order(dados$informante), ]


# Substituir NA por 0

dados[is.na(dados)] <- 0

# Salvar caso queira

write.csv2(dados,"C:/Users/manoe/OneDrive/Área de Trabalho/Doutorado/Tese/Análise de Covariação/dados.csv", row.names = FALSE, fileEncoding = "UTF-16LE")


# Clíticos de 2PS


cli = read_excel("cli.xlsx")

# Taxa por falante
cli_1 <- cli %>% 
  filter(VD == "te") %>% 
  group_by(informante, VD) %>% 
  summarise(n = n()) %>%
  mutate(aplic = sum(n)) %>% 
  ungroup()  %>% 
  select(informante, aplic)

cli_2 <- cli %>% 
  group_by(informante, VD) %>% 
  summarise(n = n()) %>%
  mutate(total = sum(n)) %>% 
  ungroup()  %>% 
  filter(VD == "te") %>% 
  select(informante, total)

taxa_cli <- inner_join(cli_1, cli_2)

cli.taxas <- taxa_cli %>% 
  mutate(cli = (aplic / total)*100) %>% 
  unique()


# Salvar caso queira
write.csv2(cli.taxas,"C:/Users/manoe/OneDrive/Área de Trabalho/Doutorado/Tese/Análise de Covariação/dados_cli.csv", row.names = FALSE, fileEncoding = "UTF-16LE")


# Juntar os dados de clíticos ao dataset com possessivos e pronomes pessoais 



dados1 = left_join(dados[,c("informante", "amostra", "deslocamento", "tempo", "genero","idade", "art", "voce")], 
                  cli.taxas[,c("informante", "cli")],
                  by = "informante")

# Substituir NA por 0
dados1[is.na(dados1)] <- 0

# Salvar caso queira
write.csv2(dados1,"C:/Users/manoe/OneDrive/Área de Trabalho/Doutorado/Tese/Análise de Covariação/dados1.csv", row.names = FALSE, fileEncoding = "UTF-16LE")

# Possessivos de 2PS

pos = read_excel("pos.xlsx")

# Taxa por falante
pos_1 <- pos %>% 
  filter(VD == "seu") %>% 
  group_by(informante, VD) %>% 
  summarise(n = n()) %>%
  mutate(aplic = sum(n)) %>% 
  ungroup()  %>% 
  select(informante, aplic)

pos_2 <- pos %>% 
  group_by(informante, VD) %>% 
  summarise(n = n()) %>%
  mutate(total = sum(n)) %>% 
  ungroup()  %>% 
  filter(VD == "seu") %>% 
  select(informante, total)

taxa_pos <- inner_join(pos_1, pos_2)

pos.taxas <- taxa_pos %>% 
  mutate(pos = (aplic / total)*100) %>% 
  unique()


# Salvar caso queira
write.csv2(pos.taxas,"C:/Users/manoe/OneDrive/Área de Trabalho/Doutorado/Tese/Análise de Covariação/dados_pos.csv", row.names = FALSE, fileEncoding = "UTF-16LE")


# Juntar dados de possessivos ao dataset com as demais variáveis

dados2 = left_join(dados1[,c("informante", "amostra", "deslocamento", "tempo", "genero", "idade", "art", "voce", "cli")], 
                   pos.taxas[,c("informante", "pos")],
                   by = "informante")

# Substituir NA por 0
dados2[is.na(dados2)] <- 0

# Salvar 
write.csv2(dados2,"C:/Users/manoe/OneDrive/Área de Trabalho/Doutorado/Tese/Análise de Covariação/dados2.csv", row.names = FALSE, fileEncoding = "UTF-16LE")

