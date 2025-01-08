# Teste com efeitos aleatórios
library(lme4)
library(lmerTest)
library(readxl)
library(tidyverse)

# Carregar planilhas
det <- read_excel("det.xlsx")
det <- det %>% mutate_if(is.character, as.factor)
det$VD <- factor(det$VD, levels =c("presença","ausência"))

pro <- read_excel("pro.xlsx")
pro <- pro %>% mutate_if(is.character, as.factor)
pro = pro %>% 
  filter(VD %in% c("você", "cê"))
pro$VD <- factor(pro$VD, levels =c("cê", "você"))

cli = read_excel("cli.xlsx")
cli <- cli %>% mutate_if(is.character, as.factor)

pos = read_excel("pos.xlsx")
pos <- pos %>% mutate_if(is.character, as.factor)
pos$VD <- factor(pos$VD, levels =c("teu", "seu"))

# Modelo com artigo antes de possessivo

mod1 <- glmer(VD ~ (1 | informante), 
                      family = binomial(link = "logit"), data = det)

# Examine os resultados
summary(mod1)


# Obtenha os efeitos aleatórios por indivíduo
mod1_informante <- ranef(mod1)

# Visualize os efeitos aleatórios
print(mod1_informante)


# Modelo com pronome pessoal de 2PS

mod2 <- glmer(VD ~ (1 | informante), 
              family = binomial(link = "logit"), data = pro)

# Examine os resultados
summary(mod2)


# Obtenha os efeitos aleatórios por indivíduo
mod2_informante <- ranef(mod2)

# Visualize os efeitos aleatórios
print(mod2_informante)

# Modelo com clítico de 2PS

mod3 <- glmer(VD ~ (1 | informante), 
              family = binomial(link = "logit"), data = cli)

# Examine os resultados
summary(mod3)


# Obtenha os efeitos aleatórios por indivíduo
mod3_informante <- ranef(mod3)

# Visualize os efeitos aleatórios
print(mod3_informante)


# Modelo com possessivos de 2PS

mod4 <- glmer(VD ~ (1 | informante), 
              family = binomial(link = "logit"), data = pos)

# Examine os resultados
summary(mod4)


# Obtenha os efeitos aleatórios por indivíduo
mod4_informante <- ranef(mod4)

# Visualize os efeitos aleatórios
print(mod4_informante)


# Unir as planilhas

dados = read_excel("dados.xlsx")
det3 = read_excel("det3.xlsx")
pro3 = read_excel("pro3.xlsx")
cli3 = read_excel("cli3.xlsx")
pos3 = read_excel("pos3.xlsx")

dados = left_join(dados[,c("informante", "informante2", "amostra", "deslocamento", "tempo", "genero","idade", "art", "voce", "cli", "pos")], 
                  det3[,c("informante", "art3")],
                  by = "informante")

dados = left_join(dados[,c("informante", "informante2", "amostra", "deslocamento", "tempo", "genero","idade", "art", "voce", "cli", "pos", "art3")], 
                  pro3[,c("informante", "voce3")],
                  by = "informante")

dados = left_join(dados[,c("informante", "informante2", "amostra", "deslocamento", "tempo", "genero","idade", "art", "voce", "cli", "pos", "art3", "voce3")], 
                  cli3[,c("informante", "cli3")],
                  by = "informante")

dados = left_join(dados[,c("informante", "informante2", "amostra", "deslocamento", "tempo", "genero","idade", "art", "voce", "cli", "pos", "art3", "voce3", "cli3")], 
                  pos3[,c("informante", "pos3")],
                  by = "informante")

dados$art3 <- as.numeric(dados$art3)
dados$voce3 <- as.numeric(dados$voce3)
dados$cli3 <- as.numeric(dados$cli3)
dados$pos3 <- as.numeric(dados$pos3)

# Substituir NA por 0
dados[is.na(dados)] <- 0

# Salvar
library(openxlsx)
write.xlsx(dados, "C:/Users/manoe/OneDrive/Área de Trabalho/Doutorado/Tese/Análise de Covariação/Efeitos mistos/dados2.xlsx", row.names = FALSE)
