# Análise categórica dos dados (variáveis nominais)

# Carregar pacotes essenciais
library(tidyverse)
library(ggstatsplot)
library(readxl)
library(sjPlot)
library(corrplot)

# Vamos analisar cada variável morfossintática individualmente, seguindo a ordem
# disposta na tese

# Uso variável de artigo antes de possessivo pré-nominal

# Carregar o arquivo de dados
det <- read_excel("det.xlsx", col_types = c("text", 
                                            "text", "text", "text", "numeric", "text", 
                                            "text", "text", "text", "skip", "skip", 
                                            "skip", "skip"))

det <- det %>% mutate_if(is.character, as.factor) # ler dados como factor

# Mudar ordenação de variáveis

det$VD <- factor(det$VD, levels =c("presença","ausência"))
levels(det$VD) #ver ordem

det$deslocamento <- factor(det$deslocamento, levels =c("Deslocamento 1",
                                                       "Deslocamento 2",
                                                       "Deslocamento 3",
                                                       "Deslocamento 4",
                                                       "Alagoas",
                                                       "Bahia"))

det$tempo <- factor(det$tempo, levels =c("início","final"))

det$numero <- factor(det$numero, levels =c("singular","plural"))

# Distribuição geral dos dados

table(det$VD)

ggstatsplot::ggpiestats(
  data = det,
  x = VD,
  perc.k = 1,
  legend.title = " ", 
  package = "wesanderson",
  palette = "Royal1",
  messages = FALSE, 
  bf.message = FALSE) 


# Distribuição por amostra

table(det$VD, det$amostra)

ggstatsplot::ggpiestats(
  data = det,
  x = VD,
  y = amostra,
  perc.k = 1,
  legend.title = " ", 
  package = "wesanderson",
  palette = "Royal1",
  messages = FALSE,
  bf.message = FALSE) + 
  theme(legend.position = "bottom") 


# Observar a distribuição a partir das variáveis sociais

# Vamos criar um data set para cada amostra

det2019 = det %>% 
  filter(amostra == "Deslocamentos 2019")

det2020 = det %>% 
  filter(amostra == "Deslocamentos 2020")


# Deslocamentos

ggstatsplot::ggbarstats(
  data = det2019,
  x = VD,
  y = deslocamento,
  xlab = "Deslocamentos",
  ylab = "Percentual",
  perc.k = 1,
  label.text.size = 4,
  ggstatsplot.layer = FALSE,
  package = "wesanderson",
  palette = "Royal1",
  messages = FALSE,
  bf.message = FALSE,
  proportion.test = FALSE) + 
  theme(legend.position = "bottom")

teste1 <- chisq.test(det2019$VD, det2019$deslocamento)

corrplot(teste1$residuals, is.cor = FALSE, method = "number", insig = "p-value")


ggstatsplot::ggbarstats(
  data = det2020,
  x = VD,
  y = deslocamento,
  xlab = "Deslocamentos",
  ylab = "Percentual",
  perc.k = 1,
  label.text.size = 4,
  ggstatsplot.layer = FALSE,
  package = "wesanderson",
  palette = "Royal1",
  messages = FALSE,
  bf.message = FALSE,
  proportion.test = FALSE) + 
  theme(legend.position = "bottom")

teste2 <- chisq.test(det2020$VD, det2020$deslocamento)

corrplot(teste2$residuals, is.cor = FALSE, method = "number", insig = "p-value") 


# Tempo no curso

ggstatsplot::ggbarstats(
  data = det2019,
  x = VD,
  y = tempo,
  xlab = "Tempo no curso",
  ylab = "Percentual",
  perc.k = 1,
  label.text.size = 4,
  ggstatsplot.layer = FALSE,
  package = "wesanderson",
  palette = "Royal1",
  messages = FALSE,
  bf.message = FALSE,
  label = "both",
  proportion.test = FALSE) + 
  theme(legend.position = "bottom")

teste8 = chisq.test(det2019$VD, det2019$tempo)

corrplot(teste8$residuals, is.cor = FALSE, method = "number", insig = "p-value") 


ggstatsplot::ggbarstats(
  data = det2020,
  x = VD,
  y = tempo,
  xlab = "Tempo no curso",
  ylab = "Percentual",
  perc.k = 1,
  label.text.size = 4,
  ggstatsplot.layer = FALSE,
  package = "wesanderson",
  palette = "Royal1",
  messages = FALSE,
  bf.message = FALSE,
  label = "Both",
  proportion.test = FALSE) + 
  theme(legend.position = "bottom")


teste9 <- chisq.test(det2020$VD, det2020$tempo)

corrplot(teste9$residuals, is.cor = FALSE, method = "number", insig = "p-value") 


# Pronomes pessoais de 2P

# Carregar o arquivo de dados
pro <- read_excel("pro.xlsx", col_types = c("text", 
                                            "text", "text", "text", "numeric", "text", 
                                            "text", "text", "text", "skip", "skip", 
                                            "skip", "skip"))


pro <- pro %>% mutate_if(is.character, as.factor) # ler dados como factor

# Mudar ordenação de variáveis

pro$VD <- factor(pro$VD, levels =c("tu","cê", "você"))
levels(pro$VD) #ver ordem

pro$deslocamento <- factor(pro$deslocamento, levels =c("Deslocamento 1",
                                                       "Deslocamento 2",
                                                       "Deslocamento 3",
                                                       "Deslocamento 4",
                                                       "Alagoas",
                                                       "Bahia"))

pro$tempo <- factor(pro$tempo, levels =c("início","final"))

pro$polaridade <- factor(pro$polaridade, levels =c("positiva","negativa"))

# Distribuição geral dos dados

table(pro$VD)

ggstatsplot::ggpiestats(
  data = pro,
  x = VD,
  perc.k = 1,
  legend.title = " ", 
  package = "wesanderson",
  palette = "Royal1",
  messages = FALSE,
  bf.message = FALSE) 


# Distribuição por amostra

table(pro$VD, pro$amostra)

ggstatsplot::ggpiestats(
  data = pro,
  x = VD,
  y = amostra,
  perc.k = 1,
  legend.title = " ", 
  package = "wesanderson",
  palette = "Royal1",
  messages = FALSE,
  label.repel = TRUE,
  bf.message = FALSE) + 
  theme(legend.position = "bottom") 


# Observar a distribuição a partir das variáveis sociais

# Vamos criar um data set para cada amostra

pro2019 = pro %>% 
  filter(amostra == "Deslocamentos 2019",
         VD %in% c("você", "cê"))

pro2020 = pro %>% 
  filter(amostra == "Deslocamentos 2020",
         VD %in% c("você", "cê"))


# Deslocamentos

ggstatsplot::ggbarstats(
  data = pro2019,
  x = VD,
  y = deslocamento,
  xlab = "Deslocamentos",
  ylab = "Percentual",
  perc.k = 1,
  label.text.size = 4,
  ggstatsplot.layer = FALSE,
  package = "wesanderson",
  palette = "Royal1",
  messages = FALSE,
  bf.message = FALSE,
  label = "both",
  proportion.test = FALSE) + 
  theme(legend.position = "bottom")

teste3 <- chisq.test(pro2019$VD, pro2019$deslocamento)
report(teste3)

corrplot(teste3$residuals, is.cor = FALSE, method = "number", insig = "p-value") 


ggstatsplot::ggbarstats(
  data = pro2020,
  x = VD,
  y = deslocamento,
  xlab = "Deslocamentos",
  ylab = "Percentual",
  perc.k = 1,
  label.text.size = 4,
  ggstatsplot.layer = FALSE,
  package = "wesanderson",
  palette = "Royal1",
  messages = FALSE,
  bf.message = FALSE,
  proportion.test = FALSE) + 
  theme(legend.position = "bottom")

teste4 <- chisq.test(pro2020$VD, pro2020$deslocamento)


corrplot(teste4$residuals, is.cor = FALSE, method = "number", insig = "p-value") 

# Tempo no curso

ggstatsplot::ggbarstats(
  data = pro2019,
  x = VD,
  y = tempo,
  xlab = "Tempo no curso",
  ylab = "Percentual",
  perc.k = 1,
  label.text.size = 4,
  ggstatsplot.layer = FALSE,
  package = "wesanderson",
  palette = "Royal1",
  messages = FALSE,
  bf.message = FALSE,
  proportion.test = FALSE) + 
  theme(legend.position = "bottom")

teste10 <- chisq.test(pro2019$VD, pro2019$tempo)

corrplot(teste10$residuals, is.cor = FALSE, method = "number", insig = "p-value") 


ggstatsplot::ggbarstats(
  data = pro2020,
  x = VD,
  y = tempo,
  xlab = "Tempo no curso",
  ylab = "Percentual",
  perc.k = 1,
  label.text.size = 4,
  ggstatsplot.layer = FALSE,
  package = "wesanderson",
  palette = "Royal1",
  messages = FALSE,
  bf.message = FALSE,
  label = "both",
  proportion.test = FALSE) + 
  theme(legend.position = "bottom")

teste11 <- chisq.test(pro2020$VD, pro2020$tempo)

corrplot(teste11$residuals, is.cor = FALSE, method = "number", insig = "p-value") 


# Clítico de 2P

# Carregar o arquivo de dados
cli <- read_excel("cli.xlsx", col_types = c("text", 
                                            "text", "text", "text", "numeric", "text", 
                                            "text", "text", "text", "skip", "skip", 
                                            "skip", "skip"))

cli <- cli %>% mutate_if(is.character, as.factor) # ler dados como factor

# Mudar ordenação de variáveis


cli$deslocamento <- factor(cli$deslocamento, levels =c("Deslocamento 1",
                                                       "Deslocamento 2",
                                                       "Deslocamento 3",
                                                       "Deslocamento 4",
                                                       "Alagoas",
                                                       "Bahia"))

cli$tempo <- factor(cli$tempo, levels =c("início","final"))



# Distribuição geral dos dados

table(cli$VD)

ggstatsplot::ggpiestats(
  data = cli,
  x = VD,
  perc.k = 1,
  legend.title = " ", 
  package = "wesanderson",
  palette = "Royal1",
  messages = FALSE,
  bf.message = FALSE) 


# Distribuição por amostra

table(cli$VD, cli$amostra)

ggstatsplot::ggpiestats(
  data = cli,
  x = VD,
  y = amostra,
  perc.k = 1,
  legend.title = " ", 
  package = "wesanderson",
  palette = "Royal1",
  messages = FALSE,
  bf.message = FALSE) + 
  theme(legend.position = "bottom") 


# Observar a distribuição a partir das variáveis sociais

# Vamos criar um data set para cada amostra

cli2019 = cli %>% 
  filter(amostra == "Deslocamentos 2019")

cli2020 = cli%>% 
  filter(amostra == "Deslocamentos 2020")


# Deslocamentos

ggstatsplot::ggbarstats(
  data = cli2019,
  x = VD,
  y = deslocamento,
  xlab = "Deslocamentos",
  ylab = "Percentual",
  perc.k = 1,
  label.text.size = 4,
  ggstatsplot.layer = FALSE,
  package = "wesanderson",
  palette = "Royal1",
  messages = FALSE,
  bf.message = FALSE,
  proportion.test = FALSE) + 
  theme(legend.position = "bottom")


teste5 = chisq.test(cli2019$VD, cli2019$deslocamento)

corrplot(teste5$residuals, is.cor = FALSE, method = "number", insig = "p-value") 


ggstatsplot::ggbarstats(
  data = cli2020,
  x = VD,
  y = deslocamento,
  xlab = "Deslocamentos",
  ylab = "Percentual",
  perc.k = 1,
  label.text.size = 4,
  ggstatsplot.layer = FALSE,
  package = "wesanderson",
  palette = "Royal1",
  messages = FALSE,
  bf.message = FALSE,
  label = "both",
  proportion.test = FALSE) + 
  theme(legend.position = "bottom")

teste6 = chisq.test(cli2020$VD, cli2020$deslocamento)

corrplot(teste6$residuals, is.cor = FALSE, method = "number", insig = "p-value") 

fisher.test(cli2020$VD, cli2020$deslocamento)

# Tempo no curso

fisher.test(cli2019$VD, cli2019$tempo)

ggstatsplot::ggbarstats(
  data = cli2019,
  x = VD,
  y = tempo,
  xlab = "Tempo no curso",
  ylab = "Percentual",
  perc.k = 1,
  label.text.size = 4,
  ggstatsplot.layer = FALSE,
  package = "wesanderson",
  palette = "Royal1",
  messages = FALSE,
  bf.message = FALSE,
  proportion.test = FALSE) + 
  theme(legend.position = "bottom")

teste12 = chisq.test(cli2019$VD, cli2019$tempo)

corrplot(teste12$residuals, is.cor = FALSE, method = "number", insig = "p-value") 


fisher.test(cli2020$VD, cli2020$tempo)

ggstatsplot::ggbarstats(
  data = cli2020,
  x = VD,
  y = tempo,
  xlab = "Tempo no curso",
  ylab = "Percentual",
  perc.k = 1,
  label.text.size = 4,
  ggstatsplot.layer = FALSE,
  package = "wesanderson",
  palette = "Royal1",
  messages = FALSE,
  bf.message = FALSE,
  label = "both",
  proportion.test = FALSE) + 
  theme(legend.position = "bottom")

teste13 = chisq.test(cli2020$VD, cli2020$tempo)

corrplot(teste13$residuals, is.cor = FALSE, method = "number", insig = "p-value") 


# Possessivo de 2PS

# Carregar o arquivo de dados
pos <- read_excel("pos.xlsx", col_types = c("text", 
                                            "text", "text", "text", "skip", "text", 
                                            "text", "text", "text", "skip", "skip", 
                                            "skip", "skip"))

pos <- pos %>% mutate_if(is.character, as.factor) # ler dados como factor

# Mudar ordenação de variáveis


pos$deslocamento <- factor(pos$deslocamento, levels =c("Deslocamento 1",
                                                       "Deslocamento 2",
                                                       "Deslocamento 3",
                                                       "Deslocamento 4",
                                                       "Alagoas",
                                                       "Bahia"))

pos$tempo <- factor(pos$tempo, levels =c("início","final"))



# Distribuição geral dos dados

table(pos$VD)

ggstatsplot::ggpiestats(
  data = pos,
  x = VD,
  perc.k = 1,
  legend.title = " ", 
  package = "wesanderson",
  palette = "Royal1",
  messages = FALSE,
 bf.message = FALSE) 


# Distribuição por amostra

table(pos$VD, pos$amostra)

ggstatsplot::ggpiestats(
  data = pos,
  x = VD,
  y = amostra,
  perc.k = 1,
  legend.title = " ", 
  package = "wesanderson",
  palette = "Royal1",
  messages = FALSE,
  bf.message = FALSE) + 
  theme(legend.position = "bottom") 


# Observar a distribuição a partir das variáveis sociais

# Vamos criar um data set para cada amostra

pos2019 = pos %>% 
  filter(amostra == "Deslocamentos 2019")

pos2020 = pos%>% 
  filter(amostra == "Deslocamentos 2020")


# Deslocamentos

ggstatsplot::ggbarstats(
  data = pos2019,
  x = VD,
  y = deslocamento,
  xlab = "Deslocamentos",
  ylab = "Percentual",
  perc.k = 1,
  label.text.size = 4,
  ggstatsplot.layer = FALSE,
  package = "wesanderson",
  palette = "Royal1",
  messages = FALSE,
  bf.message = FALSE,
  proportion.test = FALSE) + 
  theme(legend.position = "bottom")

ggstatsplot::ggbarstats(
  data = pos2020,
  x = VD,
  y = deslocamento,
  xlab = "Deslocamentos",
  ylab = "Percentual",
  perc.k = 1,
  label.text.size = 4,
  ggstatsplot.layer = FALSE,
  package = "wesanderson",
  palette = "Royal1",
  messages = FALSE,
  bf.message = FALSE,
  proportion.test = FALSE) + 
  theme(legend.position = "bottom")

# Tempo no curso

ggstatsplot::ggbarstats(
  data = pos2019,
  x = VD,
  y = tempo,
  xlab = "Tempo no curso",
  ylab = "Percentual",
  perc.k = 1,
  label.text.size = 4,
  ggstatsplot.layer = FALSE,
  package = "wesanderson",
  palette = "Royal1",
  messages = FALSE,
  bf.message = FALSE,
  proportion.test = FALSE) + 
  theme(legend.position = "bottom")

ggstatsplot::ggbarstats(
  data = pos2020,
  x = VD,
  y = tempo,
  xlab = "Tempo no curso",
  ylab = "Percentual",
  perc.k = 1,
  label.text.size = 4,
  ggstatsplot.layer = FALSE,
  package = "wesanderson",
  palette = "Royal1",
  messages = FALSE,
  bf.message = FALSE,
  proportion.test = FALSE) + 
  theme(legend.position = "bottom")