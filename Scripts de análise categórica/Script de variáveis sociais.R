library(readxl)
library(ggstatsplot)
library(tidyverse)
library(report)
library(sjPlot)

# Idade

## Artigo antes de possessivo

det = read_excel("det.xlsx")
det <- det %>% mutate_if(is.character, as.factor) 
det$VD <- factor(det$VD, levels =c("presença","ausência"))


det2019 = det %>% 
  filter(amostra == "Deslocamentos 2019")

det2020 = det %>% 
  filter(amostra == "Deslocamentos 2020")

modelo1 = glm(VD ~ idade, data = det2019, family = binomial)
report(modelo1)

modelo2 = glm(VD ~ idade, data = det2020, family = binomial)
report(modelo2)

tab_model(modelo1, modelo2, prefix.labels = "varname", p.style = "numeric", transform = NULL)


## Pronomes pessoais de 2PS

pro = read_excel("pro.xlsx")
pro = pro %>% 
  filter(VD %in% c("você", "cê"))
pro <- pro %>% mutate_if(is.character, as.factor) 


pro2019 = pro %>% 
  filter(amostra == "Deslocamentos 2019",
         VD %in% c("você", "cê"))


pro2020 = pro %>% 
  filter(amostra == "Deslocamentos 2020",
         VD %in% c("você", "cê"))

modelo3 = glm(VD ~ idade, data = pro2019, family = binomial)
report(modelo3)

modelo4 = glm(VD ~ idade, data = pro2020, family = binomial)
summary(modelo4)

tab_model(modelo3, modelo4, prefix.labels = "varname", p.style = "numeric", transform = NULL)


## Clíticos de 2PS

cli <- read_excel("cli.xlsx")
cli <- cli %>% mutate_if(is.character, as.factor) 

cli2019 = cli %>% 
  filter(amostra == "Deslocamentos 2019")

cli2020 = cli%>% 
  filter(amostra == "Deslocamentos 2020")


modelo5 = glm(VD ~ idade, data = cli2019, family = binomial)
report(modelo5)

modelo6 = glm(VD ~ idade, data = cli2020, family = binomial)
summary(modelo6)

tab_model(modelo5, modelo6, prefix.labels = "varname", p.style = "numeric", transform = NULL)


## Possessivos de 2PS

pos <- read_excel("pos.xlsx")
pos <- pos %>% mutate_if(is.character, as.factor) 
pos$VD <- factor(pos$VD, levels =c("teu","seu"))

pos2019 = pos %>% 
  filter(amostra == "Deslocamentos 2019")

pos2020 = pos%>% 
  filter(amostra == "Deslocamentos 2020")


modelo7 = glm(VD ~ idade, data = pos2019, family = binomial)
summary(modelo7)

modelo8 = glm(VD ~ idade, data = pos2020, family = binomial)
report(modelo8)

tab_model(modelo7, modelo8, prefix.labels = "varname", p.style = "numeric", transform = NULL)


# Gênero

grouped_ggbarstats(
  data         = det,
  x            = VD,
  y            = genero,
  grouping.var = amostra,
  package = "wesanderson",
  palette = "Royal1",
  proportion.test = FALSE,
  bf.message = FALSE)


grouped_ggbarstats(
  data         = pro,
  x            = VD,
  y            = genero,
  grouping.var = amostra,
  perc.k = 1,
  package = "wesanderson",
  palette = "Royal1",
  proportion.test = FALSE,
  bf.message = FALSE)

grouped_ggbarstats(
  data         = cli,
  x            = VD,
  y            = genero,
  grouping.var = amostra,
  package = "wesanderson",
  palette = "Royal1",
  proportion.test = FALSE,
  bf.message = FALSE)

grouped_ggbarstats(
  data         = pos,
  x            = VD,
  y            = genero,
  grouping.var = amostra,
  package = "wesanderson",
  palette = "Royal1",
  proportion.test = FALSE,
  bf.message = FALSE)
