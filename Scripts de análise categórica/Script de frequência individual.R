library(tidyverse)
library(readxl)
library(ggstatsplot)
library(gginnards)

# Artigo antes de possessivo
det <- read_excel("det.xlsx")

a = ggstatsplot::ggbarstats(
  data = det,
  x = VD,
  y = informante,
  xlab = "Informantes",
  ylab = "Percentual",
  perc.k = 0,
  label.text.size = 4,
  ggstatsplot.layer = FALSE,
  package = "wesanderson",
  palette = "Royal1",
  messages = FALSE,
  bf.message = FALSE,
  label = FALSE,
  proportion.test = FALSE,
  results.subtitle = FALSE) 

b = delete_layers(a, "GeomLabel") 
delete_layers(b, "GeomText") +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_blank()) 

png(filename= "art individual.jpeg", 
    res= 300,  height= 10, width=20, unit="cm") 
dev.off()


# Pronomes pessoais de 2PS
pro <- read_excel("pro.xlsx")

c = ggstatsplot::ggbarstats(
  data = pro,
  x = VD,
  y = informante,
  xlab = "Informantes",
  ylab = "Percentual",
  perc.k = 0,
  label.text.size = 4,
  ggstatsplot.layer = FALSE,
  package = "wesanderson",
  palette = "Royal1",
  messages = FALSE,
  bf.message = FALSE,
  label = FALSE,
  proportion.test = FALSE,
  results.subtitle = FALSE) 

d = delete_layers(c, "GeomLabel") 
delete_layers(d, "GeomText") +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_blank()) 

png(filename= "pro individual.jpeg", 
    res= 300,  height= 10, width=20, unit="cm") 
dev.off()


# Clíticos de 2PS
cli <- read_excel("cli.xlsx")

e = ggstatsplot::ggbarstats(
  data = cli,
  x = VD,
  y = informante,
  xlab = "Informantes",
  ylab = "Percentual",
  perc.k = 0,
  label.text.size = 4,
  ggstatsplot.layer = FALSE,
  package = "wesanderson",
  palette = "Royal1",
  messages = FALSE,
  bf.message = FALSE,
  label = FALSE,
  proportion.test = FALSE,
  results.subtitle = FALSE) 

f = delete_layers(e, "GeomLabel") 
delete_layers(f, "GeomText") +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_blank()) 

png(filename= "cli individual.jpeg", 
    res= 300,  height= 10, width=20, unit="cm") 
dev.off()


# Possessivos de 2PS
pos <- read_excel("pos.xlsx")

g = ggstatsplot::ggbarstats(
  data = pos,
  x = VD,
  y = informante,
  xlab = "Informantes",
  ylab = "Percentual",
  perc.k = 0,
  label.text.size = 4,
  ggstatsplot.layer = FALSE,
  package = "wesanderson",
  palette = "Royal1",
  messages = FALSE,
  bf.message = FALSE,
  label = FALSE,
  proportion.test = FALSE,
  results.subtitle = FALSE) 

h = delete_layers(g, "GeomLabel") 
delete_layers(h, "GeomText") +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_blank()) 

png(filename= "pos individual.jpeg", 
    res= 300,  height= 10, width=20, unit="cm") 
dev.off()

