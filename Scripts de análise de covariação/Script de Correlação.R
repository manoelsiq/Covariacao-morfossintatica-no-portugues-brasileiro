library(corrplot)
library(Hmisc)
library(tidyverse)
library(GGally)
library(readxl)
library(PerformanceAnalytics)


# Carregar planilha


dados <- read_excel("dados2.xlsx")

# Renomear variáveis
dados <- dados %>%
  rename(ART = art,
         cli2PS = cli,
         pos2PS = pos,
         pro2PS = voce)

# Selecionar variáveis
dados = dados %>% 
  select(informante,
         informante2,
         amostra,
         deslocamento,
         tempo,
         genero,
         idade,
         ART,
         pro2PS,
         cli2PS,
         pos2PS)

# Observar se a distribuição é normal
shapiro.test(dados$ART) # única normal
shapiro.test(dados$pro2PS)
shapiro.test(dados$cli2PS)
shapiro.test(dados$pos2PS)

# Criar a matriz de correlação com teste estatístico
correlacao1 <- rcorr(as.matrix(dados[, c("ART", "pro2PS", "cli2PS", "pos2PS")]), type = "spearman")

# Exibir a matriz de correlação
correlacao1$r # ver o valor de r
correlacao1$P # ver o p-valor

# Visualização gráfica

## Gráfico 1

# Selecionar as colunas com as variáveis de interesse
dados_cor <- dados[, c("ART", "pro2PS", "cli2PS", "pos2PS")]

# Definir a função 
panel.cor <- function(x, y, digits = 4, prefix = "", use = "pairwise.complete.obs", cex.cor = 1.2, ...)
{
  r <- cor(x, y, use = use)
  p <- cor.test(x, y, use = use)$p.value
  txt <- format(c(r, p), digits = digits)
  txt <- paste(prefix, txt, sep = "\n")
  text(0.5, 0.5, txt[1], cex = cex.cor * abs(r), ...)
  text(0.5, 0.4, txt[2], cex = cex.cor * abs(r), ...)
  points(x, y, ...)
  if (length(x) == 3) {
    xx <- seq(min(x), max(x), length.out = 100)
    yy <- seq(min(y), max(y), length.out = 100)
    zz <- outer(xx, yy, function(x, y) cor(c(x, y, x), c(y, x, y), use = use))
    contour(xx, yy, zz, levels = c(-1, 0, 1), drawlabels = FALSE, add = TRUE)
    text(mean(x), mean(y), format.pval(p), cex = 1.2)
  }
}


# Matriz de correlação com gráficos de dispersão, coeficientes de correlação e valores p
chart.Correlation(dados_cor, histogram = TRUE, pch = 19, 
                  main = "Matriz de Correlação", 
                  col = c("#7F00FF","#1EBEFF","#FFC200"),
                  method = "spearman")


# Por amostra

# 2019
nove = dados %>% 
  filter(amostra == "Deslocamentos 2019")

nove1 <- rcorr(as.matrix(nove[, c("ART", "pro2PS", "cli2PS", "pos2PS")]), type = "spearman")

# Exibir a matriz de correlação
nove1$r # ver o valor de r
nove1$P # ver o p-valor

nove_cor <- nove[, c("ART", "pro2PS", "cli2PS", "pos2PS")]


chart.Correlation(nove_cor, histogram = TRUE, pch = 19, 
                  main = "Matriz de Correlação", 
                  col = c("#7F00FF","#1EBEFF","#FFC200"),
                  method = "spearman")

# 2020
vinte = dados %>% 
  filter(amostra == "Deslocamentos 2020")

vinte1 <- rcorr(as.matrix(vinte[, c("ART", "pro2PS", "cli2PS", "pos2PS")]), type = "spearman")

# Exibir a matriz de correlação
vinte1$r # ver o valor de r
vinte1$P # ver o p-valor


vinte_cor <- vinte[, c("ART", "pro2PS", "cli2PS", "pos2PS")]


chart.Correlation(vinte_cor, histogram = TRUE, pch = 19, 
                  main = "Matriz de Correlação", 
                  col = c("#7F00FF","#1EBEFF","#FFC200"),
                  method = "spearman")

# 2023
corpo = dados %>% 
  filter(amostra == "Linguagem Corporificada 2023")

corpo1 <- rcorr(as.matrix(corpo[, c("ART", "pro2PS", "cli2PS", "pos2PS")]), type = "spearman")

# Exibir a matriz de correlação
corpo1$r # ver o valor de r
corpo1$P # ver o p-valor

corpo_cor <- corpo[, c("ART", "pro2PS", "cli2PS", "pos2PS")]


chart.Correlation(corpo_cor, histogram = TRUE, pch = 19, 
                  main = "Matriz de Correlação", 
                  col = c("#7F00FF","#1EBEFF","#FFC200"),
                  method = "spearman")

# Observar por Deslocamento

# Apenas Sergipe
se = dados %>% 
  filter(deslocamento %in% c("Deslocamento 1", "Deslocamento 2", "Deslocamento 3"))


se1 <- rcorr(as.matrix(se[, c("ART", "pro2PS", "cli2PS", "pos2PS")]), type = "spearman")

# Exibir a matriz de correlação
se1$r # ver o valor de r
se1$P # ver o p-valor


se_cor <- se[, c("ART", "pro2PS", "cli2PS", "pos2PS")]


chart.Correlation(se_cor, histogram = TRUE, pch = 19, 
                  main = "Matriz de Correlação", 
                  col = c("#7F00FF","#1EBEFF","#FFC200"),
                  method = "spearman")

# Deslocamento 1
desl1 = dados %>% 
  filter(deslocamento == "Deslocamento 1")

desl1_cor <- rcorr(as.matrix(desl1[, c("ART", "pro2PS", "cli2PS", "pos2PS")]), type = "spearman")

# Exibir a matriz de correlação
desl1_cor$r # ver o valor de r
desl1_cor$P # ver o p-valor


desl1_cor <- desl1[, c("ART", "pro2PS", "cli2PS", "pos2PS")]


chart.Correlation(desl1_cor, histogram = TRUE, pch = 19, 
                  main = "Matriz de Correlação", 
                  col = c("#7F00FF","#1EBEFF","#FFC200"),
                  method = "spearman")

# Deslocamento 2
desl2 = dados %>% 
  filter(deslocamento == "Deslocamento 2")

desl2_cor <- rcorr(as.matrix(desl2[, c("ART", "pro2PS", "cli2PS", "pos2PS")]), type = "spearman")

# Exibir a matriz de correlação
desl2_cor$r # ver o valor de r
desl2_cor$P # ver o p-valor


desl2_cor <- desl2[, c("ART", "pro2PS", "cli2PS", "pos2PS")]


chart.Correlation(desl2_cor, histogram = TRUE, pch = 19, 
                  main = "Matriz de Correlação", 
                  col = c("#7F00FF","#1EBEFF","#FFC200"),
                  method = "spearman")

# Deslocamento 3
desl3 = dados %>% 
  filter(deslocamento == "Deslocamento 3")

desl3_cor <- rcorr(as.matrix(desl3[, c("ART", "pro2PS", "cli2PS", "pos2PS")]), type = "spearman")

# Exibir a matriz de correlação
desl3_cor$r # ver o valor de r
desl3_cor$P # ver o p-valor


desl3_cor <- desl3[, c("ART", "pro2PS", "cli2PS", "pos2PS")]


chart.Correlation(desl3_cor, histogram = TRUE, pch = 19, 
                  main = "Matriz de Correlação", 
                  col = c("#7F00FF","#1EBEFF","#FFC200"),
                  method = "spearman")

# Deslocamento 4 e Bahia (removidos os de fora)

desl4 = dados %>% 
  filter(deslocamento %in% c("Deslocamento 4", "Bahia"))

desl4 = desl4 %>% 
  filter(!informante2 %in% c("JES4FF (2019)", 
                             "VIC4MI (2019)", 
                             "ROB4MI (2019)",
                             "MEL (2023)",
                             "LAR19 (2023)",
                             "HEL (2023)"))

desl4_cor <- rcorr(as.matrix(desl4[, c("ART", "pro2PS", "cli2PS", "pos2PS")]), type = "spearman")

# Exibir a matriz de correlação
desl4_cor$r # ver o valor de r
desl4_cor$P # ver o p-valor


desl4_cor <- desl4[, c("ART", "pro2PS", "cli2PS", "pos2PS")]


chart.Correlation(desl4_cor, histogram = TRUE, pch = 19, 
                  main = "Matriz de Correlação", 
                  col = c("#7F00FF","#1EBEFF","#FFC200"),
                  method = "spearman")

# Alagoas

al = dados %>% 
  filter(deslocamento %in% c("Alagoas"))


al_cor <- rcorr(as.matrix(al[, c("ART", "pro2PS", "cli2PS", "pos2PS")]), type = "spearman")

# Exibir a matriz de correlação
al_cor$r # ver o valor de r
al_cor$P # ver o p-valor


al_cor <- al[, c("ART", "pro2PS", "cli2PS", "pos2PS")]


chart.Correlation(al_cor, histogram = TRUE, pch = 19, 
                  main = "Matriz de Correlação", 
                  col = c("#7F00FF","#1EBEFF","#FFC200"),
                  method = "spearman")



png(filename= "Cor AL freq.jpeg", 
    res= 300,  height= 15, width=20, unit="cm") 
dev.off()
