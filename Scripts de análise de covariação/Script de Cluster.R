library(factoextra)
library(cluster)
library(readxl)
library(tidyverse)

# Carregar o dataframe
dados <- read_excel("dados2.xlsx")
dados <- dados %>% mutate_if(is.character, as.factor) 
dados <- dados %>%
  rename(ART = art,
         cli2PS = cli,
         pos2PS = pos,
         pro2PS = voce)

# Escale cada variável para ter uma média de 0 e um desvio padrão de 1
df <- scale(dados[,c("ART", "pro2PS", "cli2PS", "pos2PS")])




# Colocar os nomes do índice com base na coluna informante, para melhor visualização do gráfico
rownames(df) <- dados$informante2 

# Ver quantos grupos há

# Número de clusters versus a soma total dos quadrados internos
fviz_nbclust(df, pam, method = "wss") # aqui são 3


gap_stat <- clusGap(df,
                    FUN = pam,
                    K.max = 10, #max clusters to consider
                    B = 50) #total bootstrapped iterations

fviz_gap_stat(gap_stat)


## Análise de cluster

# Criar o objeto identificado os k-medoids (pam)
kmed <- pam(df, k = 3) # O número designa o tanto de grupo


# Visualizar
kmed

# Adiciona o cluster atribuído ao conjunto de dados
dados$cluster <- kmed$clustering
dados$cluster <- factor(dados$cluster, levels = c("1", "2", "3"))

# Salvar data
library(openxlsx)
write.xlsx(dados, "C:/Users/manoe/OneDrive/Área de Trabalho/Doutorado/Tese/Novos dados/dados.xlsx", rowNames = FALSE)


# Fazer o plot
fviz_cluster(kmed, data = df, 
             repel = TRUE, 
             labelsize = 6, 
             pointsize = 1, 
             show.clust.cent = TRUE, 
             main = FALSE,
             palette = "jco",
             ggtheme = theme_minimal(),
             ellipse.type = "convex") 

# Salvar plot
png(filename= "Cluster DB.jpeg", 
    res= 300,  height= 20, width=25, unit="cm") 
dev.off()

# Análise das distribuições por grupo

library(reshape2)

cluster = dados %>% 
  select(ART, pro2PS, cli2PS, pos2PS, cluster)
colnames(cluster)[5]<-c("Group")

head(cluster)

df.m <- melt(cluster, id.var = "Group")
df.m$Group <- as.character(df.m$Group)

str(df.m)
df.m <- mutate_if(df.m, is.character, as.factor)

### Médias por grupo
df.m %>%
  group_by(Group, variable) %>%
  summarise(
    n = n(),
    mean = mean(value),
    sd = sd(value)
  )

# Medianas por grupo
df.m %>%
  group_by(Group, variable) %>%
  summarise(
    n = n(),
    median = median(value),
    sd = sd(value)
  )

# Fazer gráfico

# Paleta de cores
library(wesanderson)
royal1_palette <- wes_palette("Royal1")

# Gráfico 1
ggplot(data = df.m, aes(x=variable, y=value, fill = variable)) +
  geom_violin(trim=FALSE) + 
  geom_boxplot() +
  facet_wrap( ~ Group, ncol = 4) +
  geom_point(position=position_jitter(width=0.1), alpha=0.4, size = 1) +  # Adicionando pontos com jitter
  xlab(label = " ") + ylab(label = NULL) +
  scale_fill_manual(values = royal1_palette) +
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 2)), 
               position = position_nudge(y = 0.7 * max(df.m$value)), # Empurra o texto para cima
               size = 3, color = "black") +
  theme(legend.position="bottom")   

png(filename= "Grupos DB.jpeg", 
    res= 300,  height= 15, width=20, unit="cm") 
dev.off()

# Gráfico 2 (com jitters)
ggplot(data = df.m, aes(x=variable, y=value, fill = variable)) +
  geom_violin(trim=FALSE) + 
  geom_boxplot() +
  geom_point(position=position_jitter(width=0.1), alpha=0.4, size = 1) +  # Adicionando pontos com jitter
  facet_wrap( ~ Group, ncol = 4) +
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 2)), 
               position = position_dodge(width = 0.75), size = 3, color = "black", vjust = -0.5) +
  xlab(label = " ") + ylab(label = NULL) +
  scale_fill_manual(values = royal1_palette) +
  theme(legend.position="bottom")

png(filename= "Grupos DB (jitters).jpeg", 
    res= 300,  height= 15, width=20, unit="cm") 
dev.off()

# Com mediana
ggplot(data = df.m, aes(x=variable, y=value, fill = variable)) +
  geom_violin(trim=FALSE) + 
  geom_boxplot(width = 0.1, color = "black", lwd = 0.8, outlier.shape = NA) +
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 2)), 
               position = position_dodge(width = 0.75), size = 3, color = "black", vjust = -0.5) +
  facet_wrap( ~ Group, ncol = 4) +
  xlab(label = " ") + ylab(label = NULL) +
  scale_fill_manual(values = royal1_palette) +
  theme(legend.position="bottom")

