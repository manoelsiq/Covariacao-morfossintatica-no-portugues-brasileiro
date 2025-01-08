library(factoextra)
library(cluster)
library(readxl)
library(tidyverse)

# Carregar o dataframe
dados <- read_excel("dados.xlsx")
dados <- dados %>% mutate_if(is.character, as.factor) # ler dados como factor

dados <- dados %>%
  rename(ART = ausencia,
         cli2PS = te,
         pos2PS = seu,
         pro2PS = voce)


# Normalizar as variáveis numéricas 
dados_num <- dados[,8:11] # Seleciona apenas as variáveis numéricas
dados_num <- scale(dados_num) # Dimensiosa as variáveis numéricas

# Combina as variáveis numéricas normalizadas com as sociais
dados_completos <- cbind(dados_num, dados[,4:7])

# Colocar os nomes do índice com base na coluna informante, para melhor visualização do gráfico
rownames(dados_completos) <- dados$informante2 

# Número de clusters versus a soma total dos quadrados internos
fviz_nbclust(dados_num, pam, method = "wss") # aqui são 3

# Realiza a análise de cluster k-medoids
km <- pam(dados_completos, k = 3) 

# Adiciona o cluster atribuído ao conjunto de dados
dados$cluster2 <- km$clustering
dados$cluster2 <- factor(dados$cluster2, levels = c("1", "2", "3"))

# Salvar data
library(openxlsx)
write.xlsx(dados, "C:/Users/manoe/OneDrive/Área de Trabalho/Doutorado/Tese/Novos dados/dados.xlsx", rowNames = FALSE)


# Ver
fviz_cluster(km, data = dados_completos, 
             repel = TRUE, 
             labelsize = 6, 
             pointsize = 1, 
             show.clust.cent = TRUE, 
             main = FALSE,
             palette = "jco",
             ggtheme = theme_minimal(),
             ellipse.type = "convex") 

# Salvar plot
png(filename= "Cluster DS.jpeg", 
    res= 300,  height= 20, width=25, unit="cm") 
dev.off()

# Análise das distribuições por grupo

library(reshape2)

cluster = dados %>% 
  select(ART, pro2PS, cli2PS, pos2PS, cluster2)
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

png(filename= "Grupos DS.jpeg", 
    res= 300,  height= 15, width=20, unit="cm") 
dev.off()

# Gráfico 2 (com jitters)
ggplot(data = df.m, aes(x=variable, y=value, fill = variable)) +
  geom_violin(trim=FALSE) + 
  geom_boxplot() +
  geom_point(position=position_jitter(width=0.1), alpha=0.4, size = 1) +  # Adicionando pontos com jitter
  facet_wrap( ~ Group, ncol = 4) +
  xlab(label = " ") + ylab(label = NULL) +
  scale_fill_manual(values = royal1_palette) +
  theme(legend.position="bottom")

png(filename= "Grupos DS (jitters).jpeg", 
    res= 300,  height= 15, width=20, unit="cm") 
dev.off()
