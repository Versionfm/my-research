rm(list = ls())
library(readr)
library(readxl)
library(dplyr)       # manipulação de dados
library(tidyr)       # tratamento de missing e tidying
library(stringr)     # operações de texto
library(forcats)
library(cluster)
library(ggradar)
library(dplyr)
library(scales)
library(patchwork)
library(fmsb)
library(ggplot2)


dfc <- read_excel("dados_codificados (2).xlsx")

dfc <- subset(dfc, select = -c(PR15))



###==========================###
###Normalização das variáveis###
###==========================###

normalize_minmax <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

dfc_norm <- as.data.frame(lapply(dfc, normalize_minmax))
dfc_norm <- dplyr::select(
  dfc_norm,
  -c(DEM1_num, DEM2_num, DEM3_num, DEM4_num, Q1_num)
)




###========####
###Outliers####
###========####
apply(dfc_norm, 2, function(x) sum(x < 0.05 | x > 0.95))

extremos <- apply(dfc_norm, 1, function(x) sum(x < 0.05 | x > 0.95))
table(extremos)

outliers_ids <- which(extremos >= 21)
length(outliers_ids)  # 42 outliers

dfc_sem_outliers <- dfc_norm[-outliers_ids, ]

boxplot(dfc_sem_outliers, las = 2, main = "Distribuição sem outliers")
outliers_ids <- which(extremos >= 21)
df_outliers <- dfc_norm[outliers_ids, ]


###Análise das caracteristicas dos Outliers
length(outliers_ids)  # já confirmado: 42
summary(df_outliers)
medias_outliers <- colMeans(df_outliers, na.rm = TRUE)
round(medias_outliers, 2)

df_inliers <- dfc_norm[-outliers_ids, ]
medias_inliers <- colMeans(df_inliers, na.rm = TRUE)

comparacao <- data.frame(
  Variável = names(df_outliers),
  Média_Outliers = round(medias_outliers, 2),
  Média_Não_Outliers = round(medias_inliers[names(df_outliers)], 2)
)

extremos_outliers <- apply(df_outliers, 1, function(x) mean(x == 0 | x == 1))
summary(extremos_outliers)

# Ver as variáveis com maiores diferenças
comparacao$Diferença <- abs(comparacao$Média_Outliers - comparacao$Média_Não_Outliers)
comparacao <- comparacao[order(-comparacao$Diferença), ]
#View(comparacao)



###=====================###
###Calcular as Distância###
###=====================###

dist_matrix <- dist(dfc_sem_outliers, method = "euclidean")^2  # Ward usa ao quadrado


###====================###
###Metodo de Clustering###
###====================###

###Dendograma
hc <- hclust(dist_matrix, method = "ward.D2")

plot(hc, labels = FALSE, hang = -1, main = "Dendrograma - Método de Ward")

###Metodo do cotovelo
set.seed(123)
k.max <- min(15, nrow(dfc_norm) - 1)

wss <- sapply(1:k.max, function(k) {
  kmeans(dfc_sem_outliers, centers = k, nstart = 25)$tot.withinss
})

sum(!is.finite(as.matrix(dfc_norm)))
length(wss)


plot(1:15, wss, type = "b", pch = 19,
     xlab = "Número de Clusters (k)",
     ylab = "WCSS",
     main = "Método do Cotovelo (sem outliers)")

###Decisão: aplicar para k = 3, 4, 5
###Tanto o dendograma como o metodo do cotovelo apontam para 3 clusters

###=======###
###K-Means###
###=======###

set.seed(123)

k3 <- kmeans(dfc_sem_outliers, centers = 3, nstart = 25)
k4 <- kmeans(dfc_sem_outliers, centers = 4, nstart = 25)
k5 <- kmeans(dfc_sem_outliers, centers = 5, nstart = 25)

table(k3$cluster)
table(k4$cluster)
table(k5$cluster)

dfc_sem_outliers$cluster3 <- k3$cluster
dfc_sem_outliers$cluster4 <- k4$cluster
dfc_sem_outliers$cluster5 <- k5$cluster


###==================================####
###Avaliação da qualidade dos Clusters###
###===================================###

library(cluster)

# Matriz de distâncias
dist_matrix <- dist(dfc_sem_outliers)

# Silhueta para k = 3
sil3 <- silhouette(k3$cluster, dist_matrix)
mean_sil3 <- mean(sil3[, 3])

# Silhueta para k = 4
sil4 <- silhouette(k4$cluster, dist_matrix)
mean_sil4 <- mean(sil4[, 3])

# Silhueta para k = 5
sil5 <- silhouette(k5$cluster, dist_matrix)
mean_sil5 <- mean(sil5[, 3])

cat("Silhueta média para k = 3:", round(mean_sil3, 3), "\n")
cat("Silhueta média para k = 4:", round(mean_sil4, 3), "\n")
cat("Silhueta média para k = 5:", round(mean_sil5, 3), "\n")

###5 clusters revelam ter a melhor silhueta, portanto prosseguimos com esse k


###Os mesmos 4 individuos fazem uma cluster tanto em k = 4 como em k = 5, o que pode ser instavel e nao representativo
which(k4$cluster == which.min(table(k4$cluster)))
which(k5$cluster == which.min(table(k5$cluster)))

###Avaliação pelo R^2###
# Selecionar só as variáveis usadas no clustering
variaveis_input <- setdiff(names(dfc_sem_outliers), c("cluster3", "cluster4", "cluster5"))
df_numericas <- dfc_sem_outliers[, variaveis_input]

# Verificar que todas são numéricas
df_numericas <- df_numericas[, sapply(df_numericas, is.numeric)]

# Calcular SS_total corretamente
SS_total <- sum(scale(df_numericas, center = TRUE, scale = FALSE)^2)

# kmeans sobre as mesmas colunas
set.seed(123)
k3 <- kmeans(df_numericas, centers = 3, nstart = 25)
k4 <- kmeans(df_numericas, centers = 4, nstart = 25)
k5 <- kmeans(df_numericas, centers = 5, nstart = 25)

# R2 global
R2_k3 <- 1 - k3$tot.withinss / SS_total
R2_k4 <- 1 - k4$tot.withinss / SS_total
R2_k5 <- 1 - k5$tot.withinss / SS_total

# Resultado
cat("R² para k = 3:", round(R2_k3, 3), "\n")
cat("R² para k = 4:", round(R2_k4, 3), "\n")
cat("R² para k = 5:", round(R2_k5, 3), "\n")



###Critério BIC
calc_bic_kmeans <- function(kmeans_model, data) {
  n <- nrow(data)
  m <- ncol(data)
  k <- nrow(kmeans_model$centers)
  wcss <- kmeans_model$tot.withinss
  
  bic <- wcss + (log(n) * m * k) / 2
  return(bic)
}

bic_k3 <- calc_bic_kmeans(k3, df_numericas)
bic_k4 <- calc_bic_kmeans(k4, df_numericas)
bic_k5 <- calc_bic_kmeans(k5, df_numericas)

cat("BIC para k = 3:", round(bic_k3, 2), "\n")
cat("BIC para k = 4:", round(bic_k4, 2), "\n")
cat("BIC para k = 5:", round(bic_k5, 2), "\n")

###BIC aponta para 3 

###=============###
###Interpretação###
###=============###

dfc_sem_outliers$cluster3 <- as.factor(dfc_sem_outliers$cluster3)
dfc_temp <- dfc_sem_outliers[, !(names(dfc_sem_outliers) %in% c("cluster5", "cluster4"))]
dfc_temp5 <- dfc_sem_outliers[, !(names(dfc_sem_outliers) %in% c("cluster3", "cluster4"))]



# Médias
medias3 <- aggregate(. ~ cluster3, data = dfc_temp, FUN = mean)
medias5 <- aggregate(. ~ cluster5, data = dfc_temp5, FUN = mean)

# Desvios padrão
sds <- aggregate(. ~ cluster3, data = dfc_temp, FUN = sd)

View(sds)
View(medias5)


names(dfc_sem_outliers)


# Selecionar variáveis de interesse
variaveis <- c("AS1", "AS2", "PR2", "PR4", "PR9", "cluster5")
# Subset para análise
df_sub <- dfc_sem_outliers[, variaveis]

# Criar perfil por cluster
perfil <- df_sub %>%
  group_by(cluster5) %>%
  summarise(
    n = n(),
    `Média de conhecimento IA (AS1)` = round(mean(AS1, na.rm=TRUE), 2),
    `% Já ouviu IA generativa (AS2)` = round(mean(AS2, na.rm=TRUE) * 100, 1),
    `Média perceção de riscos (PR9)` = round(mean(PR9, na.rm=TRUE), 2),
    `Média perceção de benefícios (PR4)` = round(mean(PR4, na.rm=TRUE), 2),
    `% Concorda 'Facilita a vida' (PR2)` = round(mean(PR2, na.rm=TRUE) * 100, 1)
  )

#write.xlsx(dfc_sem_outliers, file = "dados_com_clusters")


###========================###
###Variáveis Significativas###
###========================###


###Ordinais###

#Função para o teste
kt <- function(var) kruskal.test(as.formula(paste(var, "~ cluster5")), data = dfc_sem_outliers)

kt("AS1") #s
kt("AS2") #s
kt("AS4") #ns
kt("AS5") #s
kt("AS6") #s
kt("AS7") #s
kt("AS8") #s
kt("AS9") #s
kt("PR2") #S
kt("PR3") #S
kt("PR4") #S
kt("PR5") #s
kt("PR6") #S
kt("PR7") #S
kt("PR8") #S
kt("PR9") #S
kt("PR10") #S
kt("PR11") #S
kt("PR12") #S
kt("PR13") #S
kt("PR14") #S

###Binárias###

#AS9 
tabela_as9 <- table(dfc_sem_outliers$AS9, dfc_sem_outliers$cluster5)
chisq.test(tabela_as9)  
fisher.test(tabela_as9)
#Significativa

#PR1_muitos_riscos
tabela_PR1MR <- table(dfc_sem_outliers$PR1_muitos_riscos, dfc_sem_outliers$cluster5)
chisq.test(tabela_PR1MR)  
fisher.test(tabela_PR1MR)
#Significativa

#PR1_poucos_riscos
tabela_PR1PR <- table(dfc_sem_outliers$PR1_poucos_riscos, dfc_sem_outliers$cluster5)
chisq.test(tabela_PR1PR)  
fisher.test(tabela_PR1PR)
#Significativa

#PR1_muitos_beneficios
tabela_PR1MB <- table(dfc_sem_outliers$PR1_muitos_beneficios, dfc_sem_outliers$cluster5)
chisq.test(tabela_PR1MB)  
fisher.test(tabela_PR1MB)
#Significativa

#PR1_poucos_beneficios
tabela_PR1PB <- table(dfc_sem_outliers$PR1_poucos_beneficios, dfc_sem_outliers$cluster5)
chisq.test(tabela_PR1PB)  
fisher.test(tabela_PR1PB)
#Não Significativa

#PR1_Neutros
tabela_PR1N <- table(dfc_sem_outliers$PR1_neutro, dfc_sem_outliers$cluster5)
chisq.test(tabela_PR1N)  
fisher.test(tabela_PR1N)
#Não Significativa

###Tabela Dos Clusters###
tabela <-dfc_sem_outliers %>%
  group_by(cluster5) %>%
  summarise(across(c(AS1, AS2, AS5:AS8, PR2:PR14, PR1_muitos_riscos, PR1_poucos_riscos, PR1_muitos_beneficios), mean, na.rm = TRUE))

dftabela <- as.data.frame(tabela)

#write.csv(dftabela, "tabela_clusters.csv", row.names = FALSE)

###=======###
###Indices###
###=======###
dfc_indices <- dfc_sem_outliers
dfc_indices <- dfc_indices %>%
  mutate(
    AS6_invertida = 1 - AS6,
    ind_conhecimento = rowMeans(across(c(AS1, AS2, AS8)), na.rm = TRUE),
    ind_uso          = rowMeans(across(c(AS4)), na.rm = TRUE),
    ind_IA_paga      = rowMeans(across(c(AS9)), na.rm = TRUE),
    ind_confiança    = rowMeans(across(c(AS5, AS6_invertida, AS7)), na.rm = TRUE),
    ind_risco        = rowMeans(across(c(PR2, PR3, PR4, PR5, PR6, PR7)), na.rm = TRUE),
    ind_regulacao    = rowMeans(across(c(PR8, PR9, PR10, PR11, PR12, PR13)), na.rm = TRUE),
    ind_beneficio    = rowMeans(across(c(PR1_muitos_beneficios, PR14)), na.rm = TRUE)
  )


# Média por cluster para os índices
df_media_indices <- dfc_indices %>%
  group_by(cluster5) %>%
  summarise(across(starts_with("ind_"), mean, na.rm = TRUE))
df_media_indices3 <- dfc_indices %>%
  group_by(cluster3) %>%
  summarise(across(starts_with("ind_"), mean, na.rm = TRUE))


####==============####
####Gráficos radar####
####==============####
# Dados
df_radar3 <- data.frame(
  Cluster = c("Cluster 1", "Cluster 2", "Cluster 3"),
  Conhecimento = c(0.65, 0.66, 0.61),
  Uso         = c(0.72, 0.84, 0.75),
  Confiança   = c(0.55, 0.60, 0.56),
  Risco       = c(0.70, 0.453, 0.775),
  Regulação   = c(0.93, 0.88, 0.97),
  FerramentasPagas = c(0.54, 0.66, 0.53),
  Benefício   = c(0.43, 0.88, 0.97)
)



# Dados
df_radar <- data.frame(
  Cluster = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5"),
  Conhecimento = c(0.657, 0.537, 0.699, 0.382, 0.709),
  Uso         = c(0.708, 0.727, 0.89, 0.563, 0.78),
  Confiança   = c(0.559, 0.551, 0.610, 0.479, 0.583),
  Risco       = c(0.705, 0.861, 0.463, 0.458, 0.663),
  Regulação   = c(0.951, 0.987, 0.960, 0.313, 0.957),
  FerramentasPagas = c(0.542, 0.47, 0.70, 0.375, 0.60),
  Benefício   = c(0.438,0.977, 0.950, 0.313, 0.960)
)

# ggradar exige que a 1ª coluna seja rownames
df_radar_ready <- df_radar %>%
  rename(group = Cluster)
df_radar_ready3 <- df_radar3 %>%
  rename(group = Cluster)
df_radar$Cluster <- recode(df_radar$Cluster,
                           "Cluster 1" = "Utilizadores Céticos",
                           "Cluster 2" = "Utilizadores Moderados Preocupados",
                           "Cluster 3" = "Utilizadores Informados Confiantes",
                           "Cluster 4" = "Alheios à IA",
                           "Cluster 5" = "Utilizadores Informados Prudentes"
)
names(df_radar)[names(df_radar) == "FerramentasPagas"] <- "F.IA Pagas"
names(df_radar3)[names(df_radar3) == "FerramentasPagas"] <- "F.IA Pagas"
#edit(ggradar::ggradar)  #Usado para retirar as percentagens

# Escalar valores de 0 a 1
plot_ggradar_cluster3 <- function(cluster_nome, cor_hex) {
  ggradar(
    df_radar3 %>% filter(Cluster == cluster_nome),
    grid.min = 0, grid.mid = 0.5, grid.max = 1,
    group.line.width = 1.2,
    group.point.size = 3,
    background.circle.colour = "white",
    gridline.min.linetype = "dotted",
    gridline.mid.linetype = "dotted",
    gridline.max.linetype = "dotted",
    axis.label.size = 4,
    group.colours = cor_hex,
    legend.position = "none",
    plot.title = paste(cluster_nome),
    label.gridline.min = FALSE,
    label.gridline.mid = FALSE,
    label.gridline.max = FALSE
  )
}

# Escalar valores de 0 a 1
plot_ggradar_cluster <- function(cluster_nome, cor_hex) {
  ggradar(
    df_radar %>% filter(Cluster == cluster_nome),
    grid.min = 0, grid.mid = 0.5, grid.max = 1,
    group.line.width = 1.2,
    group.point.size = 3,
    background.circle.colour = "white",
    gridline.min.linetype = "dotted",
    gridline.mid.linetype = "dotted",
    gridline.max.linetype = "dotted",
    axis.label.size = 4,
    group.colours = cor_hex,
    legend.position = "none",
    plot.title = paste(cluster_nome),
    label.gridline.min = FALSE,
    label.gridline.mid = FALSE,
    label.gridline.max = FALSE
  )
}



cores <- c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854")

p1 <- plot_ggradar_cluster("Utilizadores Céticos", cores[1])
p2 <- plot_ggradar_cluster("Utilizadores Moderados Preocupados", cores[2])
p3 <- plot_ggradar_cluster("Utilizadores Informados Confiantes", cores[3])
p4 <- plot_ggradar_cluster("Alheios à IA", cores[4])
p5 <- plot_ggradar_cluster("Utilizadores Informados Prudentes", cores[5])


###Para k = 3
#cores <- c("#66c2a5", "#fc8d62", "#8da0cb")

#p1 <- plot_ggradar_cluster3("Cluster 1", cores[1])
#p2 <- plot_ggradar_cluster3("Cluster 2", cores[2])
#p3 <- plot_ggradar_cluster3("Cluster 4", cores[3])
# Junta os gráficos em grelha 3(com um espaçador final)
cluster_sizes <- df_fatores %>%
  count(cluster5) %>%
  arrange(desc(n))
print(cluster_sizes)

#(p1 + p2 + p3)

# Junta os gráficos em grelha 3 x 2 (com um espaçador final)
(p4 + p1 + p5) /
  (p3 + p2 + patchwork::plot_spacer())

###======================================###
###Recuperação das variáveis Demográficas###
###======================================###


dfc$ID_original <- 1:nrow(dfc)

outliers_ids <- which(extremos >= 21)  # Estes são os índices do dfc_norm

outliers_IDs <- dfc$ID_original[outliers_ids]

# Guarda apenas as colunas demográficas + ID
df_demograficos <- dfc[, c("ID_original", "DEM1_num", "DEM2_num", "DEM3_num", "DEM4_num", "Q1_num")]

# Filtra os que **não** são outliers
df_demograficos_sem_outliers <- df_demograficos[!(df_demograficos$ID_original %in% outliers_IDs), ]


dfc_demograficos_cat <- df_demograficos


dfc_demograficos_cat$Q1_cat <- factor(
  dfc_demograficos_cat$Q1_num,
  levels = c(1, 2, 3, 4, 5, 6),
  labels = c(
    "Empregado", "Conta própria", "Docente/Investigador",
    "Estudante", "Desempregado", "Inativo"
  )
)

dfc_demograficos_cat$DEM2_cat <- factor(df_demograficos$DEM2_num,
                                    levels = c(1, 2),
                                    labels = c("Masculino", "Feminino"))

# DEM1 - Faixa etária (num para cat)
dfc_demograficos_cat$DEM1_cat <- factor(df_demograficos$DEM1_num,
                                    levels = c(1, 2, 3, 4),
                                    labels = c("18 - 34", "35 - 44", "44 - 54", "55 ou mais"))

# DEM3 - Região de residência (num para cat)
dfc_demograficos_cat$DEM3_cat <- factor(df_demograficos$DEM3_num,
                                    levels = c(1, 2, 3),
                                    labels = c("Lisboa", "Alentejo", "Resto de Portugal"))

# DEM4 - Nível de educação (num para cat)
dfc_demograficos_cat$DEM4_cat <- factor(df_demograficos$DEM4_num,
                                    levels = c(1, 2, 3, 4),
                                    labels = c("Secundário", "Licenciatura", "Pós-graduação/Mestrado/Douturamento", "Outro (formação profissional, etc)"))


# Remover os mesmos outliers do data frame demográfico
dfc_demograficos_cat_sem_outliers <- dfc_demograficos_cat[-outliers_ids, ]


nrow(dfc_demograficos_cat_sem_outliers)  # Deve dar 111
nrow(dfc_sem_outliers)                  

#combinar com os clusters
df_clusters <- data.frame(cluster = dfc_sem_outliers$cluster5,
                          dfc_demograficos_cat_sem_outliers)



###=====================================###
###Cruzamento com variáveis Demográficas###
###=====================================###


###Verificação da Idade
table_idade <- table(df_clusters$cluster, df_clusters$DEM1_cat)
table_idade
chisq.test(table_idade)  # Teste de independência
fisher.test(table_idade)
#Significativa


#Verificação por genero
table_genero <- table(df_clusters$cluster, df_clusters$DEM2_cat)
table_genero
chisq.test(table_genero)  # Teste de independência
fisher.test(table_genero)
#N significativa


#Verificação por Região
table_regiao <- table(df_clusters$cluster, df_clusters$DEM3_cat)
table_regiao
chisq.test(table_regiao)  # Teste de independência
fisher.test(table_regiao)
#N significativa


#Verificação Nivel de Habilitações
table_nh <- table(df_clusters$cluster, df_clusters$DEM4_cat)
table_nh
chisq.test(table_nh)  # Teste de independência
fisher.test(table_nh)
#Significativa

#Verificar para a situação profissional
table_sp <- table(df_clusters$cluster, df_clusters$Q1_cat)
table_sp
chisq.test(table_sp)  
#Não significativo



# Supondo que tens um dataframe assim:
idade_cluster <- data.frame(
  Cluster = rep(c("1", "2", "3", "4", "5"), each = 4),
  Faixa_Etaria = rep(c("18-34", "35–44", "45–54", "55+"), times = 5),
  Frequência = c(6, 9, 5, 4, 13, 5, 8, 7, 12, 2, 9, 2, 0, 1, 0, 3, 12, 6, 4, 3)
)

ggplot(idade_cluster, aes(x = Cluster, y = Frequência, fill = Faixa_Etaria)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribuição etária por cluster",
       x = "Cluster",
       y = "N.º de Respondentes") +
  theme_minimal()

#write.csv(df_clusters, file = "dados_clusters.csv", row.names = FALSE, fileEncoding = "UTF-8")


#Gráfico de barra das habilitações
habilitacoes_cluster <- data.frame(
  Cluster = rep(c("1", "2", "3", "4", "5"), each = 4),
  Habilitacoes = rep(c("Secundário", "Outro", "Licenciatura", "Pós-graduação+"), times = 5),
  Frequência = c(3, 1, 9, 11, 8, 0, 11, 14, 1, 0, 4, 20, 3, 0, 0, 1, 7, 1, 3, 14)
)

ggplot(habilitacoes_cluster, aes(x = Cluster, y = Frequência, fill = Habilitacoes)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribuição de habilitações por cluster",
       x = "Cluster",
       y = "N.º de Respondentes") +
  theme_minimal()


####===================================================####
####Clusters com os Outliers (Complemento ao Relatório)####
####===================================================####

df_com_outliers <- dfc_norm

## ================================
## 2) Distâncias e dendrograma
## ================================
dist_full_sq <- dist(df_com_outliers, method = "euclidean")^2
hc_full <- hclust(dist_full_sq, method = "ward.D2")

plot(hc_full, labels = FALSE, hang = -1,
     main = "Dendrograma (COM outliers) - Ward.D2")


## ================================
## 3) K-means para k = 3,4,5
## ================================
set.seed(123)
k3_full <- kmeans(df_com_outliers, centers = 3, nstart = 25)
k4_full <- kmeans(df_com_outliers, centers = 4, nstart = 25)
k5_full <- kmeans(df_com_outliers, centers = 5, nstart = 25)

cat("Tamanhos k=3 (com outliers):", table(k3_full$cluster), "\n")
cat("Tamanhos k=4 (com outliers):", table(k4_full$cluster), "\n")
cat("Tamanhos k=5 (com outliers):", table(k5_full$cluster), "\n")

## Flag de “grupos singulares” (e.g., clusters muito pequenos)
flag_clusters_pequenos <- function(model, min_size = 5) {
  tab <- table(model$cluster)
  which(tab <= min_size)
}
cat("Clusters muito pequenos (k=4):", flag_clusters_pequenos(k4_full), "\n")
cat("Clusters muito pequenos (k=5):", flag_clusters_pequenos(k5_full), "\n")

## ================================
## 4) Silhueta, R² e BIC
## ================================
library(cluster)

# Silhuetas
sil3_full <- silhouette(k3_full$cluster, dist(df_com_outliers))
sil4_full <- silhouette(k4_full$cluster, dist(df_com_outliers))
sil5_full <- silhouette(k5_full$cluster, dist(df_com_outliers))

mean_sil3_full <- mean(sil3_full[, 3])
mean_sil4_full <- mean(sil4_full[, 3])
mean_sil5_full <- mean(sil5_full[, 3])

cat("Silhueta média (COM outliers) k=3:", round(mean_sil3_full, 3), "\n")
cat("Silhueta média (COM outliers) k=4:", round(mean_sil4_full, 3), "\n")
cat("Silhueta média (COM outliers) k=5:", round(mean_sil5_full, 3), "\n")

# R²
df_num_full <- df_com_outliers[, sapply(df_com_outliers, is.numeric)]
SS_total_full <- sum(scale(df_num_full, center = TRUE, scale = FALSE)^2)

R2_k3_full <- 1 - k3_full$tot.withinss / SS_total_full
R2_k4_full <- 1 - k4_full$tot.withinss / SS_total_full
R2_k5_full <- 1 - k5_full$tot.withinss / SS_total_full

cat("R² (COM outliers) k=3:", round(R2_k3_full, 3), "\n")
cat("R² (COM outliers) k=4:", round(R2_k4_full, 3), "\n")
cat("R² (COM outliers) k=5:", round(R2_k5_full, 3), "\n")

# BIC (critério simples e coerente com o que usaste)
calc_bic_kmeans <- function(kmeans_model, data) {
  n <- nrow(data); m <- ncol(data); k <- nrow(kmeans_model$centers)
  wcss <- kmeans_model$tot.withinss
  bic <- wcss + (log(n) * m * k) / 2
  return(bic)
}
bic_k3_full <- calc_bic_kmeans(k3_full, df_num_full)
bic_k4_full <- calc_bic_kmeans(k4_full, df_num_full)
bic_k5_full <- calc_bic_kmeans(k5_full, df_num_full)

cat("BIC (COM outliers) k=3:", round(bic_k3_full, 2), "\n")
cat("BIC (COM outliers) k=4:", round(bic_k4_full, 2), "\n")
cat("BIC (COM outliers) k=5:", round(bic_k5_full, 2), "\n")


## ================================
## 5) Comparação com/sem outliers
## ================================
comparacao_metricas <- data.frame(
  k = c(3,4,5),
  Silhueta_sem = c(round(mean_sil3,3), round(mean_sil4,3), round(mean_sil5,3)),
  Silhueta_com = c(round(mean_sil3_full,3), round(mean_sil4_full,3), round(mean_sil5_full,3)),
  R2_sem       = c(round(R2_k3,3), round(R2_k4,3), round(R2_k5,3)),
  R2_com       = c(round(R2_k3_full,3), round(R2_k4_full,3), round(R2_k5_full,3)),
  BIC_sem      = c(round(bic_k3,2), round(bic_k4,2), round(bic_k5,2)),
  BIC_com      = c(round(bic_k3_full,2), round(bic_k4_full,2), round(bic_k5_full,2))
)
print(comparacao_metricas)

## ================================
## 6) Onde caem os 42 outliers?
## ================================
# (Mapa: ID do respondente -> cluster com outliers)
ids_todos <- 1:nrow(df_com_outliers)
clusters_k5_full <- k5_full$cluster  
mapa_outliers <- data.frame(
  ID = ids_todos[outliers_ids],
  Cluster_k5_com_outliers = clusters_k5_full[outliers_ids]
)
head(mapa_outliers)
table(mapa_outliers$Cluster_k5_com_outliers)

## ================================
## 7) Exportar para anexos
## ================================
# CSV com as métricas comparadas
write.csv(comparacao_metricas, "comparacao_metricas_clusters_com_vs_sem_outliers.csv", row.names = FALSE)


###========================###
###Correlação das Variáveis###
###========================###

library(dplyr)
library(correlation)
library(ggcorrplot)
library(ppcor)
library(car)
library(GGally)

# 0) Selecionar apenas variáveis usadas no clustering (sem demográficas/labels)
vars_corr <- dfc_sem_outliers %>%
  dplyr::select(-cluster3, -cluster4, -cluster5)3

# Correlação com auto-selecção de método por par (Pearson/Spearman/biserial/polychoric)
# O pacote {correlation} escolhe automaticamente o coeficiente adequado par-a-par:
# - Pearson para contínuas ~normais
# - Spearman para ordinais/monótonas
# - point-biserial para binária vs contínua
# - polychoric para ordinais/binárias (quando aplicável)
# Aplica ainda CIs e ajusta p-values (BH).
corr_auto <- correlation::correlation(
  vars_corr,
  method = "auto",            # escolhe o coeficiente adequado a cada par
  bayesian = FALSE,
  ci = 0.95,
  p_adjust = "BH"             # Benjamini–Hochberg para múltiplas comparações
)

# Tabela arrumada com top correlações significativas
top_corr <- corr_auto %>%
  as.data.frame() %>%
  dplyr::filter(p < 0.05) %>%
  dplyr::arrange(dplyr::desc(abs(r))) %>%
  dplyr::select(Parameter1, Parameter2, r, CI_low, CI_high, p)


library(ggcorrplot)
# Gráfico com as correlações
mat_corr <- cor(vars_corr,
                use = "pairwise.complete.obs", 
                method = "spearman")
ggcorrplot(mat_corr,
           hc.order = TRUE,
           type = "lower", 
           lab = TRUE)

# Converter a matriz em formato longo
corr_long <- as.data.frame(as.table(mat_corr)) %>%
  dplyr::filter(Var1 != Var2) %>%
  dplyr::mutate(abs_corr = abs(Freq)) %>%
  dplyr::arrange(desc(abs_corr))

head(corr_long, 15)  # Top 15 correlações



