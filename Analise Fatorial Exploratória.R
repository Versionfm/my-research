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



###===###
###AFE###
###===###
library(psych)



# 1) Selecionar apenas variáveis originais usadas no clustering
vars_corr <- dfc_sem_outliers %>%
  dplyr::select(-cluster3, -cluster4, -cluster5) %>%
  dplyr::mutate(dplyr::across(everything(), as.numeric))

# 2) Matriz de correlação
mat_corr <- cor(vars_corr, use = "pairwise.complete.obs", method = "spearman")

# 3) Testes de adequação
KMO(mat_corr)                                # Kaiser-Meyer-Olkin
cortest.bartlett(mat_corr, n = nrow(vars_corr))  # Teste de esfericidade de Bartlett

# 4) Scree plot + análise paralela para decidir nº de fatores
fa.parallel(vars_corr, fa = "fa", fm = "ml")  # maximum likelihood


fa_result <- fa(vars_corr, nfactors = 4, rotate = "oblimin", fm = "ml", scores = "regression")
print(fa_result$loadings, cutoff = 0.3, sort = TRUE)
scores <- as.data.frame(fa_result$scores)        # colunas = F1..F4 com sinal consistente

# 1) Scores fatoriais com nomes claros
scores <- as.data.frame(fa_result$scores) %>%
  dplyr::rename(
    Regulacao         = ML1,
    Risco             = ML2,
    Conhecimento_Uso  = ML3,
    Confianca_Atitudes = ML4
  )

# 2) Juntar aos dados originais (com clusters)
df_fatores <- dplyr::bind_cols(dfc_sem_outliers, scores)

# 3) Médias por cluster
df_media_fatores <- df_fatores %>%
  group_by(cluster5) %>%
  summarise(
    Regulacao         = mean(Regulacao, na.rm = TRUE),
    Risco             = mean(Risco, na.rm = TRUE),
    Conhecimento_Uso  = mean(Conhecimento_Uso, na.rm = TRUE),
    Confianca_Atitudes = mean(Confianca_Atitudes, na.rm = TRUE),
    .groups = "drop"
  )

# 4) Re-escalar para [0,1]
rescale01 <- function(x) {
  r <- range(x, na.rm = TRUE)
  if (!is.finite(r[1]) || !is.finite(r[2])) return(rep(0.5, length(x)))
  if (diff(r) == 0) return(rep(0.5, length(x)))
  (x - r[1]) / diff(r)
}

df_media_fatores <- df_media_fatores %>%
  mutate(across(c(Regulacao, Risco, Conhecimento_Uso, Confianca_Atitudes), rescale01)) %>%
  mutate(
    group = forcats::fct_recode(
      as.factor(cluster5),
      "Utilizadores Céticos"               = "1",
      "Utilizadores Moderados Preocupados" = "2",
      "Utilizadores Informados Confiantes" = "3",
      "Alheios à IA"                       = "4",
      "Utilizadores Informados Prudentes"  = "5"
    )
  )

# 5) Preparar dados para ggradar
df_radar_new <- df_media_fatores %>%
  dplyr::select(group, Regulacao, Risco, Conhecimento_Uso, Confianca_Atitudes)

# 6) Função de plot
library(ggradar)
library(patchwork)

plot_ggradar_cluster <- function(cluster_nome, cor_hex) {
  ggradar(
    df_radar_new %>% dplyr::filter(group == cluster_nome),
    grid.min = 0, grid.mid = 0.5, grid.max = 1,
    group.line.width = 1.2, group.point.size = 3,
    background.circle.colour = "white",
    gridline.min.linetype = "dotted",
    gridline.mid.linetype = "dotted",
    gridline.max.linetype = "dotted",
    axis.label.size = 4,
    group.colours = cor_hex,
    legend.position = "none",
    plot.title = cluster_nome,
    label.gridline.min = FALSE, label.gridline.mid = FALSE, label.gridline.max = FALSE
  )
}

# 7) Gerar radares por cluster
cores <- c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854")
p1 <- plot_ggradar_cluster("Utilizadores Céticos",               cores[1])
p2 <- plot_ggradar_cluster("Utilizadores Moderados Preocupados", cores[2])
p3 <- plot_ggradar_cluster("Utilizadores Informados Confiantes", cores[3])
p4 <- plot_ggradar_cluster("Alheios à IA",                       cores[4])
p5 <- plot_ggradar_cluster("Utilizadores Informados Prudentes",  cores[5])

(p1 + p2 + p3) / (p4 + p5 + patchwork::plot_spacer())

