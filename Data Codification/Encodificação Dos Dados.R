rm(list = ls())
library(readxl)
library(readr) 
library(dplyr)       # manipulação de dados
library(tidyr)       # tratamento de missing e tidying
library(stringr)     # operações de texto
library(forcats)     # manipulação de fatores

df_raw <- read_excel("Dados para Analise de Clusters.xlsx")


glimpse(df_raw)
summary(df_raw)
names(df_raw)


codigos <- read_csv("CodigoCluster.csv", locale = locale(encoding = "LATIN1"))

names(df_raw) <- codigos[[1]]

df_raw$DEM2[df_raw$DEM2 == "Feminimo"] <- "Feminino"
names(df_raw)


# Variáveis de perceção, confiança, risco e uso de IA conforme relatório:
vars <- c(
  #Demográficas
  "DEM1", "DEM2", "DEM3", "DEM4", "Q1",
  # Atitudes e perceções gerais (escala Likert)
  "AS1","AS2","AS4","AS5","AS6","AS7","AS8","AS9",
  # Perceção de riscos futuros
  "PR1","PR2","PR3","PR4","PR5",
  # Perceção de benefícios futuros
  "PR6","PR7",
  # Avaliação de riscos específicos
  "PR8","PR9","PR10","PR11","PR12",
  # Importância de medidas de mitigação e confiança
  "PR13","PR14","PR15"
)


# 5. Validar e selecionar as variáveis existentes Validar e selecionar as variáveis existentes
definir_vars <- intersect(vars, names(df_raw))
if (length(definir_vars) == 0) stop("Nenhuma variável encontrada. Ajusta o vetor 'vars' com os nomes corretos.")
if (length(setdiff(vars, definir_vars)) > 0) {
  warning("Ignoradas variáveis não encontradas: ", paste(setdiff(vars, definir_vars), collapse = ", "))
}
df <- df_raw %>% select(all_of(definir_vars))
message("Variáveis selecionadas: ", paste(definir_vars, collapse = ", "))

# 6. Remover casos com missing nas variáveis selecionadas
df <- df %>% drop_na(all_of(definir_vars))



# 6. Inspeção de Valores Únicos
# Verificar rótulos antes de recodificar para ajustar padrões
for (v in c("AS2", "AS4", "PR1", "PR6", "AS9", "PR8", "PR12", "PR15", "Q1", "DEM1", "DEM2", "DEM3", "DEM4")) {
  cat("\nValores únicos de", v, ":\n")
  print(unique(df[[v]]))
}

# 7. Recodificação de Variáveis

## 7.1 AS1 – Autoavaliação do conhecimento em IA (Likert: 1=Muito baixo … 5=Muito alto → 0–4)
df <- df %>%
  mutate(
    AS1 = case_when(
      AS1 == 1 ~ 0,
      AS1 == 2 ~ 1,
      AS1 == 3 ~ 2,
      AS1 == 4 ~ 3,
      AS1 == 5 ~ 4,
      TRUE      ~ NA_real_
    )
  )

## 7.2 AS2 – Compreensão de IA generativa (categorias textuais → 0–3)
df <- df %>%
  mutate(
    AS2 = case_when(
      str_detect(AS2, regex("nunca ouvi|Não, nunca ouvi esse termo", ignore_case = TRUE))                               ~ 0,
      str_detect(AS2, regex("já ouvi falar, mas não sei exatamente o que é", ignore_case = TRUE))                   ~ 1,
      str_detect(AS2, regex("Sim, mas não sei como esses sistemas geram conteúdos", ignore_case = TRUE))            ~ 2,
      str_detect(AS2, regex("Sim, e compreendo que são algoritmos treinados com dados", ignore_case = TRUE))       ~ 3,
      TRUE                                                                                                         ~ NA_real_
    )
  )

## 7.3 AS4 – Frequência de uso de IA 
# (Inclui correção de "Diaramente" para capturar "Diariamente" e variações)
df <- df %>%
  mutate(
    AS4 = case_when(
      str_detect(AS4, regex("^Nunca$", ignore_case = TRUE))                                 ~ 0,
      str_detect(AS4, regex("Raramente", ignore_case = TRUE))                                ~ 1,
      str_detect(AS4, regex("Ocasionalmente", ignore_case = TRUE))                           ~ 2,
      str_detect(AS4, regex("Várias vezes por semana", ignore_case = TRUE))                 ~ 3,
      str_detect(AS4, regex("Diaramente|Diariamente", ignore_case = TRUE))                               ~ 4,
      TRUE                                                                                      ~ NA_real_
    )
  )

## 7.4 AS5–AS8 – Escala de concordância (Discordo totalmente=0 … Concordo totalmente=4)
df <- df %>%
  mutate(across(
    .cols = AS5:AS8,
    .fns = ~ case_when(
      str_detect(.x, regex("Discordo totalmente", ignore_case = TRUE)) ~ 0,
      str_detect(.x, regex("Discordo$", ignore_case = TRUE))          ~ 1,
      str_detect(.x, regex("Neutro", ignore_case = TRUE))             ~ 2,
      str_detect(.x, regex("Concordo$", ignore_case = TRUE))          ~ 3,
      str_detect(.x, regex("Concordo totalmente", ignore_case = TRUE)) ~ 4,
      TRUE                                                            ~ NA_real_
    )
  ))

## 7.4.1 AS9 - Ferramentas Pagas
## 7.4.1 AS9 – Uso de ferramentas de IA pagas (Sim Pago=2; Gratuitas=1; Nenhuma=0)
df <- df %>%
  mutate(
    AS9 = case_when(
      str_detect(AS9, regex("Sim, utilizo ferramentas de IA com subscrição ou versão paga", ignore_case = TRUE)) ~ 2,
      str_detect(AS9, regex("Não, apenas utilizo ferramentas gratuitas", ignore_case = TRUE))                   ~ 1,
      str_detect(AS9, regex("Não utilizo ferramentas de IA|Não utilizo ferramentas IA", ignore_case = TRUE))     ~ 0,
      TRUE ~ NA_real_
    )
  )

## 7.5 PR1 – Perceção combinada de riscos e benefícios
# Criar 5 dummies: muitos/poucos riscos, muitos/poucos benefícios, neutro
df <- df %>%
  mutate(
    PR1_muitos_riscos     = as.integer(str_detect(PR1, regex("Muitos Riscos", ignore_case = TRUE))),
    PR1_poucos_riscos     = as.integer(str_detect(PR1, regex("Poucos Riscos", ignore_case = TRUE))),
    PR1_muitos_beneficios = as.integer(str_detect(PR1, regex("Muitos Benefícios", ignore_case = TRUE))),
    PR1_poucos_beneficios = as.integer(str_detect(PR1, regex("Poucos Benefícios", ignore_case = TRUE))),
    PR1_neutro            = as.integer(str_detect(PR1, regex("Nem Riscos Nem Benefícios", ignore_case = TRUE)))
  )
# Remover PR1 original, se desejado:
# df <- df %>% select(-PR1)

## 7.6 PR2–PR7 – Perceção de benefícios futuros (Nenhum=0; Poucos=1; Muitos=2)
df <- df %>%
  mutate(across(
    .cols = PR2:PR7,
    .fns  = ~ case_when(
      str_detect(.x, regex("Sem riscos ou problemas", ignore_case = TRUE))   ~ 0,
      str_detect(.x, regex("Alguns riscos ou problemas", ignore_case = TRUE)) ~ 1,
      str_detect(.x, regex("Muitos riscos ou problemas", ignore_case = TRUE)) ~ 2,
      TRUE                                                                    ~ NA_real_
    )
  ))
df <- df %>% select(-PR1)


## 7.7 PR8–PR14 – Importância de medidas de mitigação (Nada=0; Pouco=1; Importante=2; Muito=3)
df <- df %>%
  mutate(across(
    .cols = PR8:PR14,
    .fns  = ~ case_when(
      str_detect(.x, regex("Nada Importante", ignore_case = TRUE))  ~ 0,
      str_detect(.x, regex("Pouco Importante", ignore_case = TRUE)) ~ 1,
      str_detect(.x, regex("Importante$", ignore_case = TRUE))       ~ 2,
      str_detect(.x, regex("Muito Importante", ignore_case = TRUE))  ~ 3,
      TRUE                                                            ~ NA_real_
    )
  ))
## 7.8 PR15 – Confiança em agentes
# Esta variável é nominal: transformar em factor ou dummies conforme posterior análise
# Exemplo: converter para factor com níveis definidos

df <- df %>%
  mutate(
    PR15 = factor(PR15,
                  levels = c(
                    "União Europeia (Instituições europeias)",
                    "Universidades e centros de investigação (comunidade científica)",
                    "Governo nacional (autoridades públicas em Portugal)",
                    "Nenhuma destas - não confio plenamente em nenhum destes atores",
                    "Organizações internacionais (ex.: Nações Unidas/UNESCO, OCDE)",
                    "ONGs e sociedade civil (organizações independentes de defesa do cidadão)",
                    "Empresas tecnológicas e indústrias"
                  )
    )
  )


df <- df %>%
  mutate(
    # 7.9 - Situação profissional
    Q1_cat = case_when(
      Q1 == "Trabalhador por conta de outrem" ~ "Empregado",
      Q1 == "Trabalhador por conta própria (profissional por conta própria ou empresário)" ~ "Conta própria",
      Q1 == "Docente ou Investigador no ensino superior" ~ "Docente/Investigador",
      Q1 == "Estudante (Universitário ou outro nível de ensino)" ~ "Estudante",
      Q1 == "Desempregado" ~ "Desempregado",
      Q1 == "Reformado ou outra situação não ativa" ~ "Inativo",
      TRUE ~ NA_character_
    ),
    Q1_num = as.numeric(factor(Q1_cat, levels = c(
      "Empregado", "Conta própria", "Docente/Investigador",
      "Estudante", "Desempregado", "Inativo"
    ))),
    
    # 7.10 - Faixa etária
    DEM1_cat = case_when(
      DEM1 %in% c("18 - 24", "25 - 34") ~ "18 - 34",
      DEM1 == "35 - 44" ~ "35 - 44",
      DEM1 == "44 - 54" ~ "44 - 54",
      DEM1 %in% c("55 - 64", "65 ou mais") ~ "55 ou mais",
      TRUE ~ NA_character_
    ),
    DEM1_num = as.numeric(factor(DEM1_cat, levels = c("18 - 34", "35 - 44", "44 - 54", "55 ou mais"))),
    
    # 7.11 - Género
    DEM2_cat = case_when(
      DEM2 == "Masculino" ~ "Masculino",
      DEM2 == "Feminino" ~ "Feminino",
      TRUE ~ NA_character_  # Exclui "Prefiro não especificar"
    ),
    DEM2_num = as.numeric(factor(DEM2_cat, levels = c("Masculino", "Feminino"))),
    
    # 7.12 - Região de residência
    DEM3_cat = case_when(
      DEM3 == "Área Metropolitana de Lisboa" ~ "Lisboa",
      DEM3 == "Alentejo" ~ "Alentejo",
      DEM3 %in% c("Centro de Portugal", "Norte de Portugal", "Regiões autónomas (Açores ou Madeira)", "Algarve") ~ "Resto de Portugal",
      TRUE ~ NA_character_
    ),
    DEM3_num = as.numeric(factor(DEM3_cat, levels = c("Lisboa", "Alentejo", "Resto de Portugal"))),
    
    # 7.13 - Nível de educação
    DEM4_cat = case_when(
      DEM4 == "Ensino secundário (12º Ano)" ~ "Secundário",
      DEM4 == "Ensino Superior - Licenciatura" ~ "Licenciatura",
      DEM4 == "Ensino Superior - Pós-graduação/Mestrado/Doutoramento" ~ "Pós-graduação",
      DEM4 == "Outro (formação profissional, etc)" ~ "Outro (formação profissional, etc)",
      TRUE ~ NA_character_  
    ),
    DEM4_num = as.numeric(factor(DEM4_cat, levels = c("Secundário", "Licenciatura", "Pós-graduação","Outro (formação profissional, etc)")))
  )


dfc <- df %>%
  select(
    -c(DEM1, DEM2, DEM3, DEM4, Q1,
       DEM1_cat, DEM2_cat, DEM3_cat, DEM4_cat, Q1_cat)
  )


write.csv(df, "perfil_clusters.csv", row.names = FALSE)
write.xlsx(dfc, file = "dados_codificados")
library(cluster)

# 1. Matriz de dissimilaridade de Gower
dissim <- daisy(dfc, metric = "gower")

# 2. Determinação do número ideal de clusters via silhueta
sil_width <- sapply(2:6, function(k) {
  pam_fit <- pam(dissim, diss = TRUE, k = k)
  pam_fit$silinfo$avg.width
})
print(data.frame(k = 2:6, sil_width = sil_width))


# 3. Clustering PAM com k = 4 (escolha guiada por métricas e interpretação prática)
pam_res <- pam(dissim, diss = TRUE, k = 4)

# 4. Atribuição de perfis de cluster ao data frame
df$PerfilCluster <- factor(pam_res$clustering, levels = 1:4,
                           labels = c("Céticos Preocupados",
                                      "Entusiastas Otimistas",
                                      "Especialistas Prudentes",
                                      "Moderados Indiferentes"))

# 5. Visualização dos resultados
# 5.1 Gráfico de silhueta
plot(pam_res, which = 2, main = "Silhueta do Clustering PAM (k=4)")


# 5.2 Distribuição do tamanho dos clusters
cluster_sizes <- table(df$PerfilCluster)
barplot(cluster_sizes,
        main = "Tamanho dos Perfis de Cluster",
        xlab = "PerfilCluster",
        ylab = "Número de Observações", las = 1)

# 6. Sumário de perfis (médias das variáveis chave por cluster)
cluster_summary <- df %>%
  group_by(PerfilCluster) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))
print(cluster_summary)
