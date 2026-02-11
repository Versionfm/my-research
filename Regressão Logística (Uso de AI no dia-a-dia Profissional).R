rm(list = ls())

# Carrega pacotes necessários
library(dplyr)
library(readr)
library(forcats)
library(pROC)
library(ggplot2)
library(sjPlot)
library(Epi)
library(car)
library(ResourceSelection)
library(summarytools)
library(performance)
library(car)
library(caret)
library(rms)
# 1. Lê o ficheiro de respostas
dados <- read_csv("AI (Responses) - Form Responses 1.csv")




# 2. Lê o ficheiro com os nomes desejados (linha por linha)
codigos <- read_csv("codigo.csv", locale = locale(encoding = "LATIN1"))


# 3. Substitui os nomes das colunas pela ordem dos códigos
names(dados) <- codigos[[1]]
dados <- dados %>%
  select(dados$T6, dados$AS1, dados$AS5, dados$MT9,dados$T4)

dados$DEM2[dados$DEM2 == "Feminimo"] <- "Feminino"
summary(dados)

#Número de missings por coluna
apply(dados, 2, function(x) sum(is.na(x))) # número de missings por variável

###============================###
###Análise Descritiva dos Dados###
###============================###


# Verifica estrutura
glimpse(dados)

# ==========================
# 1. Distribuição por género
# ==========================
dados %>%
  count(DEM2) %>%
  mutate(Percentual = round(100 * n / sum(n), 1)) %>%
  arrange(desc(n))

# ==========================
# 2. Faixas etárias
# ==========================
dados %>%
  count(DEM1) %>%
  mutate(Percentual = round(100 * n / sum(n), 1)) %>%
  arrange(DEM1)

# ==========================
# 3. Nível de escolaridade
# ==========================
dados %>%
  count(DEM4) %>%
  mutate(Percentual = round(100 * n / sum(n), 1)) %>%
  arrange(desc(n))

# ==========================
# 4. Situação profissional
# ==========================
dados %>%
  count(Q1) %>%
  mutate(Percentual = round(100 * n / sum(n), 1)) %>%
  arrange(desc(n))

# ==========================
# 5. Setor de atividade
# ==========================
dados %>%
  count(T4) %>%
  mutate(Percentual = round(100 * n / sum(n), 1)) %>%
  arrange(desc(n))


# ==========================
# 6. Gráficos opcionais
# ==========================
# Género
ggplot(dados, aes(x = DEM2)) +
  geom_bar(fill = "#34495E") +
  labs(title = "Distribuição por Género", x = "Género", y = "Nº de Respostas") +
  theme_minimal()

# Faixa etária
ggplot(dados, aes(x = DEM1)) +
  geom_bar(fill = "#2C3E50") +
  labs(title = "Distribuição por Faixa Etária", x = "Idade", y = "Nº de Respostas") +
  theme_minimal()


# Converter Sim para 1 e Não para 0
dados$T6 <- ifelse(dados$T6 == "Sim", 1, 0)
dados = subset(dados, T6 != "NA")
######===============####
####Regressão Simples####
####=================####

#Modelo nulo
mod0 <- glm(T6 ~ 1, family = binomial(link = logit), data = dados)
summary(mod0)

## Variável Idade ##
(tab1<-ftable(dados$DEM1, dados$T6)) # observação da tabela de contingência
mod1 <- glm(T6 ~ DEM1, family = binomial(link = logit), data = dados)
summary(mod1)

# Juntar grupos de idade 
dados$DEM1 <- dplyr::case_when(
  dados$DEM1 %in% c("18 - 24", "25 - 34") ~ "18 - 34",
  dados$DEM1 %in% c("35 - 44") ~ "35 - 44",
  dados$DEM1 %in% c("44 - 54") ~ "44 - 54",       # corrigido aqui
  dados$DEM1 %in% c("55 - 64", "65 ou mais") ~ "55 ou mais",
  TRUE ~ NA_character_
)

# Definir ordem correta dos níveis como factor
dados$DEM1 <- factor(dados$DEM1, levels = c("18 - 34", "35 - 44", "44 - 54", "55 ou mais"))

####Não significativa



##Variável género##
(tab2<-ftable(dados$DEM2, dados$T6))
mod2 <- glm(T6 ~ DEM2, family = binomial(link = logit), data = dados)
summary(mod2)

#Retirar prefiro não especificar
dados <- subset(dados, DEM2 != "Prefiro não especificar")
####Significativa



##Variável nível de educação##
(tab3<-ftable(dados$DEM3, dados$T6))
mod3 <- glm(T6 ~ DEM3, family = binomial(link = logit), data = dados)
summary(mod3)

dados$DEM3 <- dplyr::case_when(
  dados$DEM3 == "Área Metropolitana de Lisboa" ~ "Lisboa",
  dados$DEM3 == "Alentejo" ~ "Alentejo",
  dados$DEM3 %in% c("Centro de Portugal", "Norte de Portugal", "Regiões autónomas (Açores ou Madeira)", "Algarve") ~ "Resto de Portugal",
  TRUE ~ NA_character_
)

# Definir ordem desejada nos níveis
dados$DEM3 <- factor(dados$DEM3, levels = c("Lisboa", "Alentejo", "Resto de Portugal"))
####Significativa


##Variável Nível de Educação##
(tab4<-ftable(dados$DEM4, dados$T6))
mod4 <- glm(T6 ~ DEM4, family = binomial(link = logit), data = dados)
summary(mod4)

#Retirar formação profissional devido a ter so uma observação
dados <- subset(dados, DEM4 != "Outro (formação profissional, etc)")

####Significativa




##Variável Setor de Atividade##
(tab5<-ftable(dados$T4, dados$T6))
mod5 <- glm(T6 ~ T4, family = binomial(link = logit), data = dados)
summary(mod5)

#Agrupar os vários setores 
dados$T4 <- dplyr::case_when(
  dados$T4 %in% c("Agricultura e Agroindustrial", "Indústria Transformadora", "Construção", "Hotelaria", "Retalho", "Indústria farmacêutica") ~ "Setor Primário / Industrial",
  dados$T4 %in% c("Saúde", "Educação", "Ensino superior - Universidade", "Outros Serviços Públicos") ~ "Educação / Saúde / Serviços Públicos",
  dados$T4 %in% c("Banca", "Seguros") ~ "Financeiro",
  dados$T4 %in% c("Consultoria (de negócio, tencológica, outra)",
                  "Tecnologias de Informação (serviços e soluções com recurso a tecnologia)",
                  "Telecomunicações", "Transportes", "Aviação") ~ "Tecnologia e Serviços Empresariais",
  dados$T4 %in% c("Outros Serviços Privados") ~ "Serviços Privados",
  TRUE ~ "Outro"
)
dados <- dados[dados$T4 != "Outro", ]

# Verificar distribuição
table(dados$T4, dados$T6)

###Significativa



##Variável Conhecimento Autoavaliado em IA##
(tab6<-ftable(dados$AS1, dados$T6))
mod6 <- glm(T6 ~ AS1, family = binomial(link = logit), data = dados)
summary(mod6)

####Significativa



##Variável Profissionais que trabalham com codigo##
(tab7<-ftable(dados$MT9, dados$T6))
mod7 <- glm(T6 ~ MT9, family = binomial(link = logit), data = dados)
summary(mod7)

###Significativa



##Variável Concordancia com a afirmação a IA tem tornado a minha vida mais facil no dia a dia
(tab8<-ftable(dados$AS5, dados$T6))
mod8 <- glm(T6 ~ AS5, family = binomial(link = logit), data = dados)
summary(mod8)


dados$AS5 <- dplyr::case_when(
  dados$AS5 %in% c("Concordo","Concordo totalmente") ~ "Concorda",
  dados$AS5 %in% c("Discordo", "Discordo totalmente") ~ "Discorda",
  dados$AS5 == "Neutro" ~ "Neutro",
  TRUE ~ NA_character_
)

dados$AS5 <- factor(dados$AS5, levels = c("Neutro", "Discorda", "Concorda"))
dados$AS5 <- relevel(dados$AS5, ref = "Discorda")


####================######
####Regressão Multípla####
####==================####


mod9 <- glm(T6 ~ DEM2 + DEM3 + DEM4 + T4 + AS1 + AS5 + MT9,
            family = binomial(logit),
            data = dados)
summary(mod9)


mod10 <- glm(T6 ~ DEM2 + DEM3 + T4 + AS1 + AS5 + MT9,
            family = binomial(logit),
            data = dados)
summary(mod10)
anova(mod9, mod10, test = 'Chisq')


mod11 <- glm(T6 ~ DEM2  + T4 + AS1 + AS5 + MT9,
            family = binomial(logit),
            data = dados)
summary(mod11)
anova(mod10, mod11, test = 'Chisq')

mod12 <- glm(T6 ~ DEM2  + DEM3 + DEM1 + T4 + AS1 + AS5 + MT9,
             family = binomial(logit),
             data = dados)
summary(mod12)
anova(mod11, mod12, test = 'Chisq')

mod13 <- glm(T6 ~ T4 + AS1 + AS5 + MT9,
             family = binomial(logit),
             data = dados)
summary(mod13)
anova(mod11, mod13, test = 'Chisq')

mod14 <- glm(T6 ~ T4 + AS1 + AS5,
             family = binomial(logit),
             data = dados)
summary(mod14)
anova(mod13, mod14, test = 'Chisq')

modf <- mod14
summary(modf)


#########==================================================
#####   Bondade do ajustamento e capacidade descritiva
########===================================================
res <- residuals(modf, type = "pearson")
fitted_vals <- fitted(modf)

plot(fitted_vals, res,
     xlab = "Valores ajustados", ylab = "Resíduos de Pearson",
     main = "Resíduos vs Valores Ajustados")
abline(h = 0, col = "red")



###Multicolinearidade####
vif(modf)
##Não há


## R2 de Nagelkerke ##
(n <- length(dados$T6))
n <- nrow(modf$model)  # número de observações
R2N <- (1 - exp(-(modf$null.deviance - modf$deviance)/n)) / 
  (1 - exp(-modf$null.deviance/n))
R2N
###0.5910618


## Teste de Hosmer e Lemeshow ##
# extrai os dados usados no modelo (caso haja NAs removidos)
y <- modf$y                      # variável dependente (binária, já codificada como 0/1)
yhat <- fitted(modf)            # valores ajustados (probabilidades preditas)

# Agora ambos os vetores têm o mesmo comprimento
hl <- hoslem.test(y, yhat, g = 10)
hl
hl$expected
hl$observed
hl$p.value
##p value = 0.5198 podemos concluir que o modelo se ajusta aos dados


#### Capacidade discriminativa: curva ROC ####
ROC(form = T6 ~ T4 + AS1 + AS5 + MT9, data = dados, plot = "ROC",
    PV = T,
    MX = T,
    AUC = T)

#Modelo com uma capacidade descriminativa muito boa com 0.896
#Para um ponto de corte 0.0.646 obtem-se uma sensibilidade de 77.6% e uma especificidade de 87.5%
ci.auc(modf$y, fitted(modf), conf.level = 0.95)
#IC95% AUC = [0.8328, 0.9599]
##Singnifica que o modelo consegue distinguir corretamente entre estas classes em mais de 83% dos casos e possivelmente ate até cerca de 96%


pred_bin <- ifelse(fitted(modf) > 0.516, 1, 0)
table(Real = modf$y, Previsto = pred_bin)


###A versão reduzida do modelo, excluindo a variável MT9, relevou a especificidade (87.5%)

####==============================####
####         Interpretação        ####
####==============================####


##Significância dos Efeitos
drop1(modf, test = 'Chisq')

## OR e intervalos de confiança baseados na verosimilhança de perfil 
cbind(exp(coef(modf)), exp(confint(modf)))


# Representação dos OddRatios  com IC#
plot_model(modf, show.values = TRUE, value.offset = .3, vline.color = "red")




library(sjPlot)
names(coef(modf))
# Dicionário com os nomes legíveis
rotulos_legiveis <- c(
  "T4Financeiro"      = "Setor de atividade - Financeiro",
  "T4Serviços Privados" = "Setor de atividade - Serviços Privados",
  "T4Tecnologia e Serviços Empresariais" = "Setor de atividade - Tecnologia e Serviços Empresariais",
  "T4Setor Primário / Industrial" = "Setor de atividade - Primário / Industrial",
  "AS1"                                 = "Nível de conhecimento sobre IA (autoavaliado)",
  "AS5Concorda"                         = "Perceção de facilitação - Concorda",
  "AS5Neutro"                           = "Perceção de facilitação - Neutro"
)

# Gráfico com nomes legíveis e OR com IC
plot_model(modf, type = "est", show.values = TRUE, value.offset = 0.3,
           vline.color = "red", axis.labels = rotulos_legiveis,
           title = "Efeito dos preditores sobre o uso de IA no dia-a-dia Profissional (T6)",
           axis.title = "Odds Ratio (escala logarítmica)")

###Versão em Inglês
# Dictionary with readable English labels
readable_labels <- c(
  "T4Financeiro"      = "Sector of Activity – Financial",
  "T4Serviços Privados" = "Sector of Activity – Private Services",
  "T4Tecnologia e Serviços Empresariais" = "Sector of Activity – Technology and Business Services",
  "T4Setor Primário / Industrial" = "Sector of Activity – Primary / Industrial",
  "AS1"                                 = "Self-Assessed Knowledge of AI",
  "AS5Concorda"                         = "Perceived Facilitation – Agree",
  "AS5Neutro"                           = "Perceived Facilitation – Neutral"
)

# Plot with readable English labels and OR with CI
plot_model(modf, type = "est", show.values = TRUE, value.offset = 0.3,
           vline.color = "red", axis.labels = readable_labels,
           title = "Effect of Predictors on the Use of AI in Daily Professional Activities (T6)",
           axis.title = "Odds Ratio (log scale)")


####==========####
#### Predição ####
####==========####

set.seed(123) 
# Dados de treino e dados de teste
train_index <- createDataPartition(dados$T6, p = 0.7, list = FALSE)
train_data <- dados[train_index, ]
test_data  <- dados[-train_index, ]
mod_train <- glm(T6 ~ T4 + AS1 + AS5,
                 family = binomial(logit),
                 data = dados)
pred_probs_test <- predict(mod_train, newdata = test_data, type = "response")
pred_class_test <- ifelse(pred_probs_test >0.516 , "1", "0")
conf_matrix_test <- confusionMatrix(as.factor(pred_class_test), as.factor(test_data$T6), positive = "1")
print(conf_matrix_test)
