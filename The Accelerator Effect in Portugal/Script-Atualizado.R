library(readr)
library(ggplot2)
library(scales)
library(lmtest)
library(car)
library(effects)
library(dplyr)

###===============================
###TRATAMENTO E RECOLHA DE DADOS
###===============================

dados <- read_csv("S_ries_PIB_e_FBCF_Reais.csv")
summary(dados)

# Calcular logaritmos
dados$log_PIB <- log(dados$PIB_Real)
dados$log_FBCF <- log(dados$FBCF_Real)

# Calcular as primeiras diferenças (variação trimestral)
dados$var_PIB <- c(NA, diff(dados$log_PIB))
dados$var_FBCF <- c(NA, diff(dados$log_FBCF))
dados_limpos <- na.omit(dados)

# Calcular fator de escala
fator_escala <- max(dados$var_PIB, na.rm = TRUE) / max(dados$var_FBCF, na.rm = TRUE)

###===========================###
###Visualização das variáveis###
###==========================###
# Tabela 1: Valores reais
tabela_reais <- dados_limpos %>%
  summarise(
    media_PIB = mean(PIB_Real, na.rm = TRUE),
    sd_PIB = sd(PIB_Real, na.rm = TRUE),
    cv_PIB = sd_PIB / media_PIB * 100,
    
    media_FBCF = mean(FBCF_Real, na.rm = TRUE),
    sd_FBCF = sd(FBCF_Real, na.rm = TRUE),
    cv_FBCF = sd_FBCF / media_FBCF * 100
  )

# Tabela 2: Valores logaritmizados
tabela_log <- dados_limpos %>%
  summarise(
    media_log_PIB = mean(var_PIB, na.rm = TRUE),
    sd_log_PIB = sd(var_PIB, na.rm = TRUE),
    cv_log_PIB = sd_log_PIB / media_log_PIB * 100,
    
    media_log_FBCF = mean(var_FBCF, na.rm = TRUE),
    sd_log_FBCF = sd(var_FBCF, na.rm = TRUE),
    cv_log_FBCF = sd_log_FBCF / media_log_FBCF * 100
  )

#Visualizar
tabela_reais
tabela_log$cv_log_PIB



#Períodos de recessão
recessoes <- data.frame(
  inicio = as.Date(c("2008-01-01", "2011-01-01", "2020-01-01")),
  fim    = as.Date(c("2009-12-31", "2014-12-31", "2020-12-31")),
  crise  = c("Crise Financeira", "Crise da Dívida", "COVID-19")
)




# Investimento Privado
ggplot(dados_limpos, aes(x = Data)) +
  # Faixas das recessões
  geom_rect(data = recessoes, aes(xmin = inicio, xmax = fim, ymin = -Inf, ymax = Inf),
            fill = "gray80", alpha = 0.5, inherit.aes = FALSE) +
  # Linhas observadas e previstas
  geom_line(aes(y = FBCF_Real, color = "GFCF"), size = 1) +
  labs(
    title = "Evolution of Gross Fixed Capital Formation (GFCF) in Portugal (1977-2023)",
    x = "Quarter",
    y = "GFCF",
    color = ""
  ) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y"
  ) +
  scale_color_manual(values = c("GFCF" = "blue")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#PIB
ggplot(dados_limpos, aes(x = Data)) +
  # Faixas das recessões
  geom_rect(data = recessoes, aes(xmin = inicio, xmax = fim, ymin = -Inf, ymax = Inf),
            fill = "gray80", alpha = 0.5, inherit.aes = FALSE) +
  # Linhas observadas e previstas
  geom_line(aes(y = PIB_Real, color = "GDP"), size = 1) +
  labs(
    title = "Evolution of Real GDP in Portugal (1977-2023)",
    x = "Quarter",
    y = "GDP",
    color = ""
  ) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y"
  ) +
  scale_color_manual(values = c("GDP" = "darkgreen")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Gráfico
ggplot(dados, aes(x = Data)) +
  # Linhas verticais para cada trimestre
  geom_vline(xintercept = as.numeric(dados$Data), color = "grey90", size = 0.2) +
  
  # Linhas das variáveis
  geom_line(aes(y = var_PIB), color = "#0072B2", size = 0.8) +
  geom_line(aes(y = var_FBCF * fator_escala), color = "#E69F00", size = 0.8) +
  
  # Escalas e eixos
  scale_y_continuous(
    name = "Change(%)",
    labels = percent_format(accuracy = 0.1),
    sec.axis = sec_axis(~ . / fator_escala, name = "Change(%)", labels = percent_format(accuracy = 0.1))
  ) +
  # Legenda diretamente no gráfico (anotações)
  geom_line(aes(y = var_PIB, color = "GDP"), size = 0.8) +
  geom_line(aes(y = var_FBCF * fator_escala, color = "GFCF"), size = 0.8) +
  
  scale_color_manual(
    name = "Variables",
    values = c("GDP" = "#0072B2", "GFCF" = "#E69F00")
  ) +
  labs(
    title = "Quarterly Percentage Change in GDP and GFCF (1977-2023)",
    x = "Year",
    y = "Change(%)",
    caption = "Source: Banco de Portugal. Author's own elaboration"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top"
  ) +
  geom_rect(data = recessoes, aes(xmin = inicio, xmax = fim, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, fill = "grey80", alpha = 0.3)



###=================================
###REGRESSÃO MÚLTIPLA SIMPLES
###=================================


#Regressão linear: variação do investimento ~ variação do PIB
modelo <- lm(var_FBCF ~ var_PIB, data = dados_limpos)
#variância no PIB mostra-se altamente significativa para descrever a forma como o fbcf varia
summary(modelo)


mod12 <- lm(var_FBCF ~ log_PIB, data = dados_limpos)
summary(mod12)



# Gráfico de dispersão + linha de regressão do mod0
ggplot(dados_limpos, aes(x = var_PIB, y = var_FBCF)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Efeito Acelerador em Portugal",
       x = "Variação do PIB real (log)",
       y = "Variação do Investimento (log)") +
  theme_minimal()



#Teste de heterocedasticidade do modelo
bptest(modelo)
#O modelo apresenta homocedasticidade (homogenidade de variâncias)

###===============
###Testes de Lag
###===============

# Criar as variáveis de lag adicionais
dados_limpos$lag1_log_PIB <- dplyr::lag(dados_limpos$log_PIB, 1)
dados_limpos$lag2_log_PIB <- dplyr::lag(dados_limpos$log_PIB, 2)
dados_limpos$lag3_log_PIB <- dplyr::lag(dados_limpos$log_PIB, 3)
dados_limpos$lag4_log_PIB <- dplyr::lag(dados_limpos$log_PIB, 4)
dados_limpos$lag5_log_PIB <- dplyr::lag(dados_limpos$log_PIB, 5)
dados_limpos$lag5_var_PIB <- dplyr::lag(dados_limpos$var_PIB, 5)
dados_limpos$lag6_log_PIB <- dplyr::lag(dados_limpos$log_PIB, 6)
dados_limpos$lag9_var_PIB <- dplyr::lag(dados_limpos$var_PIB, 9)
dados_limpos$lag13_var_PIB <- dplyr::lag(dados_limpos$var_PIB, 13)
dados_limpos$lag17_var_PIB <- dplyr::lag(dados_limpos$var_PIB, 17)
# Remover NAs após criação dos novos lags
dados_lags <- na.omit(dados_limpos)

#Modelo Simples
mod_nivel <- lm(var_FBCF ~ log_PIB, data  = dados_lags)
summary(mod_nivel)
#Não significativo

# Modelo com lags do 6 ao 1 (completo)
mod_lag6_1 <- lm(var_FBCF ~ log_PIB + lag1_log_PIB + lag2_log_PIB + lag3_log_PIB + lag4_log_PIB + lag5_log_PIB + lag6_log_PIB, data = dados_lags)
summary(mod_lag6_1)
#log_PIB, lag 1,5 e 6 significativas


#Modelo com lags do 5 ao 1 
mod_lag5_1 <- lm(var_FBCF ~ log_PIB + lag1_log_PIB + lag2_log_PIB + lag3_log_PIB + lag4_log_PIB + lag5_log_PIB, data = dados_lags)
summary(mod_lag5_1)
#log_PIB, lag 1 e 3
BIC(mod_lag5_1)

#Modelo com lags do 4 ao 1 
mod_lag4_1 <- lm(var_FBCF ~ log_PIB + lag1_log_PIB + lag2_log_PIB + lag3_log_PIB + lag4_log_PIB, data = dados_lags)
summary(mod_lag4_1)
#log_PIB, lag 1 e 3 significativos
BIC(mod_lag4_1)

#Modelo com lags do 3 ao 1 
mod_lag3_1 <- lm(var_FBCF ~ log_PIB + lag1_log_PIB + lag2_log_PIB + lag3_log_PIB, data = dados_lags)
summary(mod_lag3_1)
#log_PIB, lag 1 e 3 significativos
BIC(mod_lag3_1)

#Modelo com lags do 2 ao 1 
mod_lag2_1 <- lm(var_FBCF ~ log_PIB + lag1_log_PIB + lag2_log_PIB, data = dados_lags)
summary(mod_lag2_1)
#log_PIB, lag 1 e 3 significativos
BIC(mod_lag2_1)



#Modelo com lags do 1,5 e 6 
mod_lag_156 <- lm(var_FBCF ~ log_PIB + lag1_log_PIB + lag5_log_PIB + lag6_log_PIB, data = dados_lags)
summary(mod_lag_156)
BIC(mod_lag_156)
#log_PIB, lag 1, 5 e 6 

#Modelo com 1, 3 
mod_lag_13 <- lm(var_FBCF ~ log_PIB + lag1_log_PIB + lag3_log_PIB, data = dados_lags)
summary(mod_lag_13)
#log_PIB, lag 1 e 3 significativos
BIC(mod_lag_13)

#Modelo com 1, 3 e 5
mod_lag_136 <- lm(var_FBCF ~ log_PIB + lag1_log_PIB + lag3_log_PIB + lag6_log_PIB, data = dados_lags)
summary(mod_lag_136)
#log_PIB, lag 1 e 3 significativos
BIC(mod_lag_136)

##Modelo com menor BIC 
mod_lag_16 <- lm(var_FBCF ~ log_PIB + lag1_log_PIB + lag6_log_PIB, data = dados_lags)
summary(mod_lag_16)
BIC(mod_lag_16)


#Teste de heterocedasticidade do modelo
bptest(mod_lag_13)
#Não há evidência de heterocedasticidade no modelo


#testar com multiplos de 3
#trimestres de varios anos encadeados
#tentar ter uma interceção significativa


###========================================
###Modelos com lags anuais
###========================================


###lags do log
dados_limpos$lag2anos_log_PIB <- dplyr::lag(dados_limpos$log_PIB, 9)
dados_limpos$lag3anos_log_PIB <- dplyr::lag(dados_limpos$log_PIB, 13)
dados_limpos$lag4anos_log_PIB <- dplyr::lag(dados_limpos$log_PIB, 17)
dados_limpos$lag5anos_log_PIB <- dplyr::lag(dados_limpos$log_PIB, 21)

###lags da variância anual
dados_limpos$lag1anos_var_PIB <- dplyr::lag(dados_limpos$log_PIB, 5)
dados_limpos$lag2anos_var_PIB <- dplyr::lag(dados_limpos$log_PIB, 9)
dados_limpos$lag3anos_var_PIB <- dplyr::lag(dados_limpos$log_PIB, 13)
dados_limpos$lag4anos_var_PIB <- dplyr::lag(dados_limpos$log_PIB, 17)
dados_limpos$lag5anos_var_PIB <- dplyr::lag(dados_limpos$log_PIB, 21)

###5 anos de lag
mod_5anos <- lm(var_FBCF ~ log_PIB + lag5_log_PIB + lag2anos_log_PIB + lag3anos_log_PIB
                + lag4anos_log_PIB + lag5anos_log_PIB, data = dados_limpos)
summary(mod_5anos)


###4 anos de lag
mod_4anos <- lm(var_FBCF ~ log_PIB + lag5_log_PIB + lag2anos_log_PIB + lag3anos_log_PIB
                + lag4anos_log_PIB, data = dados_limpos)
summary(mod_4anos)

###3 anos de lag
mod_3anos <- lm(var_FBCF ~ log_PIB + lag5_log_PIB + lag2anos_log_PIB + lag3anos_log_PIB
                          , data = dados_limpos)
summary(mod_3anos)

###2 anos de lag
mod_2anos <- lm(var_FBCF ~ log_PIB +lag5_log_PIB + lag2anos_log_PIB
                , data = dados_limpos)
summary(mod_2anos)

###====================
###Modelos com var_PIB
###====================

#Lag 5 anos
mod_var_5 <- lm(var_FBCF ~ var_PIB + lag1anos_var_PIB + lag2anos_var_PIB
                + lag3anos_var_PIB + lag4anos_var_PIB + lag5anos_var_PIB,
                data = dados_limpos)
summary(mod_var_5)

#Lag 4 anos
mod_var_4 <- lm(var_FBCF ~ var_PIB + lag1anos_var_PIB + lag2anos_var_PIB
                + lag3anos_var_PIB + lag4anos_var_PIB,
                data = dados_limpos)
summary(mod_var_4)

#Lag 3 anos
mod_var_3 <- lm(var_FBCF ~ var_PIB + lag1anos_var_PIB + lag2anos_var_PIB
                + lag3anos_var_PIB, data = dados_limpos)
summary(mod_var_3)

#Lag 2 anos
mod_var_2 <- lm(var_FBCF ~ var_PIB + lag1anos_var_PIB + lag2anos_var_PIB
               , data = dados_limpos)
summary(mod_var_2)

#Lag 1 ano
mod_var_1 <- lm(var_FBCF ~ var_PIB + lag1anos_var_PIB, data = dados_limpos)
summary(mod_var_1)

#mod contemporaneo
summary(modelo)

###BIC e AIC
BIC(modelo)
BIC(mod_var_1)
BIC(mod_var_2)
BIC(mod_var_3)
BIC(mod_var_4)
BIC(mod_var_5)

AIC(modelo)
AIC(mod_var_1)
AIC(mod_var_2)
AIC(mod_var_3)
AIC(mod_var_4)
AIC(mod_var_5)

###Pelos resultados concluimos que a complexidade adicionada pelos lags 

###Visualização Gráfica
plot(allEffects(mod_var_5))
plot(allEffects(mod_var_4))
plot(allEffects(mod_var_3))
plot(allEffects(mod_var_2))
plot(allEffects(mod_var_1))
plot(allEffects(modelo))


#Outliers
par(mfrow = c(1, 1))
plot(modelo, which = 1)

#Cooks Distance
influencePlot(modelo, id.n = 5)

cooksd <- cooks.distance(modelo)
influentes <- which(cooksd > 4 / length(cooksd))
dados_limpos[influentes, ]


dados_limpos[c(12, 173, 174), ]
###Descrição dos pontos acima
###Ponto 12, mostra o efeito contrário, demonstra uma diminuição do investimento face a um aumento do PIB em 0.0142 p.p.
###Pontos 173, 174, 2º e 3º trimestres após covid (março 2020), levam o modelo a substimar o efeito da variação do PIB na variação do Investimento Privado



#Modelo sem os pontos
modelo1 <- lm(var_FBCF ~ var_PIB + 0, data = dados_limpos)
mod_sens_com_alpha <- lm(var_FBCF ~ var_PIB,
               data = dados_limpos[-c(12, 173, 174, 176), ])
mod_sens <- lm(var_FBCF ~ var_PIB + 0,
               data = dados_limpos[-c(12, 173, 174, 176), ])
summary(mod_sens)
summary(mod_sens_com_alpha)
summary(modelo1)

BIC(mod_sens)
BIC(mod_sens_com_alpha)

###Comparando os dois modelos revela-se que
###os quatro pontos acima estavam a prejudicar o modelo


###Verifica homocedasticidade
bp_test(mod_sens)

modfinal <- mod_sens

###===============================
###Interpretação do modelo final
###===============================

summary(modfinal)
###Quando o PIB não cresce o investimento
###tende a ter um decréscimo de 0.002494

###Reta
plot(allEffects(mod_sens))

#Menor BIC de todos 
BIC(modfinal)


# Apenas nas linhas onde o preditor está presente
linhas_validas <- which(!is.na(dados_limpos$var_PIB))

# Inicializa coluna com NA
dados_limpos$previstos <- NA

# Previsões apenas onde é possível
dados_limpos$previstos[linhas_validas] <- predict(mod_sens, newdata = dados_limpos[linhas_validas, ])


# Gráfico Observado vs Previsto com faixas de recessão para modelo sem sensibilidade
ggplot(dados_limpos, aes(x = Data)) +
  # Faixas das recessões
  geom_rect(data = recessoes, aes(xmin = inicio, xmax = fim, ymin = -Inf, ymax = Inf),
            fill = "gray80", alpha = 0.5, inherit.aes = FALSE) +
  # Linhas observadas e previstas
  geom_line(aes(y = var_FBCF, color = "Observado"), size = 0.5) +
  geom_line(aes(y = previstos, color = "Previsto"), size = 1) +
  labs(
    title = "FBCF: Observado vs Previsto com Destaque de Recessões",
    x = "Trimestre",
    y = "Variação da FBCF",
    color = ""
  ) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y-Q%q"
  ) +
  scale_color_manual(values = c("Observado" = "red", "Previsto" = "blue")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###Adicionar os dois graficos e demonstrar a diferença entre a variação



###Podemos ver que o modelo tem uma boa capacidade descritiva nos momentos de recessão
###E capta bem a tendência

ggplot(dados_limpos, aes(x = Data)) +
  
  # Shaded recession periods
  geom_rect(data = recessoes,
            aes(xmin = inicio, xmax = fim, ymin = -Inf, ymax = Inf),
            fill = "grey80", alpha = 0.5, inherit.aes = FALSE) +
  
  # Observed (red) and predicted (blue) lines
  geom_line(aes(y = var_FBCF, color = "Observed"), size = 0.7) +
  geom_line(aes(y = previstos, color = "Predicted"), size = 0.7) +
  
  # Title and axis labels
  labs(
    title = "Observed vs. Predicted Growth of GFCF with Recession Periods Highlighted",
    subtitle = "Based on the final regression model (mod_sens)",
    x = "Quarter",
    y = "Growth Rate of FBCF (log difference)",
    color = "Legend"
  ) +
  
  # Better time scale formatting
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  
  # Custom colors for lines
  scale_color_manual(values = c("Observed" = "#E74C3C", "Predicted" = "#2980B9")) +
  
  # Clean minimal theme with rotated x labels
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )
####RELATORIO#####

###Carazterizar os dados

##Apresentar um sumario dos dados
##Tratamento dos dados


###Metodologia
##

mod8 <- lm(var_FBCF ~ var_PIB + lag5_var_PIB + lag9_var_PIB + lag13_var_PIB + lag17_var_PIB + 0, data = dados_limpos[-c(12, 173, 174, 176), ])
summary(mod8)


mod9 <- lm(var_FBCF ~ var_PIB + lag5_var_PIB + lag9_var_PIB + lag17_var_PIB + 0, data = dados_limpos[-c(12, 173, 174, 176), ])
summary(mod9)


mod10 <- lm(var_FBCF ~ var_PIB + lag5_var_PIB, data = dados_limpos[-c(12, 173, 174, 176), ])
summary(mod10)

BIC(mod10)
BIC(mod_sens)
AIC(mod10)
AIC(mod_sens)

BIC(mod9)
BIC(mod8)
