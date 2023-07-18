##############  TIME SERIES FORECASTING - CONSUMO DE ENRGIA ELÉTRICA DA REGIÃO SUDESTE
############## DO BRASIL (SES, Holt-Witers, ARIMA) #################################

# Instalação e Carregamento de Todos os Pacotes ---------------------------
# Rotina prof. Rafael Souza e Prof Fávero

pacotes <- c("readr","readxl","plotly","tidyverse","gridExtra","forecast","TTR",
             "smooth", "tsibble", "fable","tsibbledata", "fpp3","lubridate",
             "urca", "dygraphs", "quantmod","BETS","tseries","FinTS",
             "gridExtra", "scales", "caret","xtable", "tsutils","GetBCBData", 
             "quantmod","dgof","seasonal", "forecast", "TSstudio")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T)
} else {
  sapply(pacotes, require, character = T)
}

####################################################################################

# Carregando base de dados e fazendo análises visuais prévias:

options(scipen = 999)
consumo <- read_excel("/Users/guilh/OneDrive/Área de Trabalho/TCC/consumo_de_energia.xlsx")
View(consumo)

ggplotly(
  consumo %>%
    mutate(Data = as.Date(Data)) %>%
    ggplot() +
    geom_line(aes(x = Data, y = consumo$`Consumo(MWh)`, color = "série")) +
    scale_color_viridis_d() +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.4),
          panel.background = element_rect(fill = "white", color = "black"),
          panel.grid = element_line(color = "grey90"),
          panel.border = element_rect(color = "black", fill = NA),
          legend.position = "none")
)

# Declarar a base de dados como uma série temporal

ts_consumo <- ts(consumo[,2], start = c(2004,1), frequency = 12)
ts_consumo
plot(ts_consumo) %>% 

# Decomposição da série temporal:

dec_ts <- decompose(ts_consumo, type = "additive")
autoplot(dec_ts) 

# Nota-se que a série temporal possui componentes de tendência e sasonalidade, além disso,
# não temos uma série estacionária

ndiffs(ts_consumo) # Somente 1 diferenciação é necessário para que a série fique estacionária 
diff_ts_consumo <- diff(ts_consumo)
ggtsdisplay(diff_ts_consumo)


######################## Modelo Holt-Winters ###########################

length(ts_consumo)
0.7*length(ts_consumo)   # 70% da base original são as primeiras 155 observações.

consumo_split <- ts_split(ts_consumo, 12) # dividir a série em treino e teste
consumo_split

treino_HW <- consumo_split$train
teste_HW <-consumo_split$test

autoplot(ts_consumo) +
  autolayer(treino_HW, series = "Treino") +
  autolayer(teste_HW, series = "Teste") +
  scale_color_viridis_d() +
  theme_bw()

consumo_HW <- HoltWinters(treino_HW)
consumo_HW          # alpha: 0,76, beta: 0, gamma: 0,82

fc_HW <- forecast(consumo_HW, 12)
accuracy(fc_HW, teste_HW)         # MAPE

test_forecast(actual = ts_consumo,
              forecast.obj = fc_HW,
              test = teste_HW)

ggtsdisplay(diff(treino_HW))
checkresiduals(consumo_HW)

par(mfrow=c(2,2))
plot(resid(consumo_HW))
qqnorm(resid(consumo_HW))
acf(resid(consumo_HW))
pacf(resid(consumo_HW))
par(mfrow=c(1,1))

# Plotando os vaores previstos e ajustados nos da um melhor entendimento da performance do modelo
# Nota-se que o modelo Holt-Winters fez um bom trabalho em capturar tando as componentes sasonais
# como a de tendência. Porém pode-se notar que o modelo em alguns pontos de picos não acompanha
# os calores reais da base de dados.

# De um modo alternativo, o modelo pode ser treinado de uma maneira em que se otimize os 
# parâmentros alfa, beta e gamma. O "grid search" é uma boa ferramenta para otimizar os
# ajustes desses parêmntros. O algoritimo é baseado em aplicar um conjunto de vaores para cada
# parametro o modelo e fazer um teste com diferentes combinações do modelo de modo que se busque
# a minimização dos critérios de erro que será utilizada no modelo final.

######### Começando com uma busca rasa (shallow search)

shallow_grid <- ts_grid(treino_HW,
                        model = "HoltWinters",
                        periods = 6,
                        window_space = 6,
                        window_test = 12,
                        hyper_params = list(alpha = seq(0,1,0.1),
                                            beta = seq(0,1,0.1),
                                            gamma = seq(0,1,0.1)),
                        parallel = TRUE,
                        n.cores = 8
                       )

shallow_grid$grid_df[1:10,]

plot_grid(shallow_grid)

# O plot_grid nos mostra uma visão intuitiva de um intervalo ideal de valores para cada
# parâmentro. Por padrão, a função esta evidenciando os melhores modelos (10%)
# alfa (0.1-0.2)/ beta (0-0.2)/ gamma (0.3-0.9)
# Assim pode-se aplicar os intervalos dos hiperparametros para um grid search mais profundo

deep_grid <- ts_grid(treino_HW,
                     model = "HoltWinters",
                     periods = 6,
                     window_space = 6,
                     window_test = 12,
                     hyper_params = list(alpha = seq(0.1,0.2,0.01),
                                         beta = seq(0,0.2,0.01),
                                         gamma = seq(0.3,0.9,0.01)),
                     parallel = TRUE,
                     n.cores = 8)

plot_grid(deep_grid)  # Nota-se que o alcance do erro dos 10% dos modelos caiu

# O último passo do proceso é aplicar os valores ótimos de parâmentros de suavização do
# grid model baseado nesta busca:

consumo_HW_grid <- HoltWinters(treino_HW,
                               alpha = deep_grid$alpha,
                               beta = deep_grid$beta,
                               gamma = deep_grid$gamma)

fc_HW_grid <- forecast(consumo_HW_grid, h= 12)

accuracy(fc_HW_grid, teste_HW)         

# Utilizando o grid search pode-se ver que o teste de acurácia nos mostra melhores valores
# de performance com a redução do MAPE (erro absoluto percentual médio) de 3,77% para 2,12%

test_forecast(actual = ts_consumo,
              forecast.obj = fc_HW_grid,
              test = teste_HW)

plot_forecast(fc_HW_grid,
              title = "Consumo de energia elétrica do Sudeste",
              Ytitle = "Consumo (MWh)",
              Xtitle = "Ano"
             )  

############# Modelos de Suavização Exponencial Simples (SES) ##########################

treino_SES <- consumo_split$train
teste_SES <- consumo_split$test

fc_consumo_SES <- ses(treino_SES, h = 12, initial = "optimal")
fc_consumo_SES$model

accuracy(fc_consumo_SES, teste_SES)

ggtsdisplay(diff(treino_SES))
checkresiduals(fc_consumo_SES)

par(mfrow=c(2,2))
plot(resid(fc_consumo_SES))
qqnorm(resid(fc_consumo_SES))
acf(resid(fc_consumo_SES))
pacf(resid(fc_consumo_SES))
par(mfrow=c(1,1))

test_forecast(actual = ts_consumo,
              forecast.obj = fc_consumo_SES,
              test = teste_SES)


# Como podemso ver através da plotagem da previsão, a função SES utiliza o treino para
# identificaro nível da série estimando o prametro alpha e o valor inicial do modelo.
# O nível do valor previsto é bem próximo do valor da última observação da série, uma vez que,
# o valor de alpha, neste caso é próximo a 1 (alpha = 0.9449). Como o objetivo de modelo SES
# é prever o nível da série, o modelo não captura as oscilações a curto-prazo de tendencia e sasonalidade.

# Em caso de uma previsão plana, os intervalor de confiança fazem um papel importante, uma vez que
# o nível de incerteza é maior.

plot_forecast(fc_consumo_SES) %>% 
  add_lines(x = time(teste_SES) + deltat(teste_SES),
            y = as.numeric(teste_SES),
            name = "Partição de teste") %>% 
  layout(title = "Consumo de energia elétrica (MWh)",
         xaxis = list(range = c(2004, max(time(ts_consumo)) +
         deltat(ts_consumo))),
         yaxis = list(range = c(10000000,25000000)))

# Como pode-se notar pela plotagem da previsão, o teste está dentro do intervalo de confiança de 80%  

######################### MODELO ARIMA ###########################

treino_ARIMA <- consumo_split$train
teste_ARIMA <- consumo_split$test

# A função auto.arima retorna o modelo ARIMA que minimiza os valores AIC, Neste caso
# temos um modelo SARIMA(1,1,1)(0,1,1)[12] selecionado com um valor AIC = AIC=5671.57.
# Lembrando que essa função automaticamente calcular o número de diferenciações necessárias para 
# que nossa série seja estacionária.

consumo_ARIMA <- auto.arima(treino_ARIMA,
                            d=1,
                            D=1,
                            stepwise = FALSE,
                            approximation = FALSE,
                            trace = TRUE)

consumo_ARIMA
checkresiduals(consumo_ARIMA)

# Teste de Ljung-Box p-value = 0.1465 > 0.05, aceitamos H0, resíduos não são correlacionados
# a um nivél de signicncia de 5%.
#H0:	O modelo não exibe falha de ajuste.
#Ha:	O modelo exibe falha de ajuste.

fc_consumo_ARIMA <- forecast(consumo_ARIMA, h = 12)
accuracy(fc_consumo_ARIMA, teste_ARIMA)

test_forecast(ts_consumo,
              forecast.obj = fc_consumo_ARIMA,
              test = teste_ARIMA)

autoplot(fc_consumo_ARIMA)

fc_consumo_ARIMA_h48 <- forecast(consumo_ARIMA, h = 48)

autoplot(fc_consumo_ARIMA_h48)

ArchTest(fc_consumo_ARIMA$residuals)

##################### MODELOS SNAIVE ###########################

treino_snaive <- consumo_split$train
teste_snaive <- consumo_split$test

consumo_snaive <- snaive(treino_snaive, h=12)
consumo_snaive

fc_consumo_snaive <- forecast(consumo_snaive, h = 12)
autoplot(fc_consumo_snaive)

test_forecast(ts_consumo,
              forecast.obj = fc_consumo_snaive,
              test = teste_ARIMA)

accuracy(fc_consumo_snaive, teste_snaive)

############ Previsão por modelos de regressão linear ########################

#A funçao tslm do pacote forecast faz uma transformação do objeto
# de série temporal em um modelo de regressão linear. Usando a função tslm, pode-se
# aplicar o componente de regressão junto dos outros recursos. Sendo assim, faremos uma 
# previsão das últimas 12 observações da série temporal usando tendência, o quadrado da tendência e
# o componente sasonal

treino_tslm <- consumo_split$train
teste_tslm <- consumo_split$test

consumo_tslm <- tslm(treino_tslm ~ season + trend + I(trend^2))
summary(consumo_tslm)

checkresiduals(consumo_tslm)

fc_tslm <- forecast(consumo_tslm, h =12)
autoplot(fc_tslm)

test_forecast(ts_consumo,
              forecast.obj = fc_tslm,
              test = teste_tslm)

accuracy(fc_tslm, teste_tslm)

################## Usando modelo ETS # EXPONENTIAL SMOOTHING ##################

# E (Error) T (Trend) S (Season)
# Erro: aditivo (A) ou multiplicativo (M)
# Tendência: nenhuma (N), aditiva (A), multiplicativa (M) ou amortecida (Ad ou Md)
# Sazonalidade: nenhuma (N), aditiva (A) ou multiplicativa (M)

treino_ets <- consumo_split$train
teste_ets <- consumo_split$test

consumo_ets <- ets(treino_ets)
summary(consumo_ets)

fc_ets <- forecast(consumo_ets, h =12)
accuracy(fc_ets, teste_ets)

test_forecast(ts_consumo,
              forecast.obj = fc_ets,
              test = teste_ets)

############ Neural Network Time Series #######################

# O uso da função nnetar nos da um modelo de uma previsão de série tempoaral utilizando
# uma rede neural de Feed_forward com uma única camada escondida e os lags de entrada


treino_rn <- consumo_split$train
teste_rn <- consumo_split$test

consumo_rn <- nnetar(treino_rn)    #NNAR(2,1,2)[12] 
consumo_rn                         
summary(consumo_rn)

fc_rn <- forecast(consumo_rn, h = 12)
autoplot(fc_rn)
accuracy(fc_rn, teste_rn)

test_forecast(ts_consumo,
              forecast.obj = fc_rn,
              test = teste_rn)

# Comparando os modelos SES, Holt-Winters, ARIMA, snaive, tslm, ETS e RNN

accuracy(fc_HW_grid, teste_HW)   
accuracy(fc_consumo_SES, teste_SES)
accuracy(fc_consumo_ARIMA, teste_ARIMA)
accuracy(fc_consumo_snaive, teste_snaive)
accuracy(fc_tslm, teste_tslm)
accuracy(fc_ets, teste_ets)
accuracy(fc_rn, teste_rn)


