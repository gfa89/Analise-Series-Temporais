#
# TCC - Forecast de uma série temporal do consumo de energia elétrica da região 
# Sudeste do Brasil

library(scales)
library(readxl)
library(forecast)
library(fpp2)


data <- read_excel("/Users/guilh/OneDrive/Área de Trabalho/TCC/consumo_de_energia.xlsx")
View(data)

# Declarar a base de dados como uma série temporal

Y <- ts(data[,2], start = c(2004,1), frequency = 12)
Y

############################################################
# ANÁLISES PRELIMINARES #
############################################################

options(scipen = 999)
autoplot(Y) + 
  ggtitle("Consumo de energia elétrica do Sudeste do Brasil") +
  ylab("MWh") + xlab("Ano")

# Nota-se forte tendência nos dados. Investigar as tranformações:

ndiffs(Y) # avaliar a quantidade de diferenciações necessárias
DY <- diff(Y,1)

autoplot(DY) + 
  ggtitle("Consumo de energia elétrica do Sudeste do Brasil coom diferenciação") +
  ylab("MWh") + xlab("Ano")

# A série aparenta estar estacionária. Investigar sasonalidade:

ggseasonplot(DY) +
  ggtitle("Plot de sasonalizade") +
  ylab("MWh") + xlab("Mês")

options(scipen = 999)
ggsubseriesplot(Y)

# BoxCox: se a diferenciação não for o bastante, pode ser usada a 
## transformação de BoxCox.

lambda <- BoxCox.lambda(Y)
Y.bc <- BoxCox(Y, lambda = lambda)
hist(Y) # antes
hist(Y.bc) # atual
autoplot(Y.bc)
ap1 <- autoplot(Y)
ap2 <- autoplot(Y.bc)
ap1 + ap2
ap1 / ap2


serie.final <- diff(Y.bc, 1)
autoplot(serie.final)

acf(Y)
pacf(Y)

plot(diff(log(Y.bc)))

Yforecast <- HoltWinters(DY, beta = FALSE, gamma = FALSE)
Yforecast
plot(Yforecast)

Yforecast2 <- forecast(Yforecast, h=24)
Yforecast2



plotForecastErrors(Yfor0$residuals) # gerando um histograma.

##########################

# Holt-Winters Suavização Exponencial

# Caso tenha uma série que pode ser descrita por meio de modelos aditivos, tendência crescente
# ou decrescente e sazonalidade, o uso da suavização exponencial de Holt-Winders é indicada 
# para previsões de curto prazo

# Estima o nível, inclinação e componente sazonal no ponto de tempo atual. A suavização é
# controlada por três parâmetros: alfa, beta e gama para estimar o nível, inclinação e o 
# componente de tendência e sazonal a partir do ponto atual. Os parâmetros variam entre 0 e 1.
# Valores próximos a 0 significam que é colocado relativamente pouco peso nas observações mais 
# recentes ao fazer as previsões.

logY <- log(Y)
plot(logY)
logYforecast <- HoltWinters(logY)
logYforecast

# Os valores estimados de alfa, beta e gama são 0.71, 0.00 e 0.95. O alfa é relativamente baixo
# indicando que a estimativa do nível no momento atual é baseada em observações no passado mais
# distante. O valor de beta indica que a estimativa da inclinação b do componente de tendência não
# é atualizado ao longo da série temporal e, em vez disso, é definida igual ao valor inicial. Assim,
# o nível muda bastante ao longo da série temporal, mas a inclinaçào do componente de tendência
# permanece praticamente a mesma. Já o valor gama é alto, indicandp que a estimativa do componente
# sazonal no momento atual é baseada apenas em observações recentes.

plot(logYforecast)

# A técnica consegue prever os picos sazonais que ocorrem nos meses finais do ano.
# Vamos agora prever períodos que não estão na base, ou seja, de 1994 a 1998 (48 m)

logYforecast2 <- forecast(logYforecast, h=48)
plot(logYforecast2)

acf(logYforecast2$residuals, lag.max = 20, na.action = na.pass)
Box.test(logYforecast2$residuals, lag = 20, type = "Ljung-Box")

plot.ts(logYforecast2$residuals)
plotForecastErrors(logYforecast2$residuals)

####################

prev.naive <- naive(Y, h = 48)
autoplot(prev.naive)

prev.snaive <- snaive(Y, h = 48)
print(summary(prev.snaive))

fcst.snaive <- forecast(prev.snaive, h=48)
autoplot(fcst.snaive)


dec.Y <- decompose(Y, type = "multiplicative")
autoplot(dec.Y)

acf(DY)
pacf(DY, lag.max = 20)

Yarima <- arima(Y)
Yarima

Yarimaforecast <- forecast(Yarima, h = 48)
Yarimaforecast
autoplot(Yarimaforecast)

##########

Yarima2 <- auto.arima(Y, d=1, D=1, stepwise = FALSE, approximation = FALSE, trace = TRUE)
print(summary(Yarima2))
checkresiduals(Yarima2)
ArchTest(Yarima2$residuals)

fcst <- forecast(Yarima2, h=60)
autoplot(fcst)
options(scipen = 999)
plot(fcst)

acf(fcst$residuals, lag.max = 20, na.action = na.pass)
Box.test(fcst$residuals, lag = 20, type = "Ljung-Box")

adf.test(Y)   ####### dickey-fuller test
adf.test(DY)

# Holt-Winters

YHW <- hw(Y, seasonal = "multiplicative", h = 60)  ### GRAFICO AULA 3 2:07 (tempo)
summary(YHW)
autoplot(YHW)

dif12 <- diff(Y,12)
ts.plot(dif12)

##### Comparando

fcst['model'] #ARIMA
YHW['model'] # Holt-Winters

# Acurácia do modelo

accuracy(fcst) # ARIMA
accuracy(YHW) # Holt-Winters

checkresiduals(DY)
acf(DY)
pacf(DY)

logY <- log(Y)
arima.logY <- auto.arima(logY, d=1, D=1, stepwise = FALSE, approximation = FALSE, trace = TRUE)
arima.logY
print(summary(arima.logY))
checkresiduals(arima.logY)

acf(arima.logY$residuals)
pacf(arima.logY$residuals)
