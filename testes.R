rnn <- nnetar(ts_consumo)
rnn

fc_rnn <- forecast(ts_consumo, h=12)
autoplot(fc_rnn)
accuracy(fc_rnn)
checkresiduals(fc_rnn)

methods <- list(ets = list(method = "ets",
                            method_arg = list(opt.crit = "lik"),
                            notes = "ETS model with opt.crit = lik"),
                arima = list(method = "arima",
                              method_arg = list(order = c(1,1,1),
                                                seasonal = list(order = c(0,1,1))),
                              notes = "SARIMA(1,1,1)(0,1,1)"),
                hw = list(method = "HoltWinters",
                          method_arg = NULL,
                          notes = "HoltWinters Model"),
                tslm = list(method = "tslm",
                            method_arg = list(formula = input ~ trend + season),
                            notes = "tslm model with trend and seasonal components"))


md <- train_model(input = ts_consumo,
                  methods = methods,
                  train_method = list(partitions = 4,
                                      sample.out = 12,
                                      space = 3),
                  horizon = 12,
                  error = "MAPE")
plot_error(md)




par(mfrow=c(1,2))
acf(ts_consumo)
pacf(ts_consumo , lag.max = 24)
par(mfrow=c(1,1))