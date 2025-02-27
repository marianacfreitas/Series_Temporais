# Carregar bibliotecas necessárias
library(forecast)
library(tseries)
library(readxl)

############## QUESTÃO 4

ibovespa <- read_excel("Listas/Ibovespa.94.08.xls")

# Plotar a série temporal
plot(y=ibovespa$ibv, main = "Log-Retornos Diários do Ibovespa", ylab = "Log-Retornos")

# Analisar ACF e PACF

acf(ibovespa$ibv, main = "ACF dos Log-Retornos")
#acf mostra a correlação entre a série temporal e suas defasagens (lags)
#ajuda a identificar o q do modelo MA(q)
# q é o número de defasagens (lags) antes que o acf caia para zero ou dentro do intervalo de confiança


pacf(ibovespa$ibv, main = "PACF dos Log-Retornos")
#pacf mostra a correlação entre a série temporal e suas defasagens, após remover o efeito das defasagens intermediárias.
#ajuda a identificar o p do modelo AR(q)
# p é o número de defasagens (lags) antes que o pacf caia para zero ou dentro do intervalo de confiança

# Como não há queda no ACF, vamos realizar diferenças até ser estacionário
diferenca <- diff(ibovespa$ibv, differences = 1)
acf(diferenca, main = "ACF dos Log-Retornos")
pacf(diferenca, main = "PACF dos Log-Retornos")
adf.test(diferenca) #Se o valor-p for menor que 0.05, a série diferenciada é estacionária

auto.arima(ibovespa$ibv, seasonal = FALSE)  
modelo <- arima(ibovespa$ibv, order = c(5, 2, 0))
summary(modelo)



############ QUESTÂO 5

# Carregar pacotes necessários
library(tseries)
library(forecast)
library(rugarch)

# Obter resíduos do modelo ARMA
residuos <- residuals(modelo)

# Teste de Ljung-Box para correlação dos resíduos
Box.test(residuos, lag = 10, type = "Ljung-Box")
#Se o valor-p for maior que 0.05, os resíduos são não correlacionados

# Resíduos quadráticos
residuos_quad <- residuals(modelo)^2

# Plotar ACF e PACF dos resíduos quadráticos
acf(residuos_quad, main = "ACF dos Resíduos Quadráticos")
# s é o número de defasagens (lags) antes que o acf caia para zero ou dentro do intervalo de confiança

pacf(residuos_quad, main = "PACF dos Resíduos Quadráticos")
# r é o número de defasagens (lags) antes que o acf caia para zero ou dentro do intervalo de confiança


# Especificar o modelo GARCH(1,1)
spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                   mean.model = list(armaOrder = c(5, 2)),
                   distribution.model = "norm")

# Ajustar o modelo GARCH
garch_model <- ugarchfit(spec, data = residuos)
summary(garch_model)

#Testar se são correlacionados
residuos_garch <- residuals(garch_model, standardize = TRUE)
Box.test(residuos_garch, lag = 10, type = "Ljung-Box")

#Teste de normalidade
shapiro.test(residuos_garch)

# AIC e BIC do modelo ARMA
AIC(modelo)
BIC(modelo)

# AIC e BIC do modelo ARMA-GARCH
infocriteria(garch_model)
