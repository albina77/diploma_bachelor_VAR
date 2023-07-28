# 5 - добавим дамми и нефть с лагами

library(seasonal)
library(readxl)
library(ggplot2)
library(zoo)
library(reshape2)
library(vars)
library(tseries)
library(dynlm)
library(urca)

rm(list=ls())
setwd('/Users/Albina/Documents/Studying/НИР/preparing_to_seminar_01.03')

# ВВП, приведенный в реальное выражение (2016 год), млрд руб
real_gdp <- na.omit(read_excel('VVP_kvartal_s1995(3).xls', sheet = 'Real GDP'))
real_gdp <- na.omit(real_gdp[8:82, 2])
# урезаем данные, т.к. G на сайте казнчаейства только с 4 квартала 2003 по 4 квартал 2022
real_gdp_ts <- ts(real_gdp, start=c(2003, 4), end=c(2022, 2), frequency = 4)

plot(final(seas(real_gdp_ts)))

# Расходы и доходы, составленные вручную с сайта казначейства
# Приведены в реальное выражение с помощью ИПЦ

real_g_new <- read_excel('NCGGRNSAXDCRUQ.xls', sheet = 'FRED Graph')
real_g_new <- na.omit(real_g_new["real_spending_deflator"])
real_g_new_ts <- ts(real_g_new[11:75, 1], start=c(2006, 2), end=c(2022, 2), frequency = 4)

real_g_old <- na.omit(read_excel('VVP_kvartal_s1995(3).xls', sheet = 'G'))
real_g_old_ts <- ts(real_g_old["g_deflator"], start=c(2003, 4), end=c(2006, 1), frequency = 4)

g_merged <- ts(c(real_g_old_ts, real_g_new_ts), start = start(real_g_old_ts), frequency = frequency(real_g_old_ts))
plot(final(seas(g_merged)))

# цена на нефть марки Brent в долларах, приведенная в реальное выражение с помощью 
# индекса цен производителей
real_oil_price <- read_excel('POILBREUSDQ.xls', sheet='FRED Graph')

# приведено в реальное выражение с помощью Consumer Price Index for All Urban Consumers
# CPIAUCSL

seas_real_oil_price_cpi <- real_oil_price["seas_real_oil_cpi"][2:80,]
seas_real_oil_price_cpi_ts <- ts(seas_real_oil_price_cpi, start=c(2003, 2), end=c(2022, 2), frequency = 4)

plot(seas_real_oil_price_cpi_ts)

# исключаем сезонность и берем логарифм

gdp <- log(final(seas(real_gdp_ts)))
g <- log(final(seas(g_merged)))
oil <- log(seas_real_oil_price_cpi_ts)
oil_1 <- lag(oil, -1)
oil_2 <- lag(oil, -2)
oil_to_3 <- na.omit(cbind(oil, oil_1, oil_2))

seas_real_oil_price_cpi <- real_oil_price["seas_real_oil_cpi"][3:80,]
seas_real_oil_price_cpi_ts <- ts(seas_real_oil_price_cpi, start=c(2003, 3), end=c(2022, 2), frequency = 4)
oil <- log(seas_real_oil_price_cpi_ts)
oil_1 <- lag(oil, -1)
oil_to_2 <- na.omit(cbind(oil, oil_1))

seas_real_oil_price_cpi <- real_oil_price["seas_real_oil_cpi"][4:80,]
seas_real_oil_price_cpi_ts <- ts(seas_real_oil_price_cpi, start=c(2003, 4), end=c(2022, 2), frequency = 4)
oil <- log(seas_real_oil_price_cpi_ts)

dummy_2006 <- ts(numeric(1), start = c(2003, 4), end = c(2022, 2), frequency = 4)
dummy_2006[12] <- 1

# оценим модель в уровнях

library(vars)
library(tseries)

# задаем вид матрицы при ошибках, NA кладем на место коэффициента, который хотим оценить

bmat <- diag(2)
bmat[2, 1] <- NA

sv <- cbind(g, gdp)
colnames(sv) <- cbind("Government spending", "GDP")

# ur.df проводит тестирование Dickey-Fuller
summary(ur.df(gdp,  type = "none", selectlags = "AIC"))
summary(ur.df(g,  type = "drift", selectlags = "AIC"))
#Не отвергаем H0

#Проверка наличия коинтеграции. Включать константу "-1" или нет  - вопрос дискуссионный.
cointcy <- dynlm(gdp ~ g )
summary(cointcy)
#Сохраняем ошибку из регрессии (gdp ~ g) и проверяем стационарна ли она.
ehat <- resid(cointcy)
summary(ur.df(ehat,  type = "none", selectlags = "AIC"))
summary(adf.test(sv[,2]))

lagselect <- VARselect(sv, lag.max = 4, type = "const", exogen = cbind(oil, dummy_2006))
lagselect$selection

Model1 <- VAR(sv, p = 2, season = NULL, type = "const", exogen = cbind(oil, dummy_2006))
SVARMod1 <- SVAR(Model1, Bmat = bmat, hessian = TRUE, estmethod = "scoring")
SVARMod1
summary(SVARMod1)

# считаем значение мультипликатора за год

Phi(SVARMod1, nstep=4)[,,1][2,][1] + Phi(SVARMod1, nstep=4)[,,2][2,][1] + Phi(SVARMod1, nstep=4)[,,3][2,][1] + Phi(SVARMod1, nstep=4)[,,4][2,][1]

Imp1 <- irf(SVARMod1, impulse = "Government.spending", response = "GDP", n.ahead = 20,
            ortho = TRUE, runs = 1000, cumulative = FALSE)
plot(Imp1)

s = 0
for(i in (1:8)){
  s = s + Imp1$irf$Government.spending[i]
}
s
