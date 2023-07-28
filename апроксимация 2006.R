# попробуем апроксимировать 2006 год с помощью МА

library(seasonal)
library(readxl)
library(ggplot2)
library(zoo)
library(reshape2)
library(vars)
library(tseries)
library(dynlm)
library("rugarch")
library(mFilter)


rm(list=ls())
setwd('/Users/Albina/Documents/Studying/НИР/preparing_to_seminar_01.03')

real_g_new <- na.omit(read_excel('NCGGRNSAXDCRUQ.xls', sheet = 'result'))
real_g_new <- real_g_new["real_spending"]
real_g_new_ts <- ts(real_g_new[11:73, 1], start=c(2006, 2), end=c(2021, 4), frequency = 4)

real_g_old <- na.omit(read_excel('VVP_kvartal_s1995(3).xls', sheet = 'G'))
real_g_old_ts <- ts(real_g_old["G"][8:17, 1], start=c(2003, 4), end=c(2006, 1), frequency = 4)

real_g_merged <- ts(c(real_g_old_ts, real_g_new_ts), start = start(real_g_old_ts), frequency = frequency(real_g_old_ts))
plot(real_g_merged)
axis(1, at = seq(2003, 2021, by = 1))

real_g_merged <- final(seas(real_g_merged))

plot(real_g_merged)

acf(real_g_merged)
pacf(real_g_merged)


spec.ar1 = arfimaspec(mean.model=list(armaOrder=c(1,1), include.mean=FALSE, arfima=FALSE))

# оценим модель AR(1)
fit.ar1 = arfimafit(spec=spec.ar1,data=real_g_merged)
# результаты оценки
fit.ar1
# остатки
res.ar1 = fit.ar1@fit$residuals
plot(res.ar1)

# строим ACF 
res.acf1 = acf(res.ar1)
res.acf1

pacf_spred=pacf(res.ar1)
pacf_spred

Box.test(res.ar1,lag=12,type="Ljung-Box")

arma <- real_g_merged - res.ar1

plot(real_g_merged)
lines(arma, col=2)

# попробуем тупа сгладить

sma4 <- (0.5*lag(real_g_merged, 2) + lag(real_g_merged, 1) + real_g_merged + lag(real_g_merged, -1) + 0.5*lag(real_g_merged, -2)) / 4
lines(sma4, col=3)

sma8 <- (0.5*lag(real_g_merged, 4) + lag(real_g_merged, 3) + lag(real_g_merged, 2) + lag(real_g_merged, 1) + 
            real_g_merged + lag(real_g_merged, -1) + lag(real_g_merged, -2) + lag(real_g_merged, -3) + 0.5*lag(real_g_merged, -4)) / 8
lines(sma8, col=6)

hpf14400 <- hpfilter(real_g_merged,freq = 1600)

lines(hpf14400$trend, col=4)

bkf <- bkfilter(real_g_merged, pu=32, pl = 6, nfix = 12)

lines(bkf$trend, col=5)

legend(x = "bottomright",   legend = c("Original", "ARMA(1,3)", "SMA4", "SMA8", "HP_1600", "BK_12"),  
       lty = c(1, 1, 1, 1, 1, 1),  
       col = c(1, 2, 3, 4, 5, 6),  
       lwd = 1) 
axis(1, at = seq(2003, 2021, by = 1))


# возьмем за 2006 год значения из sma4

real_g_merged[10:13] <- sma4[8:11]

# теперь построим мультипликатор
# будем все-таки смотреть в разностях

# ВВП, приведенный в реальное выражение (2016 год), млрд руб
real_gdp <- na.omit(read_excel('VVP_kvartal_s1995(3).xls', 
                               sheet = 'Real GDP'))
real_gdp <- na.omit(real_gdp[8:80, 2])
# урезаем данные, т.к. G на сайте казнчаейства только с 4 квартала 2003 по 4 квартал 2021
real_gdp_ts <- ts(real_gdp, start=c(2003, 4), end=c(2021, 4), frequency = 4)

plot(real_gdp_ts)

# цена на нефть марки Brent в долларах, приведенная в реальное выражение с помощью 
# индекса цен производителей
real_oil_price <- na.omit(read_excel('POILBREUSDQ.xls', sheet='FRED Graph'))
real_oil_price <- real_oil_price["real_oil_price"]
real_oil_price_ts <- ts(real_oil_price, start=c(2003, 4), end=c(2021, 4), frequency = 4)

plot(real_oil_price_ts)

gdp <- diff(log(final(seas(real_gdp_ts))))
g <- diff(log(real_g_merged))
oil <- diff(log(real_oil_price_ts))

bmat <- diag(2)
bmat[2, 1] <- NA

sv3 <- cbind(g, gdp)
colnames(sv3) <- cbind("Government spending", "GDP")

# ur.df проводит тестирование Dickey-Fuller
summary(ur.df(gdp,  type = "none", selectlags = "AIC"))
summary(ur.df(g,  type = "drift", selectlags = "AIC"))
# отвергаем H0

#Проверка наличия коинтеграции. Включать константу "-1" или нет  - вопрос дискуссионный.
cointcy <- dynlm(gdp ~ g )
summary(cointcy)
#Сохраняем ошибку из регрессии (gdp ~ g) и проверяем стационарна ли она.
ehat <- resid(cointcy)
summary(ur.df(ehat,  type = "none", selectlags = "AIC"))
summary(adf.test(sv3[,2]))

lagselect <- VARselect(sv3, lag.max = 4, "const")
lagselect$selection

Model3 <- VAR(sv3, p = 3, season = NULL, exogen = oil, type = "const")
SVARMod3 <- SVAR(Model3, Bmat = bmat, hessian = TRUE, esmethod = "scoring")
SVARMod3

Imp3 <- irf(SVARMod3, impulse = "Government.spending", response = "GDP", n.ahead = 20,
            ortho = TRUE, runs = 1000, cumulative = TRUE)
plot(Imp3)

s = 0
for(i in 1:4){
  s = s + Imp3[1]$irf$Government.spending[i]
}
Imp3[1]$irf$Government.spending
s

# попробуем все-таки MA, как завещал Зубарев

real_g_new <- na.omit(read_excel('NCGGRNSAXDCRUQ.xls', sheet = 'result'))
real_g_new <- real_g_new["real_spending"]
real_g_new_ts <- ts(real_g_new, start=c(2003, 4), end=c(2021, 4), frequency = 4)

plot(real_g_new_ts)

real_g_merged <- final(seas(real_g_new_ts))

plot(real_g_merged)

acf(real_g_merged)
pacf(real_g_merged)


spec.ar1 = arfimaspec(mean.model=list(armaOrder=c(0,12),include.mean=FALSE, arfima=FALSE))

# оценим модель AR(1)
fit.ar1 = arfimafit(spec=spec.ar1,data=real_g_merged)
# результаты оценки
fit.ar1
# остатки
res.ar1 = fit.ar1@fit$residuals

# строим ACF 
res.acf1 = acf(res.ar1)
res.acf1

pacf_spred=pacf(res.ar1)
pacf_spred

Box.test(res.ar1,lag=12,type="Ljung-Box")

arma <- real_g_merged - res.ar1

plot(real_g_merged)
lines(arma, col=2)

plot(res.ar1)

#################################################################################
# Заменим данные за 2006 год, тк слишком выбивается из графика

real_g_new <- na.omit(read_excel('NCGGRNSAXDCRUQ.xls', sheet = 'result'))
real_g_new <- real_g_new["real_spending"]
real_g_new_ts <- ts(real_g_new[11:73, 1], start=c(2006, 2), end=c(2021, 4), frequency = 4)

real_g_old <- na.omit(read_excel('VVP_kvartal_s1995(3).xls', sheet = 'G'))
real_g_old_ts <- ts(real_g_old["G"][8:17, 1], start=c(2003, 4), end=c(2006, 1), frequency = 4)

real_g_merged <- ts(c(real_g_old_ts, real_g_new_ts), start = start(real_g_old_ts), frequency = frequency(real_g_old_ts))

mymodel <- auto.arima(real_g_merged)

# получили ARMA(1,0,1)(0,1,1)[4], где вторые 3 цифры относятся к сезонным коэффициентам

s <- sarima(real_g_merged, 1, 0, 1, 0, 1, 1, 4)
arma <- real_g_merged - s$fit$residuals
real_g_merged[10:13] <- arma[14:17] # потому что модель получилась почти точно сдвинута на один год


#################################################################################
#добавим данные за 2022

setwd('/Users/Albina/Documents/Studying/НИР/Отчеты')
q1 <- read_excel('2022.04.01.xls')
q1 <- q1[9:18, c(1, 3)]

q2 <- read_excel('2022.07.01.xls')
q2 <- q2[9:18, c(1, 3)]
q2[,2] <- q2[,2] - q1[,2]

q1[,2] <- q1[,2]/1.336050007/1.099468405
q2[,2] <- q2[,2]/1.336050007/1.099468405/1.013259854


g_merged <- ts(c(real_g_merged, ts(q1[10, 2], start = c(2022, 1)), ts(q2[10, 2], start = c(2022, 2))), start=start(real_g_merged), frequency = 4)


library(forecast)
mymodel <- auto.arima(g_merged)
mymodel
plot.ts(mymodel$residuals)
pacf(mymodel$residuals)

myforecast <- forecast(mymodel, level=c(95), h=1*4)
plot(myforecast)
axis(1, at = seq(2003, 2022, by = 1))
q4_ts <- ts(q4[10, 2], start = c(2022, 4), end = c(2022, 4), frequency = 4)

q4 <- read_excel('2023.01.01.xls')
q4 <- q4[9:18, c(1, 3)]


q4[,2] <- q4[,2]/1.336050007/1.099468405/1.013259854/0.99141574/1.013349612
q4_ts <- ts(q4[10, 2] - myforecast$mean[1] - q1[10, 2] - q2[10, 2], start = c(2022, 4), end = c(2022, 4), frequency = 4)
points(q4_ts, col=2)

g_merged <- ts(c(g_merged, myforecast$mean[1], q4_ts), start = start(g_merged), frequency = 4)
g_merged
plot(diff(log(final(seas(g_merged)))))

#################################################################################
