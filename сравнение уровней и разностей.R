# подсчет импульскных откликов в первых разностях куммулятивным суммированием 
# сравним с SVAR в уровнях

library(seasonal)
library(readxl)
library(ggplot2)
library(zoo)
library(reshape2)
library(vars)
library(tseries)
library(dynlm)

rm(list=ls())
setwd('/Users/Albina/Documents/Studying/НИР/preparing_to_seminar_01.03')

# ВВП, приведенный в реальное выражение (2016 год), млрд руб
real_gdp <- na.omit(read_excel('VVP_kvartal_s1995(3).xls', 
                               sheet = 'Real GDP'))
real_gdp <- na.omit(real_gdp[8:80, 2])
# урезаем данные, т.к. G на сайте казнчаейства только с 4 квартала 2003 по 4 квартал 2021
real_gdp_ts <- ts(real_gdp, start=c(2003, 4), end=c(2021, 4), frequency = 4)

plot(real_gdp_ts)

# Расходы и доходы, составленные вручную с сайта казначейства
# Приведены в реальное выражение с помощью ИПЦ

real_g_new <- na.omit(read_excel('NCGGRNSAXDCRUQ.xls', sheet = 'result'))
real_g_new <- real_g_new["real_spending"]
real_g_new_ts <- ts(real_g_new, start=c(2003, 4), end=c(2021, 4), frequency = 4)

plot(real_g_new_ts)

# цена на нефть марки Brent в долларах, приведенная в реальное выражение с помощью 
# индекса цен производителей
real_oil_price <- na.omit(read_excel('POILBREUSDQ.xls', sheet='FRED Graph'))
real_oil_price <- real_oil_price["real_oil_price"]
real_oil_price_ts <- ts(real_oil_price, start=c(2003, 4), end=c(2021, 4), frequency = 4)

plot(real_oil_price_ts)

gdp <- diff(log(final(seas(real_gdp_ts))))
g <- diff(log(final(seas(real_g_new_ts))))
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
summary(SVARMod3)

Phi(SVARMod3, nstep=4)[,,1][2,][1] + Phi(SVARMod3, nstep=4)[,,2][2,][1] + Phi(SVARMod3, nstep=4)[,,3][2,][1] + Phi(SVARMod3, nstep=4)[,,4][2,][1]


Imp3 <- irf(SVARMod3, impulse = "Government.spending", response = "GDP", n.ahead = 20,
            ortho = TRUE, runs = 1000, cumulative = TRUE)
plot(Imp3)

################################################## теперь в уровнях

gdp <- log(final(seas(real_gdp_ts)))
g <- log(final(seas(real_g_new_ts)))
oil <- log(real_oil_price_ts)

library(vars)
library(tseries)

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

lagselect <- VARselect(sv, lag.max = 4, "const")
lagselect$selection

Model1 <- VAR(sv, p = 3, season = NULL, exogen = oil, type = "const")
SVARMod1 <- SVAR(Model1, Bmat = bmat, hessian = TRUE, esmethod = "scoring")
SVARMod1
summary(SVARMod1)

Phi(SVARMod1, nstep=4)[,,1][2,][1] + Phi(SVARMod1, nstep=4)[,,2][2,][1] + Phi(SVARMod1, nstep=4)[,,3][2,][1] + Phi(SVARMod1, nstep=4)[,,4][2,][1]

Imp1 <- irf(SVARMod1, impulse = "Government.spending", response = "GDP", n.ahead = 20,
            ortho = TRUE, runs = 1000)
plot(Imp1)

