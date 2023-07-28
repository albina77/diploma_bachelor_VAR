install.packages("readxl")
install.packages("cellranger")

library(seasonal)
library(readxl)
library(ggplot2)
library(zoo)
library(reshape2)
library(vars)
library(tseries)

rm(list=ls())
setwd('/Users/Albina/Documents/Studying/НИР/preparing_to_seminar_01.03')

# ВВП, приведенный в реальное выражение, млрд руб
real_gdp <- na.omit(read_excel('VVP_kvartal_s1995(3).xls', 
                              sheet = 'Real GDP'))
real_gdp <- na.omit(real_gdp[8:80, 2])
# урезаем данные, т.к. G на сайте казнчаейства только с 4 квартала 2003 по 4 квартал 2021
real_gdp_ts <- ts(real_gdp, start=c(2003, 4), end=c(2021, 4), frequency = 4)

plot(real_gdp_ts)

# Расходы государства, взятые из источника ВШЭ (?)

real_g_old <- na.omit(read_excel('VVP_kvartal_s1995(3).xls', 
                       sheet = 'G'))
real_g_old_ts <- ts(real_g_old["G"], start=c(2002, 1), end=c(2021, 4), frequency = 4)

plot(real_g_old_ts)

# Расходы и доходы, составленные вручную с сайта казначейства
# Приведены в реальное выражение с помощью ИПЦ

real_g_new <- na.omit(read_excel('NCGGRNSAXDCRUQ.xls', sheet = 'result'))
real_t <- real_g_new["real_revenue"]
real_g_new <- real_g_new["real_spending"]
real_g_new_ts <- ts(real_g_new, start=c(2003, 4), end=c(2021, 4), frequency = 4)
real_t_ts <- ts(real_t, start=c(2003, 4), end=c(2021, 4), frequency = 4)

lines(real_g_new_ts, col=2)
lines(real_t_ts, col=3)
lines(real_gdp_ts, col=4)

# цена на нефть марки Brent в долларах, приведенная в реальное выражение с помощью 
# индекса цен производителей
real_oil_price <- na.omit(read_excel('POILBREUSDQ.xls', sheet='FRED Graph'))
real_oil_price <- real_oil_price["real_oil_price"]
real_oil_price_ts <- ts(real_oil_price, start=c(2003, 4), end=c(2021, 4), frequency = 4)

#p_oil <- na.omit(read_excel('VVP_kvartal_s1995(3).xls', 
                           sheet = 'Brent'))
#p_oil_ts <- ts(p_oil["p_real"][8:80, 1], start=c(2003, 4), end=c(2021, 4), frequency = 4)
#log_p_oil_ts <- log(p_oil_ts)
#diff_log_p_oil_ts <- diff(log_p_oil_ts)
#p_real_ts <- ts(p_oil["p_real"][8:80, 1], start=c(2003, 4), end=c(2021, 4), frequency = 4)

lines(p_oil_ts)

plot(real_oil_price_ts, col=2)

gdp <- log(final(seas(real_gdp_ts)))
g <- log(final(seas(real_g_new_ts)))
t <- log(final(seas(real_t_ts)))
oil <- log(real_oil_price_ts)

plot(gdp)
plot(g)
plot(t)
plot(oil)

library(vars)
library(tseries)

#bmat <- diag(3)
#bmat[1, 2] <- NA
#bmat[1, 3] <- NA

bmat <- diag(2)
bmat[2, 1] <- NA

#sv <- cbind(gdp, g, t)
#colnames(sv) <- cbind("GDP", "Government spending", "Government Revenue")

adf.test(sv[,1])

lagselect <- VARselect(sv, lag.max = 4, "const")
lagselect$selection

Model1 <- VAR(sv, p = 3, season = NULL, exogen = oil, type = "const")
SVARMod1 <- SVAR(Model1, Bmat = bmat, hessian = TRUE)
SVARMod1
summary(SVARMod1)

SVARgdp <- irf(SVARMod1, "GDP", "GDP")
plot(SVARgdp)
SVARgs <- irf(SVARMod1, "Government.spending", "GDP")
plot(SVARgs)
SVARog <- irf(SVARMod1, "Government.Revenue", "GDP")
plot(SVARog)

SVARMod1$Bse

Phi(SVARMod1, nstep=4)[,,1][3,][2] + Phi(SVARMod1, nstep=4)[,,2][3,][2] + Phi(SVARMod1, nstep=4)[,,3][3,][2] + Phi(SVARMod1, nstep=4)[,,4][3,][2]

