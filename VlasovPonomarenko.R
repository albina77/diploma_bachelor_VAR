# статься Власов Пономаренко (2010)
# добавляем налоги

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
real_gdp <- na.omit(read_excel('VVP_kvartal_s1995(3).xls', sheet = 'Real GDP'))
real_gdp <- na.omit(real_gdp[8:82, 2])
# урезаем данные, т.к. G на сайте казнчаейства только с 4 квартала 2003 по 4 квартал 2022
real_gdp_ts <- ts(real_gdp, start=c(2003, 4), end=c(2022, 2), frequency = 4)

plot(final(seas(real_gdp_ts)))

# Расходы и доходы, составленные вручную с сайта казначейства
# Приведены в реальное выражение с помощью ИПЦ

real_g_new <- read_excel('NCGGRNSAXDCRUQ.xls', sheet = 'FRED Graph')
real_t <- na.omit(real_g_new["real_revenue_deflator"])
real_t_ts <- ts(real_t[1:79, 1], start=c(2003, 4), end=c(2022, 2), frequency = 4)

real_g_new <- na.omit(real_g_new["real_spending_deflator"])
real_g_new_ts <- ts(real_g_new[11:73, 1], start=c(2006, 2), end=c(2022, 2), frequency = 4)

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
t <- log(final(seas(real_t_ts)))
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
bmat <- diag(3)
bmat[3, 2] <- NA
bmat[3, 1] <- NA
bmat[2, 1] <- NA

sv3 <- cbind(g, t, gdp)
colnames(sv3) <- cbind("Government spending", "Government revenue", "GDP")

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

lagselect <- VARselect(sv3, lag.max = 4, "const", exogen = na.omit(cbind(oil, dummy_2006)))
lagselect$selection

Model3 <- VAR(sv3, p = 1, season = NULL, exogen = na.omit(cbind(oil, dummy_2006)), type = "const")
SVARMod3 <- SVAR(Model3, Bmat = bmat, hessian = TRUE, esmethod = "scoring")
SVARMod3
#summary(SVARMod3)

#Phi(SVARMod3, nstep=4)[,,1][2,][1] + Phi(SVARMod3, nstep=4)[,,2][2,][1] + Phi(SVARMod3, nstep=4)[,,3][2,][1] + Phi(SVARMod3, nstep=4)[,,4][2,][1]


Imp3 <- irf(SVARMod3, impulse = "Government.spending", response = "GDP", n.ahead = 20, ortho = TRUE, runs = 1000, cumulative = FALSE)
plot(Imp3)

Imp4 <- irf(SVARMod3, impulse = "Government.revenue", response = "GDP", n.ahead = 20, ortho = TRUE, runs = 1000, cumulative = FALSE)
plot(Imp4)


Imp3$irf$Government.spending[1] + Imp3$irf$Government.spending[2] + Imp3$irf$Government.spending[3] + Imp3$irf$Government.spending[4] + Imp3$irf$Government.spending[5] + Imp3$irf$Government.spending[6] + Imp3$irf$Government.spending[7] + Imp3$irf$Government.spending[8] + Imp3$irf$Government.spending[9] + Imp3$irf$Government.spending[10] + Imp3$irf$Government.spending[11] + Imp3$irf$Government.spending[12]


################################################################################################

g_by_categories <- read_excel('/Users/Albina/Documents/Studying/НИР/calculations/g_by_categories_subtracted.xlsx', sheet='real')

g_by_categories_ts <- ts(g_by_categories, start=c(2005, 1), end=c(2022, 1), frequency = 4)
g_by_categories_1 <- ts(g_by_categories['ОБЩЕГОСУДАРСТВЕННЫЕ ВОПРОСЫ'], start=c(2005, 1), end=c(2022, 2), frequency = 4)
plot(g_by_categories_1)
g_by_categories_2 <- ts(g_by_categories['НАЦИОНАЛЬНАЯ ОБОРОНА'], start=c(2005, 1), end=c(2022, 2), frequency = 4)
plot(g_by_categories_2)
g_by_categories_3 <- ts(g_by_categories['НАЦИОНАЛЬНАЯ БЕЗОПАСНОСТЬ И ПРАВООХРАНИТЕЛЬНАЯ ДЕЯТЕЛЬНОСТЬ'], start=c(2005, 1), end=c(2022, 2), frequency = 4)
plot(g_by_categories_3)
g_by_categories_4 <- ts(g_by_categories['НАЦИОНАЛЬНАЯ ЭКОНОМИКА'], start=c(2005, 1), end=c(2022, 2), frequency = 4)
plot(g_by_categories_4)
g_by_categories_5 <- ts(g_by_categories['ЖИЛИЩНО-КОММУНАЛЬНОЕ ХОЗЯЙСТВО'], start=c(2005, 1), end=c(2022, 2), frequency = 4)
plot(g_by_categories_5)
g_by_categories_6 <- ts(g_by_categories['ОХРАНА ОКРУЖАЮЩЕЙ СРЕДЫ'], start=c(2005, 1), end=c(2022, 2), frequency = 4)
plot(g_by_categories_6)
g_by_categories_7 <- ts(g_by_categories['ОБРАЗОВАНИЕ'], start=c(2005, 1), end=c(2022, 2), frequency = 4)
plot(g_by_categories_7)
g_by_categories_8 <- ts(g_by_categories['КУЛЬТУРА, КИНЕМАТОГРАФИЯ И СРЕДСТВА МАССОВОЙ ИНФОРМАЦИИ'], start=c(2005, 1), end=c(2022, 2), frequency = 4)
plot(g_by_categories_8)
g_by_categories_9 <- ts(g_by_categories['ЗДРАВООХРАНЕНИЕ, ФИЗИЧЕСКАЯ КУЛЬТУРА И СПОРТ'], start=c(2005, 1), end=c(2022, 2), frequency = 4)
plot(g_by_categories_9)
g_by_categories_10 <- ts(g_by_categories['СОЦИАЛЬНАЯ ПОЛИТИКА'], start=c(2005, 1), end=c(2022, 2), frequency = 4)
plot(g_by_categories_10)

g_economy <- (g_by_categories_1 + g_by_categories_4 + g_by_categories_5) / 10^9
g_social <- (g_by_categories_10 + g_by_categories_9 + g_by_categories_7) / 10^9
g_social[1:68] <- (g_by_categories_6[1:68] + g_by_categories_8[1:68]) / 10^9 + g_social[1:68]
g_military <- (g_by_categories_2 + g_by_categories_3) / 10^9

gdp_in_level <- na.omit(read_excel('/Users/Albina/Documents/Studying/НИР/calculations/gdp.xls', sheet='in_level'))
df_g_by_categories <- data.frame(g_by_categories_1, g_by_categories_2, g_by_categories_3, 
                                 g_by_categories_4, g_by_categories_5, g_by_categories_6,
                                 g_by_categories_7, g_by_categories_8, g_by_categories_9,
                                 g_by_categories_10)
df_g_by_categories_by_gdp <- df_g_by_categories
for(i in 9:ncol(df_g_by_categories)) {       # for-loop over columns
  df_g_by_categories_by_gdp[ , i] <- ts(df_g_by_categories[ , i] / real_gdp_ts[6:75] / 10^9, start=c(2005, 1), end=c(2022, 2), frequency = 4)
  df_g_by_categories_by_gdp[ , i] <- final(seas(df_g_by_categories_by_gdp[ , i]))
}

plot(x = seq(as.Date("2005/1/1"), as.Date("2022/4/1"), by = "quarter"), y = final(seas(g_economy / real_gdp_ts[6:75] / 10^9)), 'l', ylim=c(0, 0.3))
lines(x = seq(as.Date("2005/1/1"), as.Date("2021/10/1"), by = "quarter"), y = final(seas(g_social / real_gdp_ts[6:75] / 10^9)), 'l', col=2)
lines(x = seq(as.Date("2005/1/1"), as.Date("2022/4/1"), by = "quarter"), y = final(seas(g_military / real_gdp_ts[6:75] / 10^9, x11 = "")), 'l', col=3)
legend(x = "topleft",   legend = c("экономические расходы", "социальные расходы", "военные расходы"),  
       lty = c(1, 1, 1),  
       col = c(1, 2, 3),  
       lwd = 1) 

plot(x=seq(as.Date("2005/1/1"), as.Date("2022/4/1"), by = "quarter"), y=df_g_by_categories_by_gdp$ОБЩЕГОСУДАРСТВЕННЫЕ.ВОПРОСЫ, 'l')
lines(x=seq(as.Date("2005/1/1"), as.Date("2022/4/1"), by = "quarter"), y=df_g_by_categories_by_gdp$НАЦИОНАЛЬНАЯ.ОБОРОНА, 'l', col=2)
lines(x=seq(as.Date("2005/1/1"), as.Date("2022/4/1"), by = "quarter"), y=df_g_by_categories_by_gdp$НАЦИОНАЛЬНАЯ.БЕЗОПАСНОСТЬ.И.ПРАВООХРАНИТЕЛЬНАЯ.ДЕЯТЕЛЬНОСТЬ, 'l', col=3)
legend(x = "topright",   legend = c("общегосударственные вопросы", "национальная оборона", "национальная безопасность"),  
       lty = c(1, 1, 1),  
       col = c(1, 2, 3),  
       lwd = 1) 

#for(i in 2:ncol(df_g_by_categories_by_gdp)) {       # for-loop over columns
#  plot(x=gdp_in_level$Дата, y=df_g_by_categories_by_gdp[ , i], 'l', col=i)
#lines(x=gdp_in_level$Дата, y=df_g_by_categories_by_gdp[ , i], col=i, 'l')
#}

################################################################################

# оцениваем мультипликатр для разных статей гос расходов

real_g_new <- read_excel('NCGGRNSAXDCRUQ.xls', sheet = 'FRED Graph')
real_t <- na.omit(real_g_new["real_revenue_deflator"])
real_t_ts <- ts(real_t[6:79, 1], start=c(2005, 1), end=c(2022, 2), frequency = 4)

dummy_2006 <- ts(numeric(1), start = c(2005, 1), end = c(2022, 2), frequency = 4)
dummy_2006[7] <- 1

dummy_2020 <- ts(numeric(1), start = c(2005, 1), end = c(2022, 2), frequency = 4)
dummy_2020[61:64] <- 1

real_g_by_categories <- read_excel('/Users/Albina/Documents/Studying/НИР/calculations/g_by_categories_subtracted.xlsx',sheet='deflator')

g_ts <- ts(real_g_by_categories$`ЗДРАВООХРАНЕНИЕ, ФИЗИЧЕСКАЯ КУЛЬТУРА И СПОРТ` / 10^9, start=c(2005, 1), end=c(2022, 2), frequency = 4)

real_gdp_ts <- ts(real_gdp[6:75, 1], start=c(2005, 1), end=c(2022, 2), frequency = 4)

seas_real_oil_price_cpi <- real_oil_price["seas_real_oil_cpi"]
seas_real_oil_price_cpi_ts <- ts(seas_real_oil_price_cpi[7:76, 1], start=c(2004, 3), end=c(2022, 2), frequency = 4)

gdp <- log(final(seas(real_gdp_ts)))
g <- log(final(seas(g_ts)))
oil <- log(seas_real_oil_price_cpi_ts)
t <- log(final(seas(real_t_ts)))
oil_1 <- lag(oil, -1)
oil_2 <- lag(oil, -2)

oil_to_3 <- na.omit(cbind(oil, oil_1, oil_2))

seas_real_oil_price_cpi <- real_oil_price["seas_real_oil_cpi"][8:80,]
seas_real_oil_price_cpi_ts <- ts(seas_real_oil_price_cpi, start=c(2004, 4), end=c(2022, 2), frequency = 4)
oil <- log(seas_real_oil_price_cpi_ts)
oil_1 <- lag(oil, -1)
oil_to_2 <- na.omit(cbind(oil, oil_1))

seas_real_oil_price_cpi <- real_oil_price["seas_real_oil_cpi"][9:80,]
seas_real_oil_price_cpi_ts <- ts(seas_real_oil_price_cpi, start=c(2005, 1), end=c(2022, 2), frequency = 4)
oil <- log(seas_real_oil_price_cpi_ts)

bmat <- diag(3)
bmat[2, 1] <- NA
bmat[3, 1] <- NA
bmat[3, 2] <- NA

sv <- cbind(t, g, gdp)
colnames(sv) <- cbind("Государственные доходы", "ОБЩЕГОСУДАРСТВЕННЫЕ ВОПРОСЫ", "ВВП")

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
summary(adf.test(sv[,2]))

#lagselect <- VARselect(sv, lag.max = 4, "const", exogen = cbind(oil_to_2, dummy_2006))
lagselect <- VARselect(sv, lag.max = 4, "const")
lagselect$selection

#Model <- VAR(sv, p = 2, season = NULL, exogen = cbind(oil_to_2, dummy_2006), type = "const")
Model <- VAR(sv, p = 2, season = NULL, type = "const")
SVARMod <- SVAR(Model, Bmat = bmat, hessian = TRUE, esmethod = "scoring")
SVARMod
#summary(SVARMod)

Imp3 <- irf(SVARMod, impulse = "ОБЩЕГОСУДАРСТВЕННЫЕ.ВОПРОСЫ", response = "ВВП", n.ahead = 20, ortho = TRUE, runs = 1000, cumulative = FALSE)
plot(Imp3)

s = 0
for(i in 1:4){
  s = s + Imp3[1]$irf$ОБЩЕГОСУДАРСТВЕННЫЕ.ВОПРОСЫ[i]
}
s

#Imp3 <- irf(SVARMod, impulse = "ОБЩЕГОСУДАРСТВЕННЫЕ.ВОПРОСЫ", response = "ВВП", n.ahead = 20, ortho = TRUE, runs = 1000, cumulative = FALSE)
#plot(Imp3)

#################################################################################







