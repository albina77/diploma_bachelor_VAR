# КудринКнобель оценка в приращениях

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
real_gdp <- na.omit(real_gdp[14:83, 2])
# урезаем данные, т.к. G на сайте казнчаейства только с 4 квартала 2003 по 4 квартал 2021
real_gdp_ts <- ts(real_gdp, start=c(2005, 1), end=c(2022, 2), frequency = 4)

plot(real_gdp_ts)

# Расходы и доходы, составленные вручную с сайта казначейства
# Приведены в реальное выражение с помощью ИПЦ

real_g_new <- na.omit(read_excel('NCGGRNSAXDCRUQ.xls', sheet = 'result'))
real_t <- real_g_new["real_revenue"]
real_t_ts <- ts(real_t[6:75,], start=c(2005, 1), end=c(2021, 4), frequency = 4)
t_2022 <- c(11331.23/1.336050007/1.099468405, (22822.20-11331.23)/1.336050007/1.099468405/1.013259854)
t_merged <- ts(c(real_t_ts, ts(t_2022, start = c(2022, 1))), start=start(real_t_ts), frequency = 4)

plot(real_t_ts)

g_by_categories <- read_excel('/Users/Albina/Documents/Studying/НИР/calculations/g_by_categories_subtracted.xlsx', sheet='real_time')
# этот ряд (социальная политика) не получается отсезонить - он принимается за сгенерированный процесс. какой - я 
# не смогла подобрать, но коэффициентики там получаются явно странные
g_by_categories[2:11] <- g_by_categories[2:11] / 10^9

library(tsbox)
g_by_categories <- ts_ts(ts_long(g_by_categories))

for(i in 1:8) {       # for-loop over columns
  g_by_categories[ , i] <- final(seas(g_by_categories[, i]))
}
g_by_categories[ , 9] <- c(final(seas(g_by_categories[, 9])), NA, NA)
g_by_categories[ , 10] <- c(final(seas(g_by_categories[, 10])), NA, NA)

t <- final(seas(real_t_ts))
gdp <- final(seas(real_gdp_ts))

t_y <- t / gdp
t_y <- ts(t_y[5:68], start=c(2006, 1), end=c(2022, 2), frequency = 4)
g_y <- g_by_categories / gdp
g_y <- window(g_y, start=c(2006,1), end=c(2022,2))
y_4 <- gdp - lag(gdp, 4)
y <- y_4 / gdp
y <- ts(y, start=c(2006, 1), end=c(2022, 2), frequency = 4)

mult_array <- c()
long_mult_1 <- c()

# сначала пробуем для одной статьи расходов
for(i in 1:10) { 
  bmat <- diag(3)
  bmat[2, 1] <- NA
  bmat[3, 1] <- NA
  bmat[3, 2] <- NA
  
  sv <- cbind(t_y, g_y[,i], y)
  colnames(sv) <- cbind("T_to_Y", "G_t_to_Y", "delta_Y")
  
  # ur.df проводит тестирование Dickey-Fuller
  summary(ur.df(y,  type = "drift", selectlags = "AIC"))
  summary(ur.df(g_y[,1],  type = "none", selectlags = "AIC"))
  #Не отвергаем H0
  
  #Проверка наличия коинтеграции. Включать константу "-1" или нет  - вопрос дискуссионный.
  cointcy <- dynlm(y ~ g_y[,1] + t_y)
  summary(cointcy)
  #Сохраняем ошибку из регрессии (gdp ~ g) и проверяем стационарна ли она.
  ehat <- resid(cointcy)
  summary(ur.df(ehat,  type = "none", selectlags = "AIC"))
  #summary(adf.test(sv[,3]))
  
  lagselect <- VARselect(sv, lag.max = 4, "const")
  lagselect$selection
  
  Model1 <- VAR(sv, p = 2, season = NULL, type = "const")
  SVARMod1 <- SVAR(Model1, Bmat = bmat, hessian = TRUE, esmethod = "scoring")
  SVARMod1
  #summary(SVARMod1)
  
  #mult_array <- append(mult_array, Phi(SVARMod1, nstep=4)[,,1][3,][2] + Phi(SVARMod1, nstep=4)[,,2][3,][2] + Phi(SVARMod1, nstep=4)[,,3][3,][2] + Phi(SVARMod1, nstep=4)[,,4][3,][2])
  mult_array <- append(mult_array, Phi(SVARMod1, nstep=4)[,,1][3,][2])
  
  #Imp1 <- irf(SVARMod1, impulse = "G_t_to_Y", response = "delta_Y", n.ahead = 20, ortho = TRUE, runs = 1000)
  #plot(Imp1)
  
  b1 <- Model1[1]$varresult$T_to_Y$coefficients
  b2 <- Model1[1]$varresult$G_t_to_Y$coefficients
  b3 <- Model1[1]$varresult$delta_Y$coefficients
  long_mult_1 <- append(long_mult_1, ((1 - b1[1] - b1[4])*(1 - b3[3] - b3[6]) - (b1[3] + b1[6])*(b3[1] + b3[4])) /((1 - b1[1] - b1[4])*(b3[2] + b3[5]) + (b1[2] + b1[5])*(b3[1] + b3[4])))
  #long_mult <- append(long_mult, ((1 - b1[1])*(1 - b3[3]) - (b1[3])*(b3[1])) / ((1 - b1[1])*(b3[2]) + (b1[2])*(b3[1])))

}










real_gdp <- na.omit(read_excel('VVP_kvartal_s1995(3).xls', 
                               sheet = 'Real GDP'))
real_gdp <- na.omit(real_gdp[14:83, 2])
# урезаем данные, т.к. G на сайте казнчаейства только с 4 квартала 2003 по 4 квартал 2021
real_gdp_ts <- ts(real_gdp, start=c(2005, 1), end=c(2022, 2), frequency = 4)

plot(real_gdp_ts)

# Расходы и доходы, составленные вручную с сайта казначейства
# Приведены в реальное выражение с помощью ИПЦ

real_g_new <- na.omit(read_excel('NCGGRNSAXDCRUQ.xls', sheet = 'result'))
real_t <- real_g_new["real_revenue"]
real_t_ts <- ts(real_t[6:75,], start=c(2005, 1), end=c(2021, 4), frequency = 4)
t_2022 <- c(11331.23/1.336050007/1.099468405, (22822.20-11331.23)/1.336050007/1.099468405/1.013259854)
t_merged <- ts(c(real_t_ts, ts(t_2022, start = c(2022, 1))), start=start(real_t_ts), frequency = 4)

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

g_economy <- g_by_categories_1 + g_by_categories_4 + g_by_categories_5
g_social <- g_by_categories_10 + g_by_categories_9 + g_by_categories_7
g_social[1:68] <- g_social[1:68] + g_by_categories_6[1:68] + g_by_categories_8[1:68]
g_military <- g_by_categories_2 + g_by_categories_3

real_g_new <- real_g_new["real_spending"]
real_g_new_ts <- ts(real_g_new[11:73, 1], start=c(2006, 2), end=c(2021, 4), frequency = 4)

real_g_old <- na.omit(read_excel('VVP_kvartal_s1995(3).xls', sheet = 'G'))
real_g_old_ts <- ts(real_g_old["G"][8:17, 1], start=c(2003, 4), end=c(2006, 1), frequency = 4)

real_g_merged <- ts(c(real_g_old_ts, real_g_new_ts), start = start(real_g_old_ts), frequency = frequency(real_g_old_ts))
plot(real_g_new_ts)

q1 <- read_excel('/Users/Albina/Documents/Studying/НИР/Отчеты/2022.04.01.xls')
q1 <- q1[9:18, c(1, 3)]

q2 <- read_excel('/Users/Albina/Documents/Studying/НИР/Отчеты/2022.07.01.xls')
q2 <- q2[9:18, c(1, 3)]
q2[,2] <- q2[,2] - q1[,2]

q1[,2] <- q1[,2]/1.336050007/1.099468405
q2[,2] <- q2[,2]/1.336050007/1.099468405/1.013259854


g_merged <- ts(c(real_g_merged, ts(q1[10, 2], start = c(2022, 1)), ts(q2[10, 2], start = c(2022, 2))), start=start(real_g_merged), frequency = 4)


t <- final(seas(real_t_ts))
gdp <- final(seas(real_gdp_ts))

t_y <- t / gdp
t_y <- ts(t_y[5:68], start=c(2006, 1), end=c(2022, 2), frequency = 4)
g_y <- final(seas(g_merged)) / gdp
g_y <- window(g_y, start=c(2006,1), end=c(2022,2))
y_4 <- gdp - lag(gdp, 4)
y <- y_4 / gdp
y <- ts(y, start=c(2006, 1), end=c(2022, 2), frequency = 4)

mult_array <- c()
long_mult_1 <- c()

# сначала пробуем для одной статьи расходов
bmat <- diag(3)
bmat[2, 1] <- NA
bmat[3, 1] <- NA
bmat[3, 2] <- NA

sv <- cbind(t_y, g_y, y)
colnames(sv) <- cbind("T_to_Y", "G_t_to_Y", "delta_Y")

# ur.df проводит тестирование Dickey-Fuller
summary(ur.df(y,  type = "drift", selectlags = "AIC"))
summary(ur.df(g_y,  type = "none", selectlags = "AIC"))
#Не отвергаем H0

#Проверка наличия коинтеграции. Включать константу "-1" или нет  - вопрос дискуссионный.
cointcy <- dynlm(y ~ g_y + t_y)
summary(cointcy)
#Сохраняем ошибку из регрессии (gdp ~ g) и проверяем стационарна ли она.
ehat <- resid(cointcy)
summary(ur.df(ehat,  type = "none", selectlags = "AIC"))
#summary(adf.test(sv[,3]))

lagselect <- VARselect(sv, lag.max = 4, "const")
lagselect$selection

Model1 <- VAR(sv, p = 2, season = NULL, type = "const")
SVARMod1 <- SVAR(Model1, Bmat = bmat, hessian = TRUE, esmethod = "scoring")
SVARMod1
#summary(SVARMod1)

#mult_array <- append(mult_array, Phi(SVARMod1, nstep=4)[,,1][3,][2] + Phi(SVARMod1, nstep=4)[,,2][3,][2] + Phi(SVARMod1, nstep=4)[,,3][3,][2] + Phi(SVARMod1, nstep=4)[,,4][3,][2])
mult_array <- append(mult_array, Phi(SVARMod1, nstep=4)[,,1][3,][2])

#Imp1 <- irf(SVARMod1, impulse = "G_t_to_Y", response = "delta_Y", n.ahead = 20, ortho = TRUE, runs = 1000)
#plot(Imp1)

b1 <- Model1[1]$varresult$T_to_Y$coefficients
b2 <- Model1[1]$varresult$G_t_to_Y$coefficients
b3 <- Model1[1]$varresult$delta_Y$coefficients
long_mult_1 <- append(long_mult_1, ((1 - b1[1] - b1[4])*(1 - b3[3] - b3[6]) - (b1[3] + b1[6])*(b3[1] + b3[4])) /((1 - b1[1] - b1[4])*(b3[2] + b3[5]) + (b1[2] + b1[5])*(b3[1] + b3[4])))
#long_mult <- append(long_mult, ((1 - b1[1])*(1 - b3[3]) - (b1[3])*(b3[1])) / ((1 - b1[1])*(b3[2]) + (b1[2])*(b3[1])))










# далее для охраны окружающей среды и культуры отдельно, тк короткие ряды
real_gdp <- na.omit(read_excel('VVP_kvartal_s1995(3).xls', 
                               sheet = 'Real GDP'))
real_gdp <- na.omit(real_gdp[14:81, 2])
# урезаем данные, т.к. G на сайте казнчаейства только с 4 квартала 2003 по 4 квартал 2021
real_gdp_ts <- ts(real_gdp, start=c(2005, 1), end=c(2021, 4), frequency = 4)

plot(real_gdp_ts)

# Расходы и доходы, составленные вручную с сайта казначейства
# Приведены в реальное выражение с помощью ИПЦ

real_g_new <- na.omit(read_excel('NCGGRNSAXDCRUQ.xls', sheet = 'result'))
real_t <- real_g_new["real_revenue"]
real_t_ts <- ts(real_t[6:75,], start=c(2005, 1), end=c(2021, 4), frequency = 4)
#t_2022 <- c(11331.23/1.336050007/1.099468405, (22822.20-11331.23)/1.336050007/1.099468405/1.013259854)
#t_merged <- ts(c(real_t_ts, ts(t_2022, start = c(2022, 1))), start=start(real_t_ts), frequency = 4)


plot(real_t_ts)

g_by_categories <- read_excel('/Users/Albina/Documents/Studying/НИР/calculations/g_by_categories_subtracted.xlsx', sheet='real_time')
# этот ряд (социальная политика) не получается отсезонить - он принимается за сгенерированный процесс. какой - я 
# не смогла подобрать, но коэффициентики там получаются явно странные
g_by_categories[2:11] <- g_by_categories[2:11] / 10^9

library(tsbox)
g_by_categories <- ts_ts(ts_long(g_by_categories[1:68, ]))

for(i in 1:10) {       # for-loop over columns
  g_by_categories[ , i] <- final(seas(g_by_categories[, i]))
}

t <- final(seas(real_t_ts))
gdp <- final(seas(real_gdp_ts))

t_y <- t / gdp
t_y <- ts(t_y[5:68], start=c(2006, 1), end=c(2021, 4), frequency = 4)
g_y <- g_by_categories / gdp
g_y <- window(g_y, start=c(2006,1), end=c(2021, 4))
y_4 <- gdp - lag(gdp, 4)
y <- y_4 / gdp
y <- ts(y, start=c(2006, 1), end=c(2021, 4), frequency = 4)

mult_array <- c()
long_mult <- c()

# сначала пробуем для одной статьи расходов
for(i in 1:10) { 
  bmat <- diag(3)
  bmat[2, 1] <- NA
  bmat[3, 1] <- NA
  bmat[3, 2] <- NA
  
  sv <- cbind(t_y, g_y[,i], y)
  colnames(sv) <- cbind("T_to_Y", "G_t_to_Y", "delta_Y")
  
  # ur.df проводит тестирование Dickey-Fuller
  summary(ur.df(y,  type = "drift", selectlags = "AIC"))
  summary(ur.df(g_y[,1],  type = "none", selectlags = "AIC"))
  #Не отвергаем H0
  
  #Проверка наличия коинтеграции. Включать константу "-1" или нет  - вопрос дискуссионный.
  cointcy <- dynlm(y ~ g_y[,1] + t_y)
  summary(cointcy)
  #Сохраняем ошибку из регрессии (gdp ~ g) и проверяем стационарна ли она.
  ehat <- resid(cointcy)
  summary(ur.df(ehat,  type = "none", selectlags = "AIC"))
  #summary(adf.test(sv[,3]))
  
  lagselect <- VARselect(sv, lag.max = 4, "const")
  lagselect$selection
  
  Model1 <- VAR(sv, p = 2, season = NULL, type = "const")
  SVARMod1 <- SVAR(Model1, Bmat = bmat, hessian = TRUE, esmethod = "scoring")
  SVARMod1
  #summary(SVARMod1)
  
  #mult_array <- append(mult_array, Phi(SVARMod1, nstep=4)[,,1][3,][2] + Phi(SVARMod1, nstep=4)[,,2][3,][2] + Phi(SVARMod1, nstep=4)[,,3][3,][2] + Phi(SVARMod1, nstep=4)[,,4][3,][2])
  mult_array <- append(mult_array, Phi(SVARMod1, nstep=4)[,,1][3,][2])
  
  #Imp1 <- irf(SVARMod1, impulse = "G_t_to_Y", response = "delta_Y", n.ahead = 20, ortho = TRUE, runs = 1000)
  #plot(Imp1)
  
  b1 <- Model1[1]$varresult$T_to_Y$coefficients
  b2 <- Model1[1]$varresult$G_t_to_Y$coefficients
  b3 <- Model1[1]$varresult$delta_Y$coefficients
  long_mult <- append(long_mult, ((1 - b1[1] - b1[4])*(1 - b3[3] - b3[6]) - (b1[3] + b1[6])*(b3[1] + b3[4])) /((1 - b1[1] - b1[4])*(b3[2] + b3[5]) + (b1[2] + b1[5])*(b3[1] + b3[4])))
  #long_mult <- append(long_mult, ((1 - b1[1])*(1 - b3[3]) - (b1[3])*(b3[1])) / ((1 - b1[1])*(b3[2]) + (b1[2])*(b3[1])))
  
}

