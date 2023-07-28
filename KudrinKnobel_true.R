# 3 - пробуем в точности повторить Кудрина Кнобеля

library(seasonal)
library(readxl)
library(ggplot2)
library(zoo)
library(reshape2)
library(vars)
library(tseries)
library(dynlm)
library(urca)
library('devtools')
source_url("https://raw.githubusercontent.com/anguyen1210/var-tools/master/R/extract_varirf.R")

rm(list=ls())
setwd('/Users/Albina/Documents/Studying/НИР/preparing_to_seminar_01.03')

# ВВП, приведенный в реальное выражение (2016 год), млрд руб
real_gdp <- na.omit(read_excel('VVP_kvartal_s1995(3).xls', sheet = 'Real GDP'))
real_gdp <- na.omit(real_gdp[8:82, 2])
# урезаем данные, т.к. G на сайте казнчаейства только с 4 квартала 2003 по 4 квартал 2022
real_gdp_ts <- ts(real_gdp, start=c(2003, 4), end=c(2022, 2), frequency = 4)

df <- data.frame(Date = seq(as.Date("2003/10/01"), as.Date("2022/04/01"), by = "quarter"), gdp = final(seas(real_gdp_ts)))

asy_asy <- df %>% 
  ggplot(aes(x = Date, y = gdp)) +
  geom_line() +
  scale_x_date(date_labels = "%Y", date_breaks = "2 year")+
  theme_light() +
  ylab("ВВП, млрд.руб.")+
  xlab("Год") +
  theme(plot.title = element_text(size = 11, hjust=0.5),
        axis.title.y = element_text(size=11))
asy_asy


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

asy_asy <- df %>% 
  ggplot(aes(x = Date, y = g)) +
  geom_line() +
  scale_x_date(date_labels = "%Y", date_breaks = "2 year")+
  theme_light() +
  ylab("Совокупные государственные расходы, млрд.руб.")+
  xlab("Год") +
  theme(plot.title = element_text(size = 11, hjust=0.5),
        axis.title.y = element_text(size=11))
asy_asy

plot(final(seas(g_merged))/final(seas(real_gdp_ts)))

# цена на нефть марки Brent в долларах, приведенная в реальное выражение с помощью 
# индекса цен производителей
real_oil_price <- read_excel('POILBREUSDQ.xls', sheet='FRED Graph')

# приведено в реальное выражение с помощью Consumer Price Index for All Urban Consumers
# CPIAUCSL

seas_real_oil_price_cpi <- real_oil_price["seas_real_oil_cpi"][4:80,]
seas_real_oil_price_cpi_ts <- ts(seas_real_oil_price_cpi, start=c(2003, 4), end=c(2022, 2), frequency = 4)

df <- data.frame(Date = seq(as.Date("2003/10/01"), as.Date("2022/04/01"), by = "quarter"), oil = seas_real_oil_price_cpi_ts)
df2 <- data.frame(Date = seq(as.Date("2003/10/01"), as.Date("2020/01/01"), by = "quarter"), oil = (seas_real_oil_price_cpi_ts - mean(seas_real_oil_price_cpi_ts))/var(seas_real_oil_price_cpi_ts)*3000+100)

asy_asy <- df %>% 
  ggplot(aes(x = Date, y = oil)) +
  geom_line() +
  geom_line(data=df2, aes(y=oil, x=Date), col='green')+
  scale_x_date(date_labels = "%Y", date_breaks = "2 year")+
  theme_light() +
  ylab("Цена на нефть марки Brent, $")+
  xlab("Год") +
  theme(plot.title = element_text(size = 11, hjust=0.5),
        axis.title.y = element_text(size=11))
asy_asy

plot(seas_real_oil_price_cpi_ts)

dummy_2006 <- ts(numeric(1), start = c(2003, 4), end = c(2022, 2), frequency = 4)
dummy_2006[12] <- 1

# исключаем сезонность и берем логарифм

gdp <- log(final(seas(real_gdp_ts)))
g <- log(final(seas(g_merged)))
oil <- log(seas_real_oil_price_cpi_ts)

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

lagselect <- VARselect(sv, lag.max = 4, type = "const", exogen = cbind(dummy_2006, oil))
lagselect$selection

Model1 <- VAR(sv, p = 2, season = NULL, type = "const", exogen = cbind(dummy_2006, oil))
SVARMod1 <- SVAR(Model1, Bmat = bmat, hessian = TRUE, estmethod = "scoring")
SVARMod1
summary(SVARMod1)

# считаем значение мультипликатора за год

Phi(SVARMod1, nstep=4)[,,1][2,][1] + Phi(SVARMod1, nstep=4)[,,2][2,][1] + Phi(SVARMod1, nstep=4)[,,3][2,][1] + Phi(SVARMod1, nstep=4)[,,4][2,][1]

Imp1 <- irf(SVARMod1, impulse = "oil", response = "GDP", n.ahead = 20, ortho = TRUE, runs = 1000, cumulative = FALSE)
plot(Imp1)

s = 0
for(i in (1:8)){
  s = s + Imp1$irf$Government.spending[i]
}
s
Imp1$irf$Government.spending[1] + Imp1$irf$Government.spending[2] + Imp1$irf$Government.spending[3] + Imp1$irf$Government.spending[4]

#################################################################################################

# Расходы по отдельным функциональным разделам

g_by_categories <- read_excel('/Users/Albina/Documents/Studying/НИР/calculations/g_by_categories_subtracted.xlsx', sheet='deflator')

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

plot(x = seq(as.Date("2005/1/1"), as.Date("2022/4/1"), by = "quarter"), y = final(seas(g_economy / real_gdp_ts[6:75] * 100)), 'l', ylim=c(0, 30), xlab = "Год", ylab = "Категория расходов в % к ВВП")
lines(x = seq(as.Date("2005/1/1"), as.Date("2022/4/1"), by = "quarter"), y = final(seas(g_social / real_gdp_ts[6:75] * 100)), 'l', col=2)
lines(x = seq(as.Date("2005/1/1"), as.Date("2022/4/1"), by = "quarter"), y = final(seas(g_military / real_gdp_ts[6:75] * 100, x11 = "")), 'l', col=3)
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

real_g_by_categories <- read_excel('/Users/Albina/Documents/Studying/НИР/calculations/g_by_categories_subtracted.xlsx',sheet='deflator')

g_ts <- ts(real_g_by_categories$`ОХРАНА ОКРУЖАЮЩЕЙ СРЕДЫ`/ 10^9, start=c(2005, 1), end=c(2021, 4), frequency = 4)
plot(final(seas(g_ts)))
plot(real_gdp_ts)

real_gdp_ts <- ts(real_gdp[6:75, 1], start=c(2005, 1), end=c(2021, 4), frequency = 4)

seas_real_oil_price_cpi <- real_oil_price["seas_real_oil_cpi"]
seas_real_oil_price_cpi_ts <- ts(seas_real_oil_price_cpi[9:80, 1], start=c(2005, 1), end=c(2021, 4), frequency = 4)

gdp <- log(final(seas(real_gdp_ts)))
g <- log(final(seas(g_ts)))
oil <- log(seas_real_oil_price_cpi_ts)

bmat <- diag(2)
bmat[2, 1] <- NA

sv <- cbind(g, gdp)
colnames(sv) <- cbind("ОБЩЕГОСУДАРСТВЕННЫЕ ВОПРОСЫ", "ВВП")

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

lagselect <- VARselect(sv, lag.max = 4, "const", exogen = oil)
lagselect$selection

Model <- VAR(sv, p = 1, season = NULL, exogen = oil, type = "const")
SVARMod <- SVAR(Model, Bmat = bmat, hessian = TRUE, estmethod = "scoring")
SVARMod
#summary(SVARMod)

Imp3 <- irf(SVARMod, impulse = "ОБЩЕГОСУДАРСТВЕННЫЕ.ВОПРОСЫ", response = "ВВП", n.ahead = 20, ortho = TRUE, runs = 1000, cumulative = FALSE)

s = 0
for(i in 1:8){
  s = s + Imp3[1]$irf$ОБЩЕГОСУДАРСТВЕННЫЕ.ВОПРОСЫ[i]
}
s

imp_grouped <- extract_varirf(Imp3)
asy_asy <- imp_grouped %>% 
  ggplot(aes(y=irf_общегосударственные.вопросы_ввп, ymin=lower_общегосударственные.вопросы_ввп, ymax=upper_общегосударственные.вопросы_ввп, x=period)) +
  geom_hline(yintercept = 0, color="red") +
  geom_ribbon(fill="grey", alpha=0.2) +
  geom_line() +
  theme_light() +
  ggtitle("IRF ВВП на шок расходов на охрану окружающей среды")+
  ylab("Отклик в п.п. ВВП")+
  xlab("Количество кварталов после шока") +
  theme(plot.title = element_text(size = 11, hjust=0.5),
        axis.title.y = element_text(size=11))
asy_asy

#################################################################################
real_g_by_categories <- read_excel('/Users/Albina/Documents/Studying/НИР/calculations/g_by_categories_subtracted.xlsx',sheet='deflator')

g_ts <- ts(real_g_by_categories$`ЗДРАВООХРАНЕНИЕ, ФИЗИЧЕСКАЯ КУЛЬТУРА И СПОРТ` / 10^9, start=c(2005, 1), end=c(2021, 4), frequency = 4)

real_g_by_categories_2011 <- read_excel('/Users/Albina/Documents/Studying/НИР/calculations/g_by_categories_subtracted.xlsx',sheet='2011')

g_ts_2011 <- ts(real_g_by_categories_2011$`Здравоохранение`, start=c(2011, 1), end=c(2021, 4), frequency = 4)
g_ts_2011_2 <- ts(real_g_by_categories_2011$`Физическая культура и спорт`, start=c(2011, 1), end=c(2021, 4), frequency = 4)
g_unit <- g_ts_2011 + g_ts_2011_2
plot(g_ts)
lines(g_ts_2011 + g_ts_2011_2, col=3)
