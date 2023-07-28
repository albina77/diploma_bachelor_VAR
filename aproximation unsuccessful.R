# разберемся с 2006 годом (и ранее)

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

real_g_new <- na.omit(read_excel('NCGGRNSAXDCRUQ.xls', sheet = 'result'))
real_g_new <- real_g_new["real_spending"]
real_g_new_ts <- ts(real_g_new, start=c(2003, 4), end=c(2021, 4), frequency = 4)

plot(real_g_new_ts)

real_g_old <- na.omit(read_excel('VVP_kvartal_s1995(3).xls', 
                                 sheet = 'G'))
real_g_old_ts <- ts(real_g_old["G"][8:80, 1], start=c(2003, 4), end=c(2021, 4), frequency = 4)

lines(real_g_old_ts, col=2)
axis(1, at = seq(2003, 2021, by = 1))

real_g_by_categories <- na.omit(read_excel('/Users/Albina/Documents/Studying/НИР/calculations/g_by_categories_subtracted.xlsx',sheet='real'))

g_ts <- ts(real_g_by_categories$`КУЛЬТУРА, КИНЕМАТОГРАФИЯ И СРЕДСТВА МАССОВОЙ ИНФОРМАЦИИ`, start=c(2005, 1), end=c(2021, 4), frequency = 4)
plot(g_ts)

# pattern to merge g_sophist and g_new

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

################################################################################
real_g_old_ts <- ts(real_g_old["G"][8:80, 1], start=c(2003, 4), end=c(2021, 4), frequency = 4)
lines(real_g_old_ts, col=2)

g <- real_g_old["G_init"]
g <- ts(g, start=c(2002, 1), end=c(2021, 4), frequency = 4)
plot(g)
axis(1, at = seq(2002, 2021, by = 1))
lines(final(seas(real_g_merged)), col=2)
