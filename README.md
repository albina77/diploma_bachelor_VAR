# diploma_bachelor_VAR

Репозиторий содержит вычисления, произведенные в рамках исследовательской работы на тему **"Оценка мультипликаторов различных типов государственных расходов в российской экономике"**. Для расчета мультипликаторов оценивалась Structural Vector Autoregression Model (SVAR).

В файле [Main.R](https://github.com/albina77/diploma_bachelor_VAR/blob/main/Main.R) содержатся основные вычисления: SVAR в уровнях с 2-мя эндогенными переменными (совокупные государственные расходы, ВВП), экзогенной переменной (цена на нефть марки Brent) и дамми-переменной, равной 1 для 3 квартала 2006 года. Аналогичные вычисления производятся для расходов по различным категориям.

Файл [Kilian.R](https://github.com/albina77/diploma_bachelor_VAR/blob/main/Kilian.R) содержит вариацию модели с использованием [Lutz Kilian index](https://sites.google.com/site/lkilian2019/research/data-sets) в качестве экзогенной переменной.

[Попытка](https://github.com/albina77/diploma_bachelor_VAR/blob/main/аппроксимация%202006.R) заменить данные по государственным расходам за 2006 год (выброс в категории межбюджетных трансфертов) аппроксимированными с помощью Simple Moving Average / Hodrick-Prescott filter / Baxter-King filter / Autoregressive Moving Average была отвергнута в пользу использования дамми в силу экономической специфики выброса.

В файле [График.ipynb](https://github.com/albina77/diploma_bachelor_VAR/blob/main/График.ipynb) данные по расходам по разным категориям были собраны из отчетов МинФина, составленных по разным стандартам, и визуализированы.

[ДипломПрезентация.pdf](https://github.com/albina77/diploma_bachelor_VAR/blob/main/ДипломПрезентация.pdf) описывает основные результаты.

## Данные

[NCGGRNSAXDCRUQ.xls](https://github.com/albina77/diploma_bachelor_VAR/blob/main/NCGGRNSAXDCRUQ.xls) - государственные доходы и расходы, приведенные в реальное выражение с помощью дефлятора ВВП (далее - реальные);   
[POILBREUSDQ.xls](https://github.com/albina77/diploma_bachelor_VAR/blob/main/POILBREUSDQ.xls) - цена на нефть марки Brent, приведенная в реальное выражение с помощью ИПЦ США, и индекс Килиана;  
[VVP_kvartal_s1995(3).xls](https://github.com/albina77/diploma_bachelor_VAR/blob/main/VVP_kvartal_s1995(3).xls) - реальный ВВП России;  
[g_by_categories_subtracted.xlsx](https://github.com/albina77/diploma_bachelor_VAR/blob/main/g_by_categories_subtracted.xlsx) - реальные расходы по категориям.
