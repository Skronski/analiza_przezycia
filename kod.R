
# biblioteki --------------------------------------------------------------
install.packages("OIsurv")
install.packages('tidyverse')
install.packages("survminer")
install.packages('flexsurv')

library(survminer)
library(survival)
library(tidyverse)
library(readxl)
library(ggplot)
library(flexsurv)




# wczytanie danych --------------------------------------------------------

df <- read.csv('dane.csv', sep = ';', dec = ',')

names(df) <- c('wiek', 'kobieta', 'nowotwor', 'zgon', 'dlugosc')


# model aft weibula -------------------------------------------------------

hist(df$wiek)

df$wiek_starszy <- ifelse(df$wiek >60, 1,0)

weibull <- survreg(Surv(dlugosc, zgon) ~ nowotwor+kobieta +wiek_starszy,data = df, dist='weibull')

summary(weibull)


# model lognormalny -------------------------------------------------------

lognormal <- survreg(Surv(df$duration, df$failure) ~ as.factor(df$treatment), dist='lognormal')



# model wykladniczy -------------------------------------------------------




# model logistyczny -------------------------------------------------------


