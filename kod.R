# biblioteki --------------------------------------------------------------
install.packages("OIsurv")
install.packages('tidyverse')
install.packages("survminer")
install.packages('flexsurv')
install.packages('egg')



library(survminer)
library(survival)
library(tidyverse)
library(readxl)
library(ggplot2)
library(flexsurv)
library(egg)



# wczytanie danych --------------------------------------------------------

df <- read.csv('dane.csv', sep = ';', dec = ',')

names(df) <- c('wiek', 'kobieta', 'nowotwor', 'zgon', 'dlugosc')



# eksploracja danych  -----------------------------------------------------

sum(is.na(df$wiek))


# wizualizacja danych -----------------------------------------------------

par(mfrow = c(3, 1))

p1 <- df %>% ggplot(aes(x = wiek)) + 
  geom_histogram(binwidth = 5, color = "black", fill = "lightblue") +
  ggtitle("Rozkład wieku") + 
  xlab("Wiek") + 
  ylab("Częstość")


p2 <- df %>%
  ggplot(aes(x = "", y = wiek)) +
  geom_violin(fill = "lightblue", color = "black") +
  ggtitle("rozkład wieku") +
  ylab("Wiek") +
  theme_minimal()
p3 <- df %>%
  ggplot(aes(x = "", y = wiek)) +
  geom_boxplot(fill = "lightblue", color = "black", outlier.shape = NA) +
  ggtitle("Rozkład wieku") +
  ylab("wiek") +
  theme_minimal()

ggarrange(p1,p2,p3, nrow = 1)

df %>% ggplot(aes(x = wiek, fill = kobieta)) +
  geom_histogram(binwidth = 5, alpha = 0.7, position = "identity") +
  ggtitle("Rozkład wieku ze względu na płeć") +
  xlab("Wiek") +
  ylab("Częstość") +
  facet_wrap(~ kobieta, nrow = 1) +
  theme_minimal()+
  theme(legend.position='none')

 as.factor(df$kobieta,labels = c('Kobieta', 'Mężczyzna'))

# model aft weibula -------------------------------------------------------


weibull <-
  survreg(
    Surv(dlugosc, zgon) ~ nowotwor + kobieta + wiek_starszy,
    data = df,
    dist = 'weibull'
  )

weibull %>% summary()


# model lognormalny -------------------------------------------------------

lognormal <-
  survreg(
    Surv(dlugosc, zgon) ~ nowotwor + kobieta + wiek_starszy,
    data = df,
    dist = 'lognormal'
  )
lognormal %>% summary()

# model wykladniczy -------------------------------------------------------

exponential <-
  survreg(
    Surv(dlugosc, zgon) ~ nowotwor + kobieta + wiek_starszy,
    data = df,
    dist = 'exponential'
  )

exponential %>% 

# model logistyczny -------------------------------------------------------


logistic <-
  survreg(
    Surv(dlugosc, zgon) ~ nowotwor + kobieta + wiek_starszy,
    data = df,
    dist = 'logistic'
  )

logistic %>% summary()

