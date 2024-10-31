# ładowanie bibliotek
library(readxl)
library(tidyverse)

getwd()

data <- read.table("C:/Users/student/Downloads/dane_triathlon.R")

# zajrzyj do bazy danych
view(data)

# analiza zmiennej SWIM
class(data)
class(data$SWIM) # zmienna SWIM jest typu character

# Przekształcenie zmiennej SWIM z character na liczbe
data <- data %>% # %>% przesyła obiekt data jako argument do mutate
  mutate (SWIM_s = as.numeric(as.difftime(SWIM, format="%H:%M:%S", units="secs")))

class(data$SWIM_s) # nowa zmienna jest typu numeric
hist(data$SWIM_s, breaks=40)

summary(data$SWIM_s) # Qu - kwartyle


data %>% 
  filter(PROGRAM=="Elite Women") %>% 
  ggplot(aes(x=SWIM_s))+
  geom_histogram(binwidth = 5)

data %>% 
  filter(PROGRAM=="Elite Men") %>% 
  ggplot(aes(x=SWIM_s))+
  geom_boxplot() # w białym boxie boxplota jest 50% wartości i mediana

data %>% 
  ggplot(aes(x=SWIM_s))+
  geom_boxplot()+
  facet_wrap(~PROGRAM, ncol=1)
  
# MIARY TENDENCJI CENTRALNEJ
min(data$SWIM_s) # wartość minimalna zmiennej
max(data$SWIM_s) # wartość maksymalna 
median(data$SWIM_s) # mediana
mean(data$SWIM_s) # średnia
quantile(data$SWIM_s) # kwartyle dzielące rozkład na cztery części

# nie ma wbudowanej funkcji na modalną, trzeba napisać
find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x,u))
  u[tab == max(tab)]
}

find_mode(data$SWIM_s)


# MIARY ROZPROSZENIA
max(data$SWIM_s) - min(data$SWIM_s) # rozstęp
IQR(data$SWIM_s) # rozstęp ćwiartkowy 
var(data$SWIM_s) # wariancja
sqrt(var(data$SWIM_s)) # pierwiastek kw.  z wariancji
sd(data$SWIM_s) # odchylenie standardowe
mean(abs(data$SWIM_s-mean(data$SWIM_s))) #odchylenie przeciętne


# SKOŚNOŚĆ I KURTOZA
library(moments)
skewness(data$SWIM_s)
kurtosis(data$SWIM_s)


# ROZKŁAD ZMIENNYCH JAKOŚCIOWYCH I ILOŚCIOWYCH
# (nominale + porząkowe)
table(data$PROGRAM) # najprostsza funkcja
prop.table(table(data$PROGRAM)) # jako proporcja
prop.table(table(data$PROGRAM))*100 # procent

# tabele mogą byc wielowymiarowe
table(data$POSITION,data$PROGRAM)
  

  
  
