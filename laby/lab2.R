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

# przekształcenie zmiennej SWIM z character na liczbe
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
  
# Miary tendencji centralnej
min(data$SWIM_s)
max(data$SWIM_s)
median(data$SWIM_s)
  
  
  
  
  
