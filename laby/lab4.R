library(readxl)
library(tidyverse)
library(moments)

data <- read.table("C:/Users/student/Downloads/dane_triathlon.R")

data <- data %>% 
  mutate (SWIM_s = as.numeric(as.difftime(SWIM, format="%H:%M:%S", units="secs")))

data <- data %>% 
  mutate (BIKE_s = as.numeric(as.difftime(BIKE, format="%H:%M:%S", units="secs")))

data <- data %>% 
  mutate (RUN_s = as.numeric(as.difftime(RUN, format="%H:%M:%S", units="secs")))


# zamiast tworzenia nowych obietków można użyć DPLYera
data %>%
  group_by(PROGRAM) %>% # zmienna grupująca
  summarise(meanSWIM = mean(SWIM_s), sdSWIM = sd(SWIM_s),
            meanBIKE = mean(BIKE_s), sdBIKE = sd(BIKE_s),
            meanRUN = mean(RUN_s), sdRUN = sd(RUN_s)) %>%
  ungroup() # przywraca zachowanie programu do domyślnej sytuacji
            # wraca do stanu kiedy nie dzieliliśmy danych na płeć
            # dobrym zywczajem jest używać ungroup


plot(data$SWIM_s, data$BIKE_s)

# przefiltrowane dane bez czasów zerowych
data_clear <- data %>%
  filter(RUN_s>0)
plot(data_clear$SWIM_s, data_clear$BIKE_s)

# ggplot jest lepszy i daje dużo więcej możliwości
data_clear %>% 
  ggplot(aes(x=SWIM_s, y=BIKE_s))+
  geom_point() # geometria punktowa

# dodajemy linie trendu
data_clear %>%
  ggplot(aes(x=SWIM_s,y=BIKE_s))+
  geom_point()+
  geom_smooth(method="lm") 

data_clear %>%
  ggplot(aes(x=SWIM_s,y=BIKE_s, colour = PROGRAM))+ # mapujemy kolor ze zmienną PROGRAM
  geom_smooth(method="lm")+ # dla każdej zmiennej z PROGRAM tworzy nową linie
  geom_point()

data_clear %>%
  ggplot(aes(x=SWIM_s,y=BIKE_s, colour = PROGRAM))+ 
  geom_smooth(method="lm")+ 
  geom_point()+
  geom_text(aes(label = ATHLETE.LAST), size = 3)


# standardowy test statystyczny, wykorzystywany do testowania normalności danych
# pvalue - prawdopodobieństwo testowe
shapiro.test(data_clear$SWIM_s)
# p-value = 0.0002225
# wynik pokazuje, że prawdopodbieństwo występienia odstającej wartości jest bardzo małe
# to odrzuca hipotezę zerową, założenia normalności nie są spełnione


# kowariancja nie jest unormowana, nie wiadomo czy jest mała czy duża
cov(data_clear$SWIM_s, data_clear$BIKE_s) # kowariancja

# korelacja r-Pearsona jest unormowana w granicach -1 do 1
# r-Pearson jest najpopularniejszy
cor(data_clear$SWIM_s, data_clear$BIKE_s) # korelacja (r-Pearsona)

cor(data_clear$SWIM_s, data_clear$BIKE_s, method="spearman")
cor(data_clear$SWIM_s, data_clear$BIKE_s, method="kendall")


