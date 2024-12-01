library(readxl)
library(tidyverse)
library(moments)

data <- read.table("/home/user0/Documents/R/dane_triathlon.R")

# 1.1
# upewnij się, że w Twoim zbiorze danych masz wyliczone zmienne 
# umożliwiające ich analizę jako zmiennych numerycznych (czas: pływania, jazdy na rowerze, biegu w sekundach)

data <- data %>% 
  mutate (SWIM_s = as.numeric(as.difftime(SWIM, format="%H:%M:%S", units="secs")))

data <- data %>% 
  mutate (BIKE_s = as.numeric(as.difftime(BIKE, format="%H:%M:%S", units="secs")))

data <- data %>% 
  mutate (RUN_s = as.numeric(as.difftime(RUN, format="%H:%M:%S", units="secs")))

# 1.2
# do dalszych analiz wybierz tylko te obserwacje, których wartości są > 0
data_clear <- data %>%
  filter(RUN_s>0 & SWIM_s>0 & BIKE_s >0)

# 1.3
# za pomocą wykresu rozrzutu zilustruj zależności dla par zmiennych:
# 1) pływanie + bieg, 2) rower + bieg, uwzględniając różnice między płciami 

data_clear %>%
  ggplot(aes(x=SWIM_s,y=RUN_s, color=PROGRAM))+
  geom_smooth(method="lm")+
  geom_point()+
  

data_clear %>%
  ggplot(aes(x=BIKE_s,y=RUN_s, color=PROGRAM))+
  geom_smooth(method="lm")+
  geom_point()+
  geom_text(aes(label = ATHLETE.LAST), size = 3)



# 1.4
# odpowiedz na pytanie: jaki charakter ma zależność pomiędzy zmiennymi?
# Czy jest taka sama dla kobiet, jak dla mężczyzn?

# ODP:
# jeśli czas pływania lub jazdy na rowerze wzrasta to czas biegu kobiet rośnie szybciej niż u mężczyzn
# widać większe nachylenie lini dla kobiet możliwe, że kobiety mają większą trudność w utrzymaniu 
# wysokiej wydolności w biegu po dłuższym pływaniu lub jeździe na rowerze


# 1.5
# dodatkowo – jeśli uznasz, że takie są: spróbuj zidentyfikować obserwacje odstające,
# które (być może) trzeba będzie wykluczyć w dalszych analizach

# ODP: Hannesdottir, Kasper, Briffod, Duchampt 

# 2.1
# oceń kierunek i siłę związku między zmiennymi za pomocą wybranej miary. 
# Jeśli to zasadne, dokonaj osobnych obliczeń dla kobiet i mężczyzn. Uzasadnij wybór właśnie tej miary

data_men <- data_clear %>%
  filter(PROGRAM=="Elite Men")

data_women <- data_clear %>%
  filter(PROGRAM=="Elite Women")

shapiro.test(data_men$SWIM_s)
shapiro.test(data_women$SWIM_s)
# małe prawdopodobieństwo na odstające dane czyli jest ok

# używam Pearsona bo korelacja jest unormowana od -1 do 1 i jest najpopularniejsza
cor(data_men$SWIM_s, data_men$BIKE_s) # 0.7181241
cor(data_women$SWIM_s, data_women$BIKE_s) # 0.2503541

# ODP: korelacja jest znacznie silniejsza dla mężczyzn niż dla kobiet



