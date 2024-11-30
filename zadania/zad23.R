library(readxl)
library(tidyverse)
library(moments)

# 1.1
# przekształć wyniki dla jazdy na rowerze i dla biegania na nowe 
# zmienne w bazie, wyrażając wynik w sekundach

data <- read.table("/home/user0/Documents/R/WAD/dane_triathlon.R")

data <- data %>% 
  mutate (SWIM_s = as.numeric(as.difftime(SWIM, format="%H:%M:%S", units="secs")))
  
data <- data %>% 
  mutate (BIKE_s = as.numeric(as.difftime(BIKE, format="%H:%M:%S", units="secs")))

data <- data %>% 
  mutate (RUN_s = as.numeric(as.difftime(RUN, format="%H:%M:%S", units="secs")))

view(data)  

# 1.2
# sporządzając osobne histogramy dla obu płci spróbuj ocenić, 
# czy w przypadku tych dwóch dyscyplin mamy do czynienia z rozkładem jedno czy wielomodalnym?

data %>% 
  ggplot(aes(x=BIKE_s))+
  geom_histogram()+
  facet_wrap(~PROGRAM, ncol=1)

data %>% 
  ggplot(aes(x=RUN_s))+
  geom_histogram()+
  facet_wrap(~PROGRAM, ncol=1)

# ODP: jednomodalny rozkład -  wyniki koncentrują się wokół jednego szczytu 
# nie widać wyraźnych dodatkowych szczytów, które wskazywałyby na wielomodalność 


# 1.3
# podaj wartości średniej i mediany. O czym świadczy różnica między nimi?
mean(data$RUN_s)
median(data$RUN_s)

mean(data$BIKE_s)
median(data$BIKE_s)

# ODP: średnia jest mniejsza od mediany, to wskazuje to na występowanie
# wartości mniejszych, które zaniżają średnią 
# mediana jest bardziej odporna na wartości skrajne


# 2.1
# sporządź wykresy skrzynkowe dla roweru i biegania i odpowiedz na pytania:

data %>%
  ggplot(aes(x=BIKE_s))+
  geom_boxplot()+
  facet_wrap(~PROGRAM, ncol=1)

data %>% 
  ggplot(aes(x=RUN_s))+
  geom_boxplot()+ 
  facet_wrap(~PROGRAM, ncol=1)

?geom_boxplot


# 2.2 
# w której dyscyplinie i grupie można zaobserwować najwięcej przypadków odstających?

# ODP: w grupie kobiet zarówno w biegu jak i jeździe rowerem można zaobserwować dwa 
#      przypadki odstające po lewej i prawej stronie wykresu
#      w grupie mężczyzn pojawia się jeden outlier po lewej stronie w obu dyscyplinach

# 2.3
# porównując trzy dyscypliny między sobą sprawdź, gdzie występuje najmniejsze, a gdzie największe zróżnicowanie wyników. Jakich miar do tego użyjesz?


# zróżnicowanie wyników mogę sprawdzić np. licząc rozstęp albo odchylenie standardowe

# rozstęp
max(data$BIKE_s) - min(data$BIKE_s)
max(data$RUN_s) - min(data$RUN_s)

# odchylenie standardowe
sd(data$BIKE_s)
sd(data$RUN_s)

# wartości rozstępu i odchylenia są wiekszę dla dyscypliny rower
# co wskazuje na większe zróżnicowanie wyników


# 2.4
# policz (osobno dla obydwu płci): ile minut i sekund wynosi różnica między najszybszym i najwolniejszym:
# a) wynikiem w pływaniu, b) wynikiem na rowerze, c) wynikiem biegu


data_men <- filter(data, PROGRAM == "Elite Men" & SWIM_s > 0 & BIKE_s > 0 & RUN_s > 0)
data_women <- filter(data, PROGRAM == "Elite Women" & SWIM_s > 0 & BIKE_s > 0 & RUN_s > 0)

roznica <- function(dyscyplina) {
  najszybszy <- min(dyscyplina)
  najwolniejszy <- max(dyscyplina)
  roznica <- najwolniejszy - najszybszy
  
  minuty <- roznica %/% 60
  sekundy <- roznica %% 60
  
  wynik <- paste(minuty, sprintf("%02d", sekundy), sep = ":")
  return(wynik) 
}

roznica(data_men$SWIM_s)
roznica(data_women$SWIM_s)

roznica(data_men$BIKE_s)
roznica(data_women$BIKE_s)

roznica(data_men$RUN_s)
roznica(data_women$RUN_s)

# 3.1
# dla nowych dwóch zmiennych policz i zinterpretuj wyniki: 
# a) rozstępu ćwiartkowego, b) skośności oraz c) kurtozy


# rozstęp ćwiartkowy
IQR(data$BIKE_s) 
IQR(data$RUN_s)
# ODP: rozrzut dla roweru wynosi 419 czyli połowa danych rózni się o 419
#      dla biegu rozrzut jest mniejszy, czyli czasy są do siebie bardziej zbliżone
#      większa zmienność w czasach jazdy na rowerze w porównaniu do biegu

# skośność
skewness(data$BIKE_s) 
skewness(data$RUN_s)
# ODP: wartości są ujemne, czyli rozkład jest skośny w lewo

# kurtoza
kurtosis(data$BIKE_s) 
kurtosis(data$RUN_s)
# ODP: kurtoza ok. 10 wskazuje na spiczasty kształt i większe ogony
#      wysoka kurtoza w obu przypadkach sugeruje, że w danych występują wartości odstając



# 4.1 
# Sporządź rozkład procentowy zmiennej określającej reprezentowany 
# kraj i wymień sześć najczęściej reprezentowanych krajów.

tabela_kraj <- table(data$NATIONALITY)
rozklad <- prop.table(tabela_kraj) * 100
view(rozkład_procentowy)

# ODP:
# FRA 5.4545455
# GER 5.4545455
# ESP 4.5454545
# GBR 4.5454545
# ITA 4.5454545
# USA 4.5454545



