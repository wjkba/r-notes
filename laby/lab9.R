library(tidyverse)
library(factoextra)

baza1 <- read.table("/home/user0/Documents/R/dane_triathlon_full.R")

baza1 <- baza1 %>% 
  mutate (SWIM_s = as.numeric(as.difftime(SWIM, format="%H:%M:%S", units="secs")))

baza1 <- baza1 %>% 
  mutate (BIKE_s = as.numeric(as.difftime(BIKE, format="%H:%M:%S", units="secs")))

baza1 <- baza1 %>% 
  mutate (RUN_s = as.numeric(as.difftime(RUN, format="%H:%M:%S", units="secs")))

# 1.2
# do dalszych analiz wybierz tylko te obserwacje, których wartości są > 0
baza1 <- baza1 %>%
  filter(RUN_s>0 & SWIM_s>0 & BIKE_s >0)

# metoda k-srednich
wybrane <- select(baza1,c("SWIM_s","BIKE_s","RUN_s"))
wybrane_stand <- scale(wybrane)

wynik1 <- kmeans(wybrane_stand, 3, nstart = 5)
print(wynik1)

fviz_cluster(wynik1, data=wybrane_stand)
fviz_nbclust(wybrane_stand, kmeans, method="wss")

library(cluster)
gap <- clusGap(wybrane_stand, kmeans, K.max=8)
fviz_gap_stat(gap)

set.seed(3)
wynik1 <- kmeans(wybrane_stand, 2, nstart=3)
print(wynik1)
fviz_cluster(wynik1, data= wybrane_stand)

# co mamy w klastrach
baza1$klaster <- wynik1$cluster
table(baza1$klaster, baza1$PROGRAM)

# metoda PAW - klastrowanie wokół medoidóœ
wynik2 <- pam(wybrane_stand,4)
fviz_cluster(wynik2, data= wybrane_stand)

# czy 4 klsatry to optymalna liczba
fviz_nbclust(wybrane_stand, pam, method="wss")
gap2 <- clusGap(wybrane_stand, pam, K.max = 8)
fviz_gap_stat(gap2)

# to moze 6

wynik2 <- pam(wybrane_stand,6)
print(wynik2)


# metoda hierarchiczna
odleglosci <- dist(wybrane_stand)
wynik3 <- hclust(odleglosci)
print(wynik3)
plot(wynik3)
plot(wynik3, hang = -0.1, cex=0.7)


wynik4 <- hclust(odleglosci, method = "ward.D2")
plot(wynik4, hang = 0.1, cex 0.7)

# Zadanie 1:
# Proszę wykonać analizę metodą PAM
# osobno dla płci (Panowie dla siebie, Panie dla siebie)
# biore Panóœ
# czy w tej konfiguracji najlepszym rozwiązaniem będą 3 klastry?
# czy może lepiej jest wyróżnić inną liczbę klastrów





