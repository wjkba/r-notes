library(tidyverse)
library(questionr) # nowy pakiet
library(DescTools)
library(knitr)

liderzy <- read.csv("/home/user0/Documents/R/liderzy.csv")
write.table(liderzy, "/home/user0/Documents/R/liderzy.R")
liderzy <- read.table("/home/user0/Documents/R/liderzy.R")

freq(liderzy$z1.wiedza.dziedzinowa)
liderzy <- filter(liderzy, Calkowity.czas.wypelniania!="")


# zapis tabeli krzyżowej w postaci formuły
ltabs(liderzy$z1.wiedza.dziedzinowa)

# podstawy tabel krzyżowych - 3 procentowania i ich interpretacja
prop(table(liderzy$z1.wiedza.dziedzinowa, liderzy$plec))
rprop(table(liderzy$z1.wiedza.dziedzinowa, liderzy$plec))
cprop(table(liderzy$z1.wiedza.dziedzinowa, liderzy$plec))

# Test chi kwadrat
chi1 <- chisq.test(liderzy$z1.wiedza.dziedzinowa, liderzy$plec)
print(chi1$observed)
print(chi1$expected)

# dokładny test niezależności, gdy liczebności oczekiwane są niskie
fisher.test(liderzy$z1.wiedza.dziedzinowa, liderzy$plec)

# alternatywnie symulacja Monte Carlo jako argument testu chi2
chisq.test(liderzy$z1.wiedza.dziedzinowa, liderzy$plec, simulate.p.value = TRUE)

# miara siły swiązku nr 1 (normalne-nominalne + mieszane) - V Kramera
# zwraca wartość miary od 0 do 1
cramer_wynik <- cramer.v(table(liderzy$z1.wiedza.dziedzinowa, liderzy$plec))
cramer_wynik

freq(liderzy$staz) # wynik jest nieuporządkowany
cprop(table(liderzy$z1.wiedza.dziedzinowa, liderzy$staz))

liderzy$staz2 <- factor(liderzy$staz, levels = c("0-2", "3-5", "6-10", "11-15", "16-20", "powyzej 20 lat"))
freq(liderzy$staz2)
liderzy$z1.wiedza.dziedzinowa <- factor(liderzy$z1.wiedza.dziedzinowa, levels = c("To nie jest dla mnie wazne", "Trochę ważne", "Ważne (albo nie najważniejsze)", "Jest to dla mnie bardzo ważne"))

freq(liderzy$z1.wiedza.dziedzinowa)
cprop(table(liderzy$z1.wiedza.dziedzinowa))
print(cprop(table(liderzy$z1.wiedza.dziedzinowa, liderzy$staz2), total=F), width=120)

KendallTauB(table(liderzy$z1.wiedza.dziedzinowa, liderzy$staz2),conf.level = 0.95)
