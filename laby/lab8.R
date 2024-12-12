library(tidyverse)
library(questionr)
library(DescTools)
library(knitr)
library(FactoMineR)
library(factoextra)

baza <- read.csv("/home/user0/Documents/R/liderzy2611.csv", header = T)

baza_full <- filter(baza, Calkowity.czas.wypelniania!="")

doMCA <- baza_full %>%
  select(c(starts_with("z1"),plec,branza,wielkosc,wiek))

wynikMCA <- MCA(doMCA, quanti.sup = 10, quali.sup = 7:9)

summary(wynikMCA)

plot.MCA(wynikMCA)

fviz_mca_ind(wynikMCA, repel = T)

plot.MCA(wynikMCA, invisible = c("var", "quali.sup","quanti.sup"), cex=0.6,autoLab = "yes")
fviz_mca_ind(wynikMCA, repel=TRUE, ggtheme=theme.minimal())

plot.MCA(wynikMCA, invisible= c("ind"), cex=0.6, autoLab="yes", selectMod="cos")
fviz_mca_ind(wynikMCA, repel=TRUE, ggtheme= theme_minimal())
