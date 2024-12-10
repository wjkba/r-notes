library(tidyverse)
library(questionr)
library(DescTools)
library(knitr)

liderzy <- read.csv("/home/user0/Documents/R/liderzy.csv")
write.table(liderzy, "/home/user0/Documents/R/liderzy.R")
liderzy <- read.table("/home/user0/Documents/R/liderzy.R")

freq(liderzy$z1.wiedza.dziedzinowa)
liderzy <- filter(liderzy, Calkowity.czas.wypelniania!="")


doPCA <- liderzy %>%
  select(starts_with("UWES"))

doPCA <- doPCA %>%
  mutate(across(1:9, ~ case_match(
    .,
    "Nigdy" ~ 0,
    "Kilka razy w roku lub rzadziej" ~ 1,
    "Raz w miesiącu lub rzadiej" ~ 2,
    "Kilka razy w miesiący" ~ 3,
    "Raz w tygodniu" ~ 4,
    "Kilka razy w tygodniu" ~ 5,
    "Zawsze" ~ 6,
    .default = NA_real_ # Do obsługi innych wartości
  )))

library(psych)
library(ggcorrplot)
corr <- cor(doPCA,use="pairwise.complete.obs")
ggcorrplot(corr)


doPCA_full <- na.omit(doPCA)

wynik2 <- prcomp(doPCA_full, scale= T)

eigenvalues <- wynik2$sdev*2
eigenvalues
summary(wynik2)
plot(wynik2)
biplot(wynik2)

modelPCA <- principal(doPCA_full, nfactors = 2, rotate="None",
                      missing = T, impute = "mean")

modelPCA <- principal(doPCA_full, nfactors, rotate="varimax")

summary(modelPCA)
loadings(modelPCA)
modelPCA$scores
wymiary <- modelPCA$scores
liderzy_wymiary <- cbind(liderzy,wymiary)
