library(tidyverse)
library(car)
baza <- read.csv("/home/user0/Documents/R/shopping_trends.csv", header=T)

shapiro.test(baza$Purchase.Amount..USD.[baza$Season=="Fall"])
shapiro.test(baza$Purchase.Amount..USD.[baza$Season=="Spring"])
shapiro.test(baza$Purchase.Amount..USD.[baza$Season=="Summer"])
shapiro.test(baza$Purchase.Amount..USD.[baza$Season=="Winter"])


model1 <- aov(Purchase.Amount..USD.~Season, data=baza)
qqPlot(model1$residuals, id=F)
hist(model1$residuals)

leveneTest(Purchase.Amount..USD.~Season, data=baza)

summary(model1)

TukeyHSD(model1)



pairwise.t.test(baza$Purchase.Amount..USD.,baza$Season, p.adjust.method="bonferroni")
pairwise.t.test(baza$Purchase.Amount..USD.,baza$Season, p.adjust.method="holm")

install.packages("effectsize")
library(effectsize)
eta_squared(model1)


kruskal.test(baza$Purchase.Amount..USD.,baza$Season)
pairwise.wilcox.test(baza$Purchase.Amount..USD.,baza$Season)


# ZADANIE
# Wykonaj jedną analizę ANOVA i jedną analizę MANOVA na zbiorze iris,
# za każdym razem sprawdzając założenia
# zinterpretuj wyniki modeli wykonując również testy post-hoc i obliczjąc eta







