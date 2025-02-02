library(tidyverse)
library(car)
library(effectsize)


baza_iris <- iris


shapiro.test(iris$Sepal.Length)


iris %>%
  group_by(Species) %>%
  summarise(shapiro_p = shapiro.test(Sepal.Length)$p.value)

# p większe od 0.05
# nie ma podstaw do odrzucenia hipotezy o normalności rozkładu Sepal.Length


# ANOVA

model_anova <- aov(Sepal.Length ~ Species, data = iris)

qqPlot(model_anova$residuals, id=F)
hist(model_anova$residuals)


leveneTest(Sepal.Length ~ Species, data = iris)
# wariancje Sepal.Length między grupami są istotnie różne, p mniejsze od 0.01


summary(model_anova)
# statystycznie różnice w średniej długości działek 
# kielicha między co najmniej dwoma gatunkami irysów




TukeyHSD(model_anova)
# wszystkie p-wartości  są równe 0,
# wszystkie pary gatunków różnią się istotnie pod względem średniej długości działek kielicha

pairwise.t.test(iris$Sepal.Length, iris$Species, p.adjust.method="bonferroni")
pairwise.t.test(iris$Sepal.Length, iris$Species, p.adjust.method="holm")

eta_squared(model_anova)

kruskal.test(Sepal.Length ~ Species, data = iris)

pairwise.wilcox.test(iris$Sepal.Length, iris$Species)




# MANOVA 

iris %>%
  group_by(Species) %>%
  summarise(across(where(is.numeric), ~ shapiro.test(.x)$p.value))

model_manova <- manova(cbind(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) ~ Species, data = iris)

summary(model_manova)
