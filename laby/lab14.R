library(effectsize)
library(MASS)
library(tidyverse)

baza <- iris
model4 <- manova(cbind(Petal.Length,Petal.Width)~Species, data=baza)
summary(model4)
eta_squared(model4)
summary.aov(model4)

baza %>%
  group_by(Species) %>%
  summarise(x1 = mean(Petal.Length), mean(Petal.Width),
            sd1 =sd(Petal.Length), sd2=sd(Petal.Width)) %>%
  ungroup()

baza[,1:4] <- scale(baza[,1:4])
lda_model <- lda(Species~cbind(Petal.Length, Petal.Width), data=baza)


wyniki_lda <- data.frame(
  species=baza[, "Species"],
  lda = predict(lda_model)$x)

ggplot(wyniki_lda) +
  geom_point(aes(x=lda.LD1, y=lda.LD2, color = species), size = 2) +
  theme_classic()


baza3 <- read.table("/home/user0/Documents/R/triathlon_30.01.25.R")

baza3[,16:20] <- scale(baza3[,16:20])

lda_model2 <- lda(region~cbind(SWIM_s,  BIKE_s, RUN_s, T1_s, T2_s), data = baza3, CV=F)
lda_model2

lda_df <- data.frame(
  region = baza3[,"region"],
  lda = predict(lda_model2)$x
)

ggplot(lda_df)+
  geom_point(aes(x= lda.LD2, y=lda.LD3, color = region), size=2) +
  theme_classic()


# za pomocą pięciu zmiennych zobrazować dyskrymincję grup programu (Elite Men Elite Women)``

lda_model3 <- lda(PROGRAM~cbind(SWIM_s, BIKE_s, RUN_s, T1_s, T2_s), data = baza3, CV=F)


lda_df2 <- data.frame(
  region = baza3[,"PROGRAM"],
  lda = predict(lda_model3)$x
)

ggplot(lda_df)+
  geom_boxplot(aes(x=LD1, color = program)) +
  theme_classic()



