library(tidyverse)
library(car)
library(lm.beta)

baza1 <- read.table("/home/user0/Documents/R/triathlon.last.R")

baza1 <- baza1 %>% 
  mutate (SWIM_s = as.numeric(as.difftime(SWIM, format="%H:%M:%S", units="secs")))

baza1 <- baza1 %>% 
  mutate (BIKE_s = as.numeric(as.difftime(BIKE, format="%H:%M:%S", units="secs")))

baza1 <- baza1 %>% 
  mutate (RUN_s = as.numeric(as.difftime(RUN, format="%H:%M:%S", units="secs")))

bazaK <- filter(baza1, PROGRAM=="Elite Women")
bazaM <- filter(baza1, PROGRAM=="Elite Men")

ilosciowe <- select(bazaK, c("TOTAL", "SWIM_s", "BIKE_s", "RUN_s", "T1_s", "T2_s"))
ilosciowe <- as.data.frame(scale(ilosciowe))
model_ilosciowe <- lm(TOTAL ~ SWIM_s+BIKE_s+RUN_s, data = ilosciowe)
summary(model_ilosciowe)

hist(bazaK$SWIM_s)
hist(ilosciowe$SWIM_s)

lm.beta(model_ilosciowe)


model_mix <- lm(TOTAL~SWIM_s+BIKE_s+RUN_s+region2,data=bazaK)
summary(model_mix)
plot(model_mix)
lm.beta(model_mix)

macierz <- select(bazaK, c("SWIM_s", "BIKE_s", "RUN_s", "T1_s", "T2_s"))
cor(macierz)

model_int <- lm(TOTAL ~ SWIM_s+BIKE_s+RUN_s+T2_s, data = bazaK)
vif(model_int, type = "predictor")
summary(model_int)
plot(model_int)






