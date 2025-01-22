library(tidyverse)
baza <- read.csv("/home/user0/Documents/R/shopping_trends.csv")
baza$flag <- case_when(baza$Purchase.Amount..USD. > 80 ~ 1, .default = 0)

model1 <- glm(flag~Age+Gender+Season, data=baza, family = "binomial")
summary(model1)

exp(-0.0334035)
1-exp(-0.0334035)

library(pscl)
pR2(model)["McFadden"]

library(car)
vif(model1)

#//

model2 <- glm(flag ~ Age + Gender + Payment.Method, 
              data = baza, family = "binomial")




summary(model2)

exp(0.2061067) 
1 - exp(-0.2061067)  

library(pscl)
pR2(model2)["McFadden"]
