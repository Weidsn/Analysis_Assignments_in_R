# Assignment 4

library(tidyverse)

df_pressure <- read.csv("coffee_bloodPressure.csv")

df_pressure <- df_pressure |> 
  rename(Coffee=Cups.of.Coffee, Pressure=Blood.Pressure..Systolic.Pressure.)

ggplot(df_pressure, aes(Coffee, Pressure)) +
  geom_point()+
  geom_smooth(method="lm")

df_pressure$id = 1:nrow(df_pressure)

train_pressure <- sample_frac(df_pressure, 0.7)
test_pressure <- anti_join(df_pressure, train_pressure)


lmPressure <- lm(Pressure ~ Coffee, data=train_pressure)
summary(lmPressure)
plot(lmPressure$residuals)

# The relation between coffee consumption and blood pressure is positive but weak.


test_pressure$predicted <- predict(lmPressure, test_pressure)
glimpse((test_pressure))

rmse <- sqrt(mean((test_pressure$predicted - test_pressure$Pressure)^2))
print(rmse)

predict(lmPressure, data.frame(Coffee = c(8,6)))

