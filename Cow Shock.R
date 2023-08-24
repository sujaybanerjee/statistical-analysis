
cow <- read.csv("/Users/suj/Downloads/CowshockData.csv")

# What are some good predictors of a cow twitching?

colnames(cow)[1] <- "Current"


#Lets build a linear model

cowModel <- lm(Twitch ~., data = cow)

summary(cowModel)


#Lets check our model assumptions
plot(cowModel)

# ln(p/(1-p)) = Beta_0 + Beta_1*Current
# p = probability of a cow twitching
# p/(1-p) is the odds


#modeling log odds linearly forces probability to be between 0 and 1


#Lets build our first logistic regression model
model1 <- glm(Twitch ~ Current, data = cow, family = "binomial")

summary(model1)



# What  is the probabilty a cow twitches with a current of 3 amps

beta0 <- -.5075
beta1 <- 0.4897
current <- seq(from = 0, to = 10, by = .1)

p <- exp(beta0 + beta1*current)/(1 + exp(beta0 +beta1*current))

plot(current, p)


predict.glm(model1, data.frame(Current = seq(from = 0, to = 10, by = .1)), type = "response")



#Lets build a bigger model
bigLogisticModel <- glm(Twitch ~., data = cow, family = "binomial")

summary(bigLogisticModel)
step(bigLogisticModel)


library(dplyr)

cow$Sex <- recode(cow$Sex, "0" = "Male", "1" = "Female")

finalLogisticModel <- glm(Twitch ~ Current + Sex, data = cow, family = "binomial")

summary(finalLogisticModel)


