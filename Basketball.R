
library(ggplot2)

bball <- read.csv("/Users/suj/Downloads/Basketball.csv")


#Lets start by visualizing Sagarin Rating

ggplot(data = bball) + geom_histogram(aes(x = Sagarin.Rating))


# What makes a team "good" (as defined by Sagarin Rating) ?
ggplot(data = bball) + geom_boxplot(aes(x = Sagarin.Rating,
                                         fill = factor(NCAA)))

# Are Free Throws a good indicator of a successful team?

#use a linear model of free throws to predict sagarin rating
# Make new variable in data set
bball$FTPercent <- bball$FTM/bball$FTA

model1 <- lm(Sagarin.Rating ~ FTPercent, data = bball)
summary(model1)

#the p-value of .001 gives a very strong(significant) positive relationship between free throws and sagarin

cor.test(bball$Sagarin.Rating, bball$FTPercent)
#same p-value

#Lets visualize
ggplot(data = bball) + 
  geom_point(aes(x = FTPercent, y = Sagarin.Rating)) + 
  geom_point(
    size = 3) +
  geom_smooth(method = "lm", se = FALSE)

# IF the FT % goes up by 70% to 80%, the Sagarin rating goes up by 3.69 because you have to divide the 36.896 by 10
# Raising FT % by 10 is really hard, but only a little

#Check my model assumptions
plot(model1)


# The data is not independent; teams are the same over the years 
# data is not a random sample since it is all major league conference


# Lets instead use BLocks to predict success
model2 <- lm(Sagarin.Rating ~ Blocks, data = bball)

ggplot(data = bball) + 
  geom_point(aes(x = Blocks, y = Sagarin.Rating))

cor(bball$Sagarin.Rating, bball$Blocks)
# this is R

plot(model2)

summary(model2)

#Our R^2 is the percent of variation in Sagarin.Rating directly explained by Blocks

#We can explain 19% of our variation is explained

#Adjusted R^2 is for if there's too many variables in the model, it accounts for the increase 
# in variation that goes up or down when you add variables

# As we add variables to our model, it is important to look at each of those variables individually

ggplot(data = bball) + 
  geom_point(aes(x = PossessionsPerGame, y = Sagarin.Rating))

ggplot(data = bball) + 
  geom_point(aes(x = PossessionsPerGame, y = O.PossessionsPerGame))



#Can we find the "best" model using as many variables as possible?

bigModel <- lm(Sagarin.Rating ~ factor(NCAA) + FGMPerGame, data = bball)

summary(bigModel)

#Sagarin = 52.6 +7.7(NCAA) + 1.02(FGMPerGame)
#for every FG made, their sagarin rating goes up by about 1
#factor term either adds 7.7 or 0

#Lets investigate: how many FG do teams make per game?

summary(bball$FGMPerGame)

summary(bball$Sagarin.Rating)


#our model explains around 70% of all the variation in Sagarin rating (R^2)

#Lets use all of our variables
realBigModel <- lm(Sagarin.Rating ~. -Line -RPI, data = bball)

summary(realBigModel)

#Not a great idea to use every variable because there are highly correlated with each other

#Some variables do not have unique information to offer


#Introducing the "step" function

step(realBigModel)


#AIC: Akaike Information Criterion

# AIC = 2k - 2ln(L)
# L = How likely is our model to be true?
# k = # of variables in our model

#Lower AIC means model is more likely

finalModel <- lm(formula = Sagarin.Rating ~ Team + Year + NCAA + Sagarin.Rank + 
                   Games + FGM + FGA + X3FGM + FTM + FTA + Assists + Blocks + 
                   Steals + Points + O.FGA + O.3FGM + O.OREB + O.PF + O.Assists + 
                   O.Blocks + O.Points + FGMPerGame + FGAPerGame + X3FGMPerGame + 
                   X3FGAPerGame + FTMPerGame + FTAPerGame + DREBPerGame + PFPerGame + 
                   AssistsPerGame + StealsPerGame + PointsPerGame + O.FGMPerGame + 
                   O.FGAPerGame + O.3FGMPerGame + O.FTMPerGame + O.OREBPerGame + 
                   O.AssistsPerGame + O.TOPerGame + O.BlocksPerGame + O.PointsPerGame + 
                   AssistsTORatio, data = bball)


predict(finalModel, bball[1,])

#Suppose you have two competing models
m1 <- lm(Sagarin.Rating ~FTM, data = bball)
m2 <- lm(Sagarin.Rating ~ Steals, data = bball)

AIC(m1)
AIC(m2)
#doesnt tell you anything about practical significance 

#smaller number means the model is more likely to be better fit after penalization scheme (penalized for adding variable)
