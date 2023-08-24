
#p value just says if theres evidence of a relationship
#p value doesnt directly say how useful a relationship is, need to look at actual correlation to see how meaningful the relationship is
#you can also look at the R^2 value which tells us how strong the relationship is
#high r value = strong relationship
#small p value = strong evidence of a linear relationship, but doesnt say anything about how tightly packed your points are


### Class Notes/Lab Nov/29/2022
library(openintro)
library(ggplot2)
house <- read.csv("/Users/suj/Downloads/HousePrices.csv")

house2 <- subset(house, Case !=7 & Case != 66)

summary(house2$Price)

## 1
priceSize <- lm(Price ~ Size, data = house2)
summary(priceSize)

priceBaths <- lm(Price ~ Baths, data = house2)
summary(priceBaths)

priceBeds <- lm(Price ~ Beds, data = house2)
summary(priceBeds)

# size is the best indicator of the price of a house, based on the lm model. The Price~Size gave the smallest 
# p-value and had the highest R-squared

## 2
ggplot(data = house2) + 
  geom_point(aes(x = Size, y = Price))

# This changes the randomness of the dataset because we strategically removed 2 data points that were outside of 
# the linear trend of the rest of the data

## 3
#  Price = -72910.5 + 141.8*x
# for every square foot added to a house, $141.80 is added to the price

## 4
# this LITERAL MATHEMATICAL MEANING means that a house with 0 sq. ft would have a price of -$72910.50. This is
# not a problem WITHIN the model, UNLESS we try to predict teh price of a hosue outside of the range we saw 
# (580 - 3990 sq ft).

## 5
houseData <- data.frame(102, 5, 4,  0, 3100)
colnames(houseData) <- c("Case", "Beds", "Baths", "New", "Size" )
predict(priceSize, houseData)

# predicted value = 366617.6
# manual calculation = 366616.8

## 6 
ggplot(data = house2) + 
  geom_point(aes(x = Size, y = Price)) + 
  geom_abline(aes(slope = 141.783, intercept = -72910.478), color = "darkgreen")
# model appears to fit well

## 7
priceBaths <- lm(Price ~ Baths, data = house2)
summary(priceBaths)
# baths is a good predictor because the pvalue is tiny but it's not AS good as size
# assumptions are the same as the price~size model

## 8 
# yes, according to the p-values
ggplot(data = house2) + 
  geom_point(aes(x = Baths, y = Price))
# graph has an upward trend..

model3 <- lm(Price ~ Size + Baths, data = house2)
summary(model3)
# p-value looks good (2.2e-16) which gives sig evidence that coefficient is not 0

cor(house2$Size, house2$Baths)
#highly correlated, so there is dependence
cor(house2$Size, house2$Price)
#size is better predictor of price because there is higher correlation and lower p-value -> strong relationship
cor(house2$Price, house2$Baths)


## 9 
# on their own, each variable is a good predictor, but bath and size are not independent because as you add baths 
# you HAVE to add size which basically negates the "goodness" of the bath variable as a predicter

## 10
model4 <- lm(Price ~ Size + factor(New), data = house2)
summary(model4)
#for every sq ft increase in size, there is a $132 increase in price
#if the house is new, the price will go up by $41,306

## 11

# price = 132.844*Size + 41306.182*New - 63154.488
# because New is either 0 or 1, there will be 2 lines in the model, one shifted up + 41306

## 12

ggplot(data = house2) + 
  geom_point(aes(x = Size, y = Price)) +
  geom_abline(aes(slope = 132.844, intercept = -63154.488), color = "blue") + 
  geom_abline(aes(slope = 132.844, intercept = -63154.488 + 41306.182), color = "red")

## 13
model5 <- lm(Price ~ Size + Size:factor(New), data = house2)
summary(model5)
#strength of relationship is enhanced by removing variables that are saying the same thing
