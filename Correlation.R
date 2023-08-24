
#We can conduct a formal statistical test for correlation

library(openintro)

#Lets look at births data
births

#Claim: there is no correlation between mother's age and child's weight at birth (r = 0)



#Prepare: Calculate the correlation, r
#birthNoNA <- na.omit(births)

cor(births$m_age, births$weight)

#r = 0.04 is very weak if not no correlation, but is it statistically significant?

#Calculate:
cor.test(births$m_age, births$weight)

#p-value is large, so differences likely due to random chance
#0 is in the 95% confidence interval


#Practice one more:
#weeks of gestation and weight of baby
cor.test(births$weeks, births$weight)

#p-value so small, so very likely that there is a correlation



#Lets learn about linear regression

#What is it? And what are the assumptions?



#LSLR is least squares linear regression
#minimize sum of squares

library(tidyverse)

house <- read.csv("/Users/suj/Downloads/HousePrices.csv")

#Lets examine the relationship between size (in sq. feet) and Price
cor(house$Size, house$Price)
cor.test(house$Size, house$Price)

#Lets do Least Squares Regression
model1 <- lm(Price ~ Size, data = house)

#y = beta_0 + beta_1*x
# Price = beta_0 + beta_1*Size

# Price = -31923.7 + 119.8*Size (model1)

#What are the assumptions of linear regression?
#(We need to check that LSLR is "best")

#  1) Data values independent and collected using randomization
#In Gainesville, Florida these houses were randomly selected, so it can only be generalized to Gainesville
#If there are houses near each other, there is dependence, but we can assume the 101 out of million population is independent

#  2) Residual values are normally distributed for any given X ~ N(0, sigma). (residual value of mean 0, and some fixed value sigma of st.dev)
#that second part means that the standard deviations are relativel fixed. If they aren't, then the values with small st.dev are being ignored
#(residual = observed - predicted)
#when the model misses, it misses in a normal fashion
#tilt your head and make the line the center of this normal distribution. 
#The points should fall relatively normally around this line. Draw a few normal curves to see

#  3) Data are linearly related


#Lets check these assumptions!

# a perfect residual plot is a nice horizontal band around y = 0 that makes sense in context

plot(model1)


#Lets investigate out two potential outliers:
#house 7 and 66 

house[7, ]


ggplot(data = house) +
  geom_point(aes(x = Size, y = Price))


#if you choose to remove the point, it is because residual is very high and messes up line
#but this messes up the randomized assumption








