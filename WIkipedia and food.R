# Assessing claims about a single population mean

# 1) Simulation
# 2) Boostrapping (a special simulation from our data)
# 3) CLT


#How do I assess claims about two groups?

#Jupiter vs Zebra
wiki <- read.csv("/Users/suj/Downloads/Wikipedia Times - Sheet1.csv")


#Remove rows with missing values
wikiNoMissing <- na.omit(wiki)


#Lets test the claim that navigating to Zebra and Jupiter from St. Anselm's Pink City Sr. Sec. School, Jaipur
#is equally difficult

#To start, lets assume that equally difficult means the same average time to get to the page


#Prepare: alpha = 0.05, we need a test statistic: 
#find the difference between the group means
#(x-bar_{zebra} - x-bar_{jupiter})

zebra <- subset(wikiNoMissing, Target == "Zebra")
jupiter <- subset(wikiNoMissing, Target == "Jupiter")

realTestStatistic <- mean(zebra$Time) - mean(jupiter$Time)



#Check: Our data were gathered independently
#our subjects we randomly sampled


#Calculate:
#How do I calculate the probability tof observing a test statistic of 54.65 *or something more extreme*
#just by random chance?


#In a randomization test, it doesnt matter if you shuffle up data if assumptions say means should be same

#Our goal: shuffle the order group variable ("Target")

testStatistic <- replicate(10000, 
                           {sample(wikiNoMissing$Target)


shuffleData <- data.frame(Time = wikiNoMissing$Time, shuffledTarget = sample(wikiNoMissing$Target))

zebra <- subset(shuffleData, shuffledTarget == "Zebra")
jupiter <- subset(shuffleData, shuffledTarget == "Jupiter")

mean(zebra$Time) - mean(jupiter$Time)
})

hist(testStatistic)


#Lets calculate a p-value to quantify how unusual it is to get a test statistic of 56 by random chance



#Count up:how many of my simulated/randomized test statistics are equal to or more extreme than my REAL test statistic

mean(testStatistic >= realTestStatistic)*2

#my p-value is ~.19


#Neat trick:
#These are the same
x <- c(0,0,1,0)
x <- c(FALSE, FALSE, TRUE, FALSE)
mean(x)

#Taking the mean of a boolean vector gives the PROPORTION of TRUEs


#FIX CODE^^^^






#Lets look at IceCream
library(resampledata)


#today we are going to test the claim that chocolate and vanilla ice cream have the same average calories

#mu_chocalate = mu_vanilla
IceCream
#command click to view


#Prepare: alpha = .05, x-bar_choc - x-bar_van = 
realTestStatIceCream <- mean(IceCream$ChocolateCalories) - mean(IceCream$VanillaCalories)


#Check: Our data are randomly sampled

#With paired data, we can simply test the hypothesis that the DIFFERENCE is equal to 0

t.test(IceCream$ChocolateCalories - IceCream$VanillaCalories)



#fast food restaurants

fastFood <- read.csv("/Users/suj/Downloads/Datafiniti_Fast_Food_Restaurants.csv")

mcdBK <- subset(fastFood, name == "McDonald's" | name == "Burger King")

#Dou our random sample of MCd and Burger Kind give any evidence against the claim that BK and MCd have the same average 
#latitude

BK <- subset(mcdBK, name == "Burger King")
MCD <- subset(mcdBK, name == "McDonald's")

realtTestStatBKMCD <- mean(BK$latitude) - mean(MCD$latitude)

#Check: Data are randomly sampled and independent
#Proceed with caution (data may not be independent)


t.test(BK$latitude, MCD$latitude)



IceCream

#what happens if I incorrectly do a two-sample t-test?
t.test(IceCream$ChocolateCalories, IceCream$VanillaCalories)


#test statistic
mean(IceCream$ChocolateCalories) - mean(IceCream$VanillaCalories)


#Doing the correct test:
#Matched-pairs t-test
t.test(IceCream$ChocolateCalories, IceCream$VanillaCalories, paired = TRUE)



#Lets investigate sd
sd(IceCream$ChocolateCalories)
sd(IceCream$VanillaCalories)


#calculate paired differences
diff <-IceCream$ChocolateCalories - IceCream$VanillaCalories
hist(diff)
sd(diff)


#Less difference when looking at each company for both flavors rather sd of chocolate and vanilla icecream in general
#Paired nature reduces sd

#Think about if I have two independent groups or are they paired





#another example
library(openintro)
textbooks


#Are books, on average cheaper at UCLA or Amazon?
#Test the claim that book prices, on average, are the same at both websites


#Incorrect 2-sample t-test
t.test(textbooks$ucla_new, textbooks$amaz_new)


#Correct test is matched pairs t-test
t.test(textbooks$ucla_new, textbooks$amaz_new, paired = TRUE)

#p-value says that claim that we observed a mean difference if the claim that they are the same price is almost impossible



#End of talking about quantitative variables



#Start talking about categorical variables
#We can simulate sliams related to binary variables


#How can CLT tell us about claims about variables?


#1 from HW2

p <- .08
n <- 300
seNearSighted <- sqrt(p*(1-p)/n)



#CLT approach has an additional assumption
#Our sample size is "big enough"

#Try to sample n= 10000 and calculate proportion with rare disease with incidence rate of .001%


#proportion
x <- replicate(1000, {
  mean(sample(0:1, size = 1000000, replace = TRUE, prob = c(.99999, .00001)))
})
hist(x)


#Assumption: np >= 10 and n(1-p) >= 10
