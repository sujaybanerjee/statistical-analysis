library(openintro)


#1 
#we can assume that there is randomness and independence in the collection of the data


dogs <- table(cancer_in_dogs)
chi1 <- chisq.test(dogs)


#chi-square contributions
chi1$residuals^2

#chi-square residuals
chi1$residuals

#there was more cancer than expected in dogs that were exposed, and less in dogs that werent
#trended in direction of the relationship because the largest residuals were with cancer and weed killer
#1.67 sd above than what we would expect if there was no relationship
#positive relationship



#2
#check to see if students and parents do not hear each others answers
#look at dataset help doc
#not independent if they do hear

#one college or random of all colleges


drugs <- table(drug_use$student)


#p = .3

#method 1:
#t.test needs numbers so make 0 and 1 for not and uses

studentData <- c(rep(1, 219), rep(0, 226))

#check np >= 10 and n(1-p) >=10 (can I model my sample proportion with a normal distribution?)
t.test(studentData, mu = .3)


#method 2:

simDrug <- replicate(10000, {
  mean(sample(0:1, size = 445, prob = c(.508, .492), replace = TRUE))
})

hist(simDrug)
quantile(simDrug, c(.025, .975))

mean(simDrug <= .3)

#method 3:
drugMean <- mean(studentData)

#CLT says p-hat ~ N(p, sqrt(p(1-p)/n))
pnorm(drugMean, .3, sd = sqrt(.3*.7/445), lower.tail = FALSE)

#The p-value is so improbable that it is essentially 0



chisq.test(drugs, p = c(.7, .3))


#2.2e-16 is very close to 0

# doing a chi-square goodness of fit test (o-e)^2/e + (o-e)^2/e 

#78.226 is sum of squared differences is 78 which is big when expecting 0



table(race_justice)

chisq.test(table(race_justice))

#check expected cell count assumption
chisq.test(table(race_justice))$expected


#chi-square contributions
chisq.test(table(race_justice))$residuals^2


#see direction
chisq.test(table(race_justice))$residuals

#blacks less likely to believe police treat equally
#whites more likely to believe police treat equally


#how aggregation/disaggregation affects interpretation/certain trend




#does our treatment program have an effect on suicidal
#suicidal idealations/mental health in relation to treatment program
#Enter data

data1 <- matrix(c(130, 63, 52, 116), nrow = 2, ncol = 2)

colnames(data1) <- c("Yes", "No")
rownames(data1) <- c("Yes", "No")


chisq.test(data1)


#McNemar's Test
#2 categorical variables


#does our intervention cause people to change from Yes to No, or from No to Yes?


#(b-c)^2/(b+c)

mcnemar.test(data1, correct = FALSE)

#wellness treatment program seems to have no effect on suicidal ideation

#large p value -> no significant evidence
#35% chance that the difference happened due to random chance


#claim could be true or false, test could reject or fail to reject


#power is probability we dont fail to reject a false claim




#where is the middle 90% of my data?
#Instead, lets find the 90% quantile

qnorm(.9, mean = 0.005, sd = .001/sqrt(25))




#Whats the probability of x_bar_25 <= 5.26 (my cutoff value)
#If mu = 5.5?

pnorm(5.26, mean = 5.5, sd = 1/sqrt(25))
#probability a type 2 error is made


#set alpha
alpha <- .01

#set n
n <- 5

#Set true mean value
trueMean <- 8

cutoff <- qnorm(1-alpha, mean = 5, sd = 1/sqrt(n))


#calculate the probability of a type 2 error:
type2error <- pnorm(cutoff, mean = trueMean, sd = 1/sqrt(n))

#our power is:
#probability that we correctly reject a false claim
1 - type2error



#greater the type 1 error, the smaller the type 2 error and vice versa
#tradeoff
#balance in some way based on context


#can also change error by increasing standard deviation
