#we are statisticians for Pew research trying to predict 2042 election
#Bernie Sanders vs. Baron Trump
#media outlet X claims the race is perfectly even



#suppose we have a $5,000 budget
#n = 125   of people surveyed

#How many of our 125 would have to say "Trump" or "Sanders" for us to NOT believe Media X's claim



#Lets simulate assuming Media X is telling the truth

#Let "S" represent the number of people who claim they would vote for Sanders

simVotes <- replicate(100000, {sum(sample(0:1, size = 125, prob = c(.5, .5), replace = TRUE))})

hist(simVotes)

mean(simVotes)


#lets find middle 90% 
quantile(simVotes, .05)
quantile(simVotes, .95)

#lets find middle 99% 
quantile(simVotes, .005)
quantile(simVotes, .995)





#Claim: Students love all 3 dining halls equally
#The number of students whose favorite dining hall is Proctor is the 
#same as the number for Atwater, Proc, and Ross


#lets take a "random" sample (from all of us)

diningData <- c("R", "R","R","R","R","R",
                "P","P","P","P","P","P","P",
                "A","A","A","A","A","A","A","A")

table(diningData)

#lets simulate with n=21



diningData2 <- c("R", "R","R","R","R","R",
                 "A","A","A","A","A","A","A","A")

table(diningData2)



ross <- replicate(10000, 
                      {sum(sample(0:1, size = 14, prob = c(.5, .5), replace = TRUE))})

atwater <- 14 - ross

hist(ross)
hist(atwater)
hist(atwater - ross)



probabilityDist <- table(atwater - ross)/10000

difference <- atwater - ross



ggplot(data = data.frame(probabilityDist)) + 
  geom_col(aes(x = Var1, y = Freq))




bootAtwater <- replicate(10000, 
          {sum(sample(0:1, size = 14, prob = c(6/14, 8/14), replace = TRUE))})

bootRoss <- 14 - bootAtwater

bootDiff <- bootAtwater - bootRoss



hist(bootDiff)




allDining<- replicate(10000, 
             {table(sample(c("A","P","R"), size = 21, 
                      prob = c(1/3, 1/3, 1/3), 
                      replace = TRUE))/21})

difference <- 1/3 - allDining




#statistic 1: A - P - R (around -7)
#statistic 2: Calculate standard deviation of our data
#statistic 3: max- min


# actual values for our data for 1,2,3
#statistic 1: A - P - R: 8-7-6 = -5 (not a good stat)
#statistic 2:  1
sd(c(8,7,6))
#statistic 3: 8-6 = 2


#lets simulate

sample1 <- sample(c("A", "P", "R"), size = 21, replace = TRUE, prob = c(1/3,1/3,1/3))



table1 <- table(sample1)

stat2 <- sd(table1)
stat3 <- max(table1) - min(table1)

#simulate for stat 2
stat2sim <- replicate(10000,{
  sample1 <- sample(c("A", "P", "R"), size = 21, replace = TRUE, prob = c(1/3,1/3,1/3))
  
  table1 <- table(sample1)
  sd(table1)
  
})
hist(stat2sim)


#simulate for stat 3
stat3sim <- replicate(10000,{
  sample1 <- sample(c("A", "P", "R"), size = 21, replace = TRUE, prob = c(1/3,1/3,1/3))
  
  table1 <- table(sample1)
  max(table1)-min(table1)
  
})
hist(stat3sim)


#representative  sample averages

avgRepSample <- c(10.4, 16.2, 11.1, 10, 14.2, 16.3, 
                  15.6, 10.1, 8.8, 7.6, 12.3, 8.8,
                  10.1, 9.9, 9.2, 10.7, 8.1, 10,
                  10.7, 10, 9.2, 11.1, 10.9, 11.4)

mean(avgRepSample)
hist(avgRepSample)

#take a simple random sample (srs)
sample(1:100, size = 10)

#central limit theorem (CLT)
#the sample mean from ANY population follows a normal distribution
#the mean of distribution is the SAME as the mean of our population

avgSRS <- mean(c(9,5,8,12,1,1,12,9,10,8))
