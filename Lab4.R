



skittlesRed <- replicate(10000, {
  mean(sample(0:1, size = 40, prob =c(4/5, 1/5), replace = TRUE))
})

meanS <-mean(skittlesRed)
sdS <- sd(skittlesRed)

pnorm(.20, mean = meanS, sd = sdS/sqrt(40))



skittlesTaste <- replicate(10000, {
  sum(sample(0:1, size = 40, prob = c(0.5, 0.5), replace = TRUE))
})

hist(skittlesTaste)

table(skittlesTaste)/10000







n <- 191
red <- 40

#bootstrapping
sims <- replicate(10000, {
  mean(sample(0:1, size = 40, prob = c(1 - (red/n), (red/n)), replace = TRUE))
})


hist(sims)


#calculate p-value
#proportion of test statistics (sample proportions) equal to or more extreme than our claim of p = .2
#more extreme can happen in both directions
mean(sims <= .2) *2


#t-distribution assumes exact symmetric distribution and multiplies by 2

quantile(sims, c(.025, .975))

#since .2 falls in 95% confidence interval



#use a t-test to test claim of p = .2 (mu = .2)
isaacData <- c(rep(1, red), rep(0, n-red))
t.test(isaacData, mu = .2)


#based on the p-value of .7499, we can say the claim is plausible.



#maybe a calibration approach is better




#Lets see if for n = 40 and p = .2, is our test statistic p-hat normally distributed?

pHat <- replicate(10000, {
  mean(sample(0:1, size = 40, replace = TRUE, prob = c(.8, .2)))
  
})

hist(pHat)




#Claim 
#p_{red} = p_{yellow} =  p_{green} =  p_{green} =  p_{purple} = 0.2

skittlesData <- c(rep("red", 40),
                  rep("orange", 24),
                  rep("yellow", 39),
                  rep("green", 47),
                  rep("purple", 41))

skittlesTable <- table(skittlesData)



#Find a test statistic

mean(skittlesTable/length(skittlesData))

#this is a pretty good test statistic
sd(skittlesTable)


#what is distribution of Bryce's test statistic?

bryceTest <- replicate(10000, {
  sd(table(sample(c("red", "orange", "yellow", "green", "purple"),
       size = 191,
       replace = TRUE)))
})

hist(bryceTest)

#calculate p-value
mean(bryceTest >= sd(skittlesData))



#For many "common" tests, there exists a "most powerful" statistic
#Here, the Chi-Square test statistic

#sum((observed - expected)^2/expected)

#(47 - 38.2)^2/38.2 + ... + (39 - 38.2)^2/38.2


chisqTestStat <- sum((skittlesTable - 191/5)^2 / (191/5))


chiTest <- replicate(10000, {
  table <- table(sample(c("red", "orange", "yellow", "green", "purple"),
                        size = 191,
                        replace = TRUE))
  sum((table - 191/5)^2 / (191/5))
})


hist(chiTest)


#calculate p-value
mean(chiTest >= chisqTestStat)


#does a chi-square test
chisq.test(skittlesTable)


#This test has 3 assumptions
#1) data collected w/ randomization
#2) data are independent
#3) all expected cell counts >= 5







