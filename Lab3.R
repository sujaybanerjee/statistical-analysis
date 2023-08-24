classNumbers <- read.csv("/Users/suj/Downloads/classNumbers.csv", header = FALSE)


#CLT any sample of any size from the population has a mean that follows a normal distribution


#lets take a sample of size n=49

sampleMeans <- replicate(10000, {
  mean(sample(classNumbers$V1, size = 49))
})

hist(sampleMeans)

hist(classNumbers$V1)

#mu
mean(classNumbers$V1)

#sigma
sd(classNumbers$V1)
#335.241

335.241/7
#47.89

#Does the CLT hold for our population of "random" number?
mean(sampleMeans)


sd(sampleMeans)




#Lets consider all GPAs at Midd

#average GPA is 3.71 with sd = 0.4

#Suppose I take random sample of n = 9 students
#what are "plausible" values for (xbar sub9) x_9 (the mean of our 9 GPAs)




gallupTaboo<- read.csv("/Users/suj/Downloads/GallupTaboo.csv")


#1
sampleDrinks <- replicate(10000, {
  mean(sample(gallupTaboo$Alcohol, size = 100, replace = TRUE))
})

mean(sampleDrinks)
sd(sampleDrinks)
hist(sampleDrinks)


#claim mu = 9.15 

sd(gallupTaboo$Alcohol)/10
mean(gallupTaboo$Alcohol)
#claim not true because mean is 4.52 not 9.15



#2
weed <- replicate(10000, {
  sample(gallupTaboo$Cannabis, size = 100, replace = TRUE)
})
table(weed)

mean(weed)

weedSim <- replicate(10000, {
  sample(0:1, size = 100, prob = c(.71, .29), replace = TRUE)
})
mean(weedSim)




#1 

#Calculate probabilities with normal distribution
#pnorm()

pnorm(9.15, mean = 9.15, sd = .445)
#0.5 less than 9.15

pnorm(4.52, mean = 9.15, sd = .445)
#1.182541e-25
#sooooooo unlikely, claim probably not true


pnorm(14.52, mean = 9.15, sd = .445, lower.tail = FALSE)




