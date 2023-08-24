#Lets calculate the E(X): expected number of peppers I eat before getting a spicy pepper

#Lets create an "X" vector

X = 5 
X <-5


#Lets make X be the integers 1 to 100
X <- 1:100000


#Lets make our "p" vector
p <- .1 * .9^(X-1)


#Now sum X * p to find E(X)
e <-sum(X*p)




#Reading data into R:
covid <- read.csv("/Users/suj/Downloads/us-counties-COVID.csv")




#Bruck: "In California, the number of deaths is increasing over time"

covidCalifornia <- subset(covid, 
                          state == "California")


#Find avg daily cases for California

#Extract a column with $
mean(covidCalifornia$cases)

