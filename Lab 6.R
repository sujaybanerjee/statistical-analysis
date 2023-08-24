
#1

#set n
n <- 4371

#set alpha
alpha <- .01

#claimed mean
claimedMean <- .5

#true mean
trueMean <- .47


#type 1 error
cutoff <- qnorm(1-alpha, mean = .47, sd = sqrt((trueMean*(1-trueMean))/n))


#type 2 error
pnorm(cutoff, mean = claimedMean, sd = sqrt((claimedMean*(1-claimedMean))/n))



#2

#claimed mean

n1 <- 10
n2 <- 50
n3 <- 500

claimedMean2 <- .51

#type 1 error
cutoff2 <- qnorm(1-alpha, mean = .47, sd = sqrt((trueMean*(1-trueMean))/n3))


#type 2 error
type2 <- pnorm(cutoff2, mean = claimedMean2, sd = sqrt((claimedMean2*(1-claimedMean2))/n3))

#power
power <- 1 - type2
#.019
#.039
#.297


powers <- matrix(c(10, 50, 500,
                 .0192, .03947, .2968),
                 nrow = 3,
                 ncol = 2)

plot(powers)




#3

#calculate cutoff
alpha <- .01

cutoff <- qnorm(1-alpha, mean = 4, sd = 2.2/sqrt(50))

#calculate type 2
type2 <- pnorm(cutoff, mean = 5, sd = 2.2/sqrt(50))

#calculate power
1 - type2
#or do lower.tail = FALSE

#81% that I will correctly reject claim
#19% that I incorrectly accept claim











