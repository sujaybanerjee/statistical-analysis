

sample(0:1, size = 10, prob = c(2/6, 4/6), replace = TRUE)


data <- c(rep(0,21), rep(1, 19))


trail1 <- table(replicate(10000, {
  mean(sample(0:1, size = 1, prob = c(2/6, 4/6), replace = TRUE))
}))




trial2 <- table(replicate(10000, {
  mean(sample(0:1, size = 1, prob = c(4/6, 2/6), replace = TRUE))
}))

trail1 + trial2



#data
#zeros <- 27
#ones <- 13

dataVector <- c(rep(0,27), rep(1, 13))


#Lets simulate what would happen with X spies and N-X agents

sims <- replicate(10000, {
spies <- 4
agents <- 0



spiesNumbers <- sample(0:1, size = 10*spies, prob = c(2/3, 1/3), replace = TRUE)

agentsNumbers <- sample(0:1, size = 10*agents, prob = c(1/3, 2/3), replace = TRUE)

sum(c(spiesNumbers, agentsNumbers))
})


hist(sims)

#Calculate a p-value for test stat = 13 (actual numbers of 1's)

mean(sims <= 13)*2
#very plausible that 4 spies



