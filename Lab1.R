plants <- read.csv("/Users/suj/Downloads/Plants.csv")
install.packages("tidyverse")
install.packages("ggplot2")
library(tidyverse)
library(ggplot2)

finalBiomass1 <- plants |>
  subset(Species ==1) 

finalBiomass1h <- finalBiomass1$BiomassT2
  
hist(finalBiomass1h, breaks = 20)

prob1 <- finalBiomass1 |>
  filter(BiomassT2 == 300) |>
  nrow()/nrow(plants)
#not really, slightly high




finalBiomass2 <- plants |>
  subset(Species ==2) 

finalBiomass2h <- finalBiomass2$BiomassT2

hist(finalBiomass2, breaks = 20)

prob2 <- finalBiomass2 |>
  filter(BiomassT2 == 300) |>
  nrow()/nrow(plants)

#fairly likely





finalBiomass3 <- plants |>
  subset(Species == 3) 

finalBiomass3h <- finalBiomass3$BiomassT2

hist(finalBiomass3, breaks = 20)


prob3 <- finalBiomass3 |>
  filter(BiomassT2 == 300) |>
  nrow()/nrow(plants)

#pretty likely


#Check out the experimental design
table(plants$Row, plants$Column)
table(plants$Fert, plants$Species)
table(plants$Fert, plants$Column) #not great (cant compare columns)


plants %>%
  filter(Species == 1) %>%
  nrow()

#4/34

x <- (1:5)
#num plants bought

p <- (4/34)*(30/34)^(x-1)
#probability

sum(x*p)
#expected value
#1.28

#not correct^




ggplot(data = plants) + geom_histogram(aes(x = BiomassT2))


ggplot(data = plants) + geom_bar(aes(x = Species))


#lets graph biomassT1 and biomassT2


ggplot(data = plants) + 
  geom_point(aes(x = BiomassT1, y = BiomassT2))



#Lets make some pretty graphs and calculate some probabilities

#I could pick 5 random rows and from my data of Species 1 hydrangeas 
#and look at those 5 biomasses

species1 <- subset(plants, Species == 1)

#pick 5 hydrangeas
selectedPlants <- sample(1:34, size = 5)


#Get biomasses of the 5 selected hydrangeas
sum(species1[selectedPlants,"BiomassT2"] < 200)


#replicate()
simProbs <-replicate(100000, {
  selectedPlants <- sample(0:1, size = 5, prob = c(30/34, 4/34), replace = TRUE)
  sum(species1[selectedPlants,"BiomassT2"] < 200)
  })

hist(simProbs)

#Lets find the number of 0s
zeroProb <- sum(simProbs == 0)/100000
#.51027


#exact probability
(30/34)*(29/33)*(28/32)*(27/31)*(26/30)

#table
table(simProbs)/100000





mean(simProbs)







sample(0:1, size = 5, prob = c(30/34, 4/34), replace = TRUE)




#sum above vector
simInfinite <-replicate(100000, 
                        {sum(sample(0:1, size = 5, prob = c(30/34, 4/34), replace = TRUE))})

hist(simInfinite)


#expected number of bad hydrangeas
mean(simInfinite)
















##########################
#HW1

#1

ggplot(finalBiomass1, aes(x=BiomassT2)) +
  geom_boxplot(fill = "dodgerblue4") +
  theme_classic() + 
  labs(title = "Final Biomass of Species 1")

ggplot(finalBiomass2, aes(x=BiomassT2)) +
  geom_boxplot(fill = "dodgerblue2") +
  theme_classic() + 
  labs(title = "Final Biomass of Species 2")

ggplot(finalBiomass3, aes(x=BiomassT2)) +
  geom_boxplot(fill = "dodgerblue3") +
  theme_classic() + 
  labs(title = "Final Biomass of Species 3")



#2
fertBiomassA <- plants |>
  subset(Fert == "A")

fertBiomassB <- plants |>
  subset(Fert == "B")

fertBiomassC <- plants |>
  subset(Fert == "C")

ggplot(fertBiomassA, aes(x=BiomassT2)) +
  geom_boxplot(fill = "dodgerblue4") +
  theme_classic() + labs(title = "Final Biomass Fertilizer A")

ggplot(fertBiomassB, aes(x=BiomassT2)) +
  geom_boxplot(fill = "dodgerblue2") +
  theme_classic() + 
  labs(title = "Final Biomass Fertilizer B")

ggplot(fertBiomassC, aes(x=BiomassT2)) +
  geom_boxplot(fill = "dodgerblue3") +
  theme_classic() +
  labs(title = "Final Biomass Fertilizer C")


#3
finalBiomass <- plants$BiomassT2

ggplot(data = plants) + 
  geom_tile(aes(x = Species, y = Fert, fill = finalBiomass)) +
  theme_classic() + 
  labs(title = "Final Biomass")

#4

ggplot(data = plants) + 
  geom_point(aes(x = BiomassT1, y = BiomassT2), color = "dodgerblue4") +
  theme_classic() + 
  labs(title = "BiomassT1 vs. BiomassT2")

#5

ggplot(data = plants) + 
  geom_point(aes(x = BiomassT1, y = BiomassT2, shape = Fert, color = Species)) +
  theme_classic() + 
  labs(title = "Biomass with Species and Fertilizer")


#6
ggplot(data = plants) + 
  geom_col(mapping = aes(x = Row, y = BiomassT2), fill = "darkgreen") +
  theme_classic() + 
  labs(title = "Final Biomass of Species by Row")



#7

ggplot(data = plants) + 
  geom_tile(aes(x = Row, y = BiomassT2, fill = Species)) +
  theme_classic() + 
  labs(title = "Final Biomass by Row")


#8
modBiomass <- plants |>
  subset(Species != 1)

modfertBiomassC <- plants |>
  subset(Species !=1) |>
  subset(Fert == "C")

ggplot(modBiomass, aes(x=BiomassT2)) +
  geom_boxplot(fill = "dodgerblue4") +
  theme_classic() + 
  labs(title = "Final Biomass of Species 1")


ggplot(modfertBiomassC, aes(x=BiomassT2)) +
  geom_boxplot(fill = "dodgerblue4") +
  theme_classic() + labs(title = "Final Biomass Fertilizer C")












