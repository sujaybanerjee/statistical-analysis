

#lets talk about ANOVA (Analysis of Variance)

#How can we keep this people on hold the longest
#airline
#n = 15 people 

#Hold data
holdData <- data.frame(holdTime = c(5,11,1,2,8,0,1,4,6,3,13,9,8,15,7),
                       type = c(rep("Ad", 5),
                                rep("Muzak", 5),
                                rep("Classical", 5)))



# Prepare: mu_{ad} = mu_{muzak} = mu_{classical}

#alpha = .05

# Check:
# 1) Data are collected with randomization, and observations are independent
# 2) Observations within each group are "normally" distributed
# 3) Thesd within each group is roughly the same for all groups


#Calculate:
#lets do ANOVA:

anova1 <- aov(holdTime ~ type, data = holdData)

summary(anova1)
#Pr(>F) is just p-value

#with .0126 p-value, very unlikely that all three means are equal to each other

#Lets compare each of our means directly
TukeyHSD(anova1)


#lets explore our Lacrosse.txt data set

lax <- read.table("/Users/suj/Downloads/Lacrosse.txt")

#add our own column names
colnames(lax) <- c("Model", "Side", "Severity")

#Which lacrosse helmet should I buy?

#R thinks our categorical variable is numeric
library(dplyr)


#Make a copy of my data

lax_data <- lax

lax_data$Model <- recode(lax$Model,
                    '1' = "SHC",
                    '2' = "SHCAF",
                    '3' = "SCHUL",
                    '4' = "BUL")

table(lax_data$Model)


lax_data$Side <- recode(lax$Side,
                         '1' = "Front",
                         '2' = "Back")

table(lax_data$Side)

#Let's ignore Side for now. Lets just look at Model


#lets make a boxplot
library(ggplot2)

ggplot(data = lax_data) + geom_boxplot(aes(x = Model, y = Severity))

#Lets test the claim that the MEAN severity is the same for all helmet models


#type 1 error is claim is wrong - there is actually one that is better, but there isnt a difference
#type 2 error is that you are saying they are all the same, but there is actually one that is protecting you more

#Prepare: ANOVA time! alpha = .1

#Check: 
#Assumptions: We can assume the player put in the same amount of force into each hit - independence
# Point of impact same in both front and back for each hit
# Helmets assigned/ordered randomly
# One impact per helmet

# Within each group (Model), we need roughly Normally Distributed 
#Severity values with similar standard deviations

#Lets check the above
ggplot(data = lax_data) + geom_density(aes(x = Severity)) + facet_wrap(~Model)


# WARNING: ASSUMPTIONS ARE VIOLATED
#(In particular, 2 models have Severities that do NOT seem Normally Distributed)

#Calculate:
anova_bad <- aov(Severity ~ Model, data = lax_data)
summary(anova_bad)

#Small p-value means claim is very unlikely, so we think that there are better or worse models

#Which models are different?
TukeyHSD(anova_bad)

#The significant p-values are the ones comparing the helmet to BUL. All the differences are negative, meaning that BUL had a higher Severity.
#So any of the other three are fine, but just not BUL

#Now, lets take into account "Side"


ggplot(data = lax_data) + geom_density(aes(x = Severity)) + facet_grid(Side~Model)


#Lets do a two-way ANOVA
good_ANOVA <- aov(Severity ~ Model + Side, data = lax_data)

summary(good_ANOVA)

#Low p-value indicates that the model and sides do not behave the same

TukeyHSD(good_ANOVA)

#Severity of the impact in the back is higher than the front
#Cumulative for all the helmets


# Lets add an interaction term
#(How do these two variables interact?)


good_ANOVA_final <- aov(Severity ~ Model + Side + Model:Side, data = lax_data)

summary(good_ANOVA_final)


TukeyHSD(good_ANOVA_final)

# p-value is essentially 0, indicating that there is a huge difference between Model and Size


#BUL and SHCUL back are atrocious


ggplot(data = lax_data) + geom_boxplot(aes(x = Model, y= Severity, fill = Side))


