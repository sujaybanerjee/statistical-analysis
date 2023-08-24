
library(tidyverse)
library(ggpubr)
library(rstatix)

friends <- read.csv("/Users/suj/Downloads/Copy of Social Satisfaction Survey (Responses) - Form Responses 1.csv")


#Just j vs s
t.test(friends$Sophomore, friends$Senior, paired = TRUE)
t.test(friends$Junior, friends$Senior, paired = TRUE)
t.test(friends$Freshman, friends$Senior, paired = TRUE)
t.test(friends$Freshman, friends$Sophomore, paired = TRUE)
t.test(friends$Freshman, friends$Junior, paired = TRUE)
t.test(friends$Sophomore, friends$Junior, paired = TRUE)


friendGroup <- friends %>%
  pivot_longer(-ID,
               names_to = "Year",
               values_to = "Satisfaction")


#Assumptions

#check for no significant outliers
identify_outliers(Satisfaction, data = friendGroup)
#two outlier


ggplot(friends) + 
  geom_boxplot(aes(x = factor(Year), y = Satisfaction))


shapiro_test(Year, Rating, data = friends)

               
ggqqplot(friends)



