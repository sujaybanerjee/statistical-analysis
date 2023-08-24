
#afterlife and race data
#white - 1132 yes, 247 no
#balck - 203 yes, 44 no
#other - 120 yes, 41 no

#There is no relationship between race and belief in afterlife

# calculate chi-square value of cell 1
#(observed - expected)^2/expected

#make my data
raceAfterlife <- matrix(c(1132, 203, 120, 
                          247, 44, 41),
                        nrow = 3,
                        ncol = 2)

colnames(raceAfterlife) <- c("Yes", "No")
rownames(raceAfterlife) <- c("White", "Black", "Other")


#proportion Yes
n <- sum(raceAfterlife)

colSums(raceAfterlife)/n

#proportion of each race
rowSums(raceAfterlife)/n

#expected yes + white
(0.8142138 * 0.77168439)*n

(1132 - 1122.8)^2/1122.8


#expected no + other
(0.1857862 * 0.09009513)*n

(41 - 29.9)^2/29.9



#do a chi-square test
chiTest1 <- chisq.test(raceAfterlife)

#There is number means that there is a 6%  probability we saw this or something more extreme than just random chance

(chiTest1$observed - chiTest1$expected)^2/chiTest1$expected


#Check standardized residual of contributions
chiTest1$residuals^2
chiTest1$residuals





#chi-square example

data1 <- data.frame(values = c(rep("A", 90),
                               rep("B", 9),
                               rep("C", 1)))

table(data1)

#do chi-square test
chi1 <- chisq.test(table(data1), p = c(.9, .0999, .0001))

chi1$expected


#check chi-square contributions
chi1$residuals^2



