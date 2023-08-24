


#Lets find all the people who got COVID vaccine and died

covidDeath <- subset(VAERS,
                     DIED == "Y" & VAX_TYPE == "COVID19")

#Lets choose one random person's symptom text

covidDeath$SYMPTOM_TEXT[847]
covidDeath$SYMPTOM_TEXT[647]
covidDeath$SYMPTOM_TEXT[1]
