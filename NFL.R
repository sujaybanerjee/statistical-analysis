
# Looking at NFL Field Goals

fg <- read.table("/Users/suj/Downloads/FGs.txt")


colnames(fg) <- c("Yards", "Success", "Week")

# Is there a relationship between distance (yards), week of season, and whether a FG is successful?

fg <- glm(Success ~ Yards + factor(Week), data = fg, family = "binomial")

summary(fg)
