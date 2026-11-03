d <- read.csv('home.csv')
m1 <- lm(d$baseline_barthel ~ d$followup_tug)
summary(m1)

m2 <- lm(d$baseline_barthel ~ d$fall_followup)
summary(m2)
