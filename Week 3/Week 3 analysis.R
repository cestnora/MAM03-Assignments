install.packages('moonBook')
install.packages('lubridate')
library(moonBook)
library(ggplot2)
library(survival)
library(lubridate)
library(dplyr)
d <- read.csv('cancer.csv')
head(d)

d$sex <- factor(d$sex)

# Test if age is normally distributed per stage
stage1 <- d[d$stage == 'I',]
stage2 <- d[d$stage == 'II',]
stage3 <- d[d$stage == 'III',]

shapiro.test(stage1$age)
shapiro.test(stage2$age)
shapiro.test(stage3$age)
shapiro.test(d$age)

# Check 
sex_table <- table(d$stage, d$sex)
chisq.test(sex_table)$expected
# Baseline info: age, recorded sex, cancer stage and comorbidity
base_table <- mytable(~stage + age + sex + charlson_index, data=d)
base_table_stage <- mytable(stage~age + sex + charlson_index, data=d, method=2, catMethod=0, show.all=TRUE)
# 
base_table
base_table_stage
densityplot(age~stage, data=d)

ggplot(d, aes(x = charlson_index, fill = stage)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 20, stat='count')


ggplot(d, aes(x = sex, fill = stage)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 20, stat='count') 

# Replace empty cells with NA
d[d == ""] <- NA

# Add a mortality indicator (death) to dataframe
d$death <- ifelse(is.na(d$date_of_death), 0, 1)

# Add a recurrence indicator to dataframe
d$recurrence <- ifelse(is.na(d$recurrence_date), 0, 1)

# Change the datatype of recurrence_date, date_of_death & followup_end_date to dates
date_cols <- c('surgery_date', 'recurrence_date', 'date_of_death', 'followup_end_date')
for (date in date_cols)
{
  d[[date]] <- ymd(d[[date]])
}

# Calculate followup time in days
d$time <- as.numeric(d$followup_end_date - d$surgery_date)

coxph(Surv(time, death))