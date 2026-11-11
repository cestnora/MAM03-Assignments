install.packages('moonBook')
install.packages('Rtools')
library(moonBook)
library(ggplot2)
library(survival)
d <- read.csv('cancer.csv')
head(d)

d$sex <- factor(d$sex)
d$charlson_index <- factor(d$charlson_index)
# Baseline info: age, recorded sex, cancer stage and comorbidity
base_table <- mytable(stage~age + sex + charlson_index, data=d)
densityplot(age~stage, data=d)

ggplot(d, aes(x = age, fill = stage)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 20) +
  theme_minimal()