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

ggplot(d, aes(x = charlson_index, fill = stage)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 20, stat='count')


ggplot(d, aes(x = sex, fill = stage)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 20, stat='count') 
