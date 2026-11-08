install.packages('ggplot2movies')
library(ggplot2movies)

install.packages('mice')
library(mice)


d = datasets::cars
head(d)
plot(d)
est = lm(dist~speed, data=d)
abline(a=coef(est)[1], b=coef(est)[2])
est

d2= datasets::mtcars
head(d)
model = glm(mpg~hp+wt, data=d2, family=gaussian)
summary(model)

d3=datasets::stackloss
head(d3)
new_day = data.frame(Air.Flow = 60, Water.Temp = 20, Acid.Conc. = 85)
model = lm(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc., data=d3)

predict(model, newdata=new_day, interval='confidence')

library(splines)
x = bs(d2$wt, df=6)
plot(d2$wt, x[,1], type='p', pch=16)
plot(d2$wt, x[,2], col=2, pch=16)

d4= MASS::mcycle
head(d4)
x = bs(d4$time, knots=c(quantile(d4$time, c(0.25, 0.75)), median(d4$time)), Boundary.knots=range(d4$time))
par(mfrow=c(2,1))
plot(d4$time, x[,1], type='p', pch=10)
plot(d4$time, x[,2], type='p', pch=16)
plot(d4$time, x[,3], type='p', pch=6)


# Point is to see that mice iterations come up with different imputations
d <- mice::nhanes
head(d)

d$hyp <- factor(d$hyp) #change variable to categorical
imp <- mice(d, maxit=50)
d1 <- complete(imp,1)
d2 <- complete(imp,2)
cov(d1[,c(2,4)])
cov(d2[,c(2,4)])
?cov
cov(d[,c(2,4)])
cov(d[,c(2,4)], use='pairwise.complete.obs')

d[,1] = as.factor(d[,1])
plot(imp)
