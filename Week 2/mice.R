install.packages('ggplot2movies')
library(ggplot2movies)
library(splines)
library(mice)

#Load the data
movies = as.data.frame(ggplot2movies::movies)
head(movies)

#Select relevant variables (important because the dataset contains administrative variables (see ?movies))
movies = movies[,c("year", "length" ,     "budget"  ,    "rating" ,     "votes" ,        "Action"   ,  
                   "Animation",   "Comedy" ,     "Drama"    ,   "Documentary" ,"Romance"   , "Short"      )]

#Remove movies with very few votes
movies = movies[movies$votes > 1000,]
movies$budget = movies$budget / 1000000
#Impute the data
imp = mice(movies)

#Simple linear regression model
result = list()
for(k in 1:5)
{
  tmp = complete(imp, k)
  result[[k]] = lm(rating~budget,data=tmp)
}
summary(pool(result))



#With spline function
spline = ns(movies$budget, df=6)
head(spline)
knots = attr(spline, "knots")
bk = attr(spline, "Boundary.knots")

results = list()
for(k in 1:5)
{
  tmp = complete(imp, k) 
  X = ns(tmp$budget, knots=knots, Boundary.knots = bk)
  results[[k]] = lm(tmp$rating~X)
}

est = pool(results)
summary(est)

#For a vcov object, we need to manually calculate the covariance matrix with Rubins' rules
#See pagina 3 from https://pmc.ncbi.nlm.nih.gov/articles/PMC2727536/ for the equations

#(1) Extract covariance matrices from each imputed model
U = lapply(results, vcov)
U
#(2) Extract MLEs
Q = lapply(results, coef)

#(3) Within-imputation covariance matrix+
m = length(result)
Ubar = Reduce("+", U) / m

#(4) Between-imputation covariance matrix
Qmat = do.call(cbind, Q)
B = cov(t(Qmat))

#(5) Total pooled covariance matrix (Rubin's rules)
VCOV = Ubar + (1 + 1/m) * B

#Finally, we have the covariance matrix
#Sanity check: do we get the same results as pooled? Yes!
cbind(sqrt(diag(VCOV)) , summary(est)$std.error)

 
#Make a plot with newdata (budgets bb)

bb = seq(0,200, length=1000)
X = cbind(1, ns(bb, knots=knots, Boundary.knots=bk))

Ypred = X %*% summary(est)$estimate
se = sqrt(diag(X %*% VCOV %*% t(X)))

dev.new()
plot(bb, Ypred, type="l", ylim=c(5,8), xlab="Budget", ylab="E[Rating]")
lines(bb, Ypred + 1.96 * se)
lines(bb, Ypred - 1.96 * se)

#Conclusion: Adding more budget to your movie is not a guarantee for a better rating. Note that budget is not corrected for inflation, 
#That is a substantial problem in this analysis



