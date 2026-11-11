d <- data.frame(
  time = c(1, 2, 3),
  hazard = c(0.10, 0.15, 0.20),
  survival = c(0.9, 0.7, 0.5)
)
d$density = d$hazard * d$survival

install.packages('survival')
library(survival)
d <- survival::lung
head(d)

s = survival::Surv(time = d$time, event = d$status)
s
survfit(s ~ sex, data=d)

# Slide 54
d = data.frame(
  id = 1:8,
  entry = c(0, 0, 0, 2, 2, 3, 0, 4),
  exit = c(2, 4, 5, 5, 6, 7, 8, 9),
  event = c(1, 1, 0, 1, 0, 1, 0, 1)
)
head(d)
s1 = Surv(c(d$entry, d$exit)) 
s2 = Surv(c(d$entry, d$exit, d$event))
plot(survfit(s1~1))

# Slide 106
d <-  survival::aml
head(d)
cox <- coxph(Surv(time, status)~x, data=d)
cox
summary(cox)
plot(survfit(cox))

d <- survival::heart
head(d)
cox <- coxph(Surv(start, stop, event)~transplant, data=d)
plot(survfit(cox))
summary(cox)
cox

d <- survival::flchain
head(d)
cox <- coxph(Surv(futime, death)~., data=d) # ~. means function of everything else
cox.zph(cox)
cox2 <- coxph(Surv(futime, death)~age + sex + mgus, data=d) 
cox.zph(cox2)
plot(survfit(cox2))

# Non proportional hazards
fit <- coxph(Surv(tstart, time, status) ~ trt + prior + age + karno + karno:tgroup, data=vet3)
summary(fit)

d = d[d$futime>0,]
model = coxph(Surv(futime, death)~age, data=d)
plot(cox.zph(model))
data=survSplit(Surv(futime,death)~., data=d, cut=seq(10, max(d$futime), 10), episode='tint')
dim(data)
coxph(Surv(futime,death)~age + age:tint, data=data)

?survival::flchain

# Wednesday
library(splines)
d <- survival::flchain
d$futime_years <- d$futime / 365.25
d <- d[d$futime_years > 0,]
# Split follow-up into annual intervals
cuts <- seq(1, max(d$futime_years, na.rm = TRUE), by = 1)

pp <- survival::survSplit(
  Surv(futime_years, death) ~ age + sex, 
  data = d, cut = cuts, episode = "interval", id = "id"
)
# Actual time at risk in each interval, measured in years
pp$exposure <- pp$futime_years - pp$tstart
head(pp)

model = glm(death~ns(interval, df=4), data=pp, family=binomial)
a = predict(model)
plot(pp$interval,exp(a)/(1-exp(a)))

glm(death~1+offset(log(exposure)), data=pp, family=poisson)

library(foreign)
list.files()
d = read.spss("DatensatzmitNacherhebung_rev.sav", to.data.frame=TRUE)
head(d)
d$Geburtsdatum <- as.Date(d$Geburtsdatum/86400, origin='1582-10-14')
d$Todesdatum <- as.Date(d$Todesdatum/86400, origin='1582-10-14')
d$Geburtsdatum <- as.Date(d$Geburtsdatum/86400, origin='1582-10-14')

d$final = d$Todesdatum_Jahr
d$final[is.na(d$Uberlebensjahre)] = d$Uberlebensjahre_lebt[is.na(d$Uberlebensjahre)]
d$dead <- !is.na(d$Todesdatum_Jahr)

# Split observation into 2 observations depending on if someone became famous  
dnew = c()
i=1
for(i in 1:nrow(d)){
  tmp = d[i,]
  tmp$L = 0
  tmp$R = tmp$final - tmp$Geburtsjahr
  tmp$D = tmp$dead
  dnew <- rbind(dnew, tmp)
  
  if(is.na(d[i,]Fameonset))
  {
    # Not famous
    tmp$L = 0
    tmp$R = tmp$final - tmp$Geburtsjahr
    tmp$D = tmp$dead
    tmp$famous <- 0
    dnew <- rbind(dnew, tmp)
  } else 
  {
    # Famous
    tmp$L = 0
    tmp$R = tmp$Fameonset - tmp$Geburtsjahr
    tmp$famous = 0
    tmp$d = tmp$dead
    dnew = rbind(dnew, tmp)
    
    tmp$L = tmp$Fameonset - tmp$Geburtsjahr
    tmp$R = tmp$final - tmp$Fameonset
    tmp$famous = 1
    tmp$d = tmp$dead
    dnew = rbind(dnew, tmp)
  }
}

model = coxph(Surv(L, R, D) ~ famous, data=dnew)
coxzph(model)