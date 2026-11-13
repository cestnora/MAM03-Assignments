# Explanatory comments added by ChatGPT

# Load packages for survival analysis and LaTeX tables.
# xtable is loaded here but is not used in the code below.
library(survival)
library(xtable)

# Optionally set the working directory, then list the files it contains.
#setwd("~/Files/MIK/MAM3/week3/code")
list.files()

# Read the Oscar data from a CSV file.
data = read.csv("oscars.csv")

# Keep the variables needed for the analysis and convert them to numeric.
data = data[,c("Birth", "Final", "time", "death", "FirstNom_Year", "FirstWin_Year", "Noms", "Wins")]
data = as.data.frame(apply(data,2,function(x) as.numeric(as.character(x))))

# Check the number of rows and columns before and after selecting nominees.
# Restrict the analysis to actors with at least one nomination.
dim(data)
data = data[data$Noms>0,]
dim(data)
head(data,20)

# Fit an initial Cox model comparing actors who ever won with those who never won.
# This treats winning as fixed throughout follow-up, including time before a win.
# It can therefore introduce immortal-time bias: future winners must survive
# long enough to receive their first Oscar.
coxph(Surv(time, death)~Wins>0, data=data)

# Calculate age at first win, age at first nomination, and age at the end
# of follow-up (death or censoring).
data$Agefirstwin = data$FirstWin_Year - data$Birth
data$Agefirstnom = data$FirstNom_Year - data$Birth
data$Agemax = data$Final - data$Birth

# Build a start-stop dataset with winning status as a time-varying covariate.
# Actors enter follow-up at their first nomination: this is delayed entry
# on the age time scale.
# Each row represents an interval (start, stop], with an event at stop if event=1.
dataCorrect = c()
for(i in 1:nrow(data))
{
  # Get the relevant ages for actor i.
  Agefirstnom = data$Agefirstnom[i]
  Agefirstwin = data$Agefirstwin[i]
  Agemax = data$Agemax[i]
  
  # Consider the different possible winning histories.
  if(is.na(Agefirstwin))
  {
    # No recorded win: one interval with win=0 throughout follow-up.
    tmp = c(ID=i, start=Agefirstnom, stop=Agemax, event=data$death[i], win=0)
  }else{
    
    if(Agefirstwin==Agefirstnom)
    {
      # First win and first nomination occur at the same recorded age:
      # one interval with win=1 from entry onward.
      tmp = c(ID=i, start=Agefirstnom, stop=Agemax, event=data$death[i], win=1)
    }else{
      # First win occurs after entry: split follow-up into two intervals.
      # Before the win, win=0; reaching the first win is not a death event.
      tmp1 = c(ID=i, start=Agefirstnom, stop=Agefirstwin, event=0, win=0)
      
      # After the win, win=1; death or censoring occurs at the final age.
      tmp2 = c(ID=i, start=Agefirstwin, stop=Agemax, event=data$death[i], win=1)
      tmp = rbind(tmp1,tmp2)
    }
  }
  
  # Append this actor's interval(s) to the new dataset.
  dataCorrect = rbind(dataCorrect,tmp)
}

# rbind above produces a matrix; convert it to a data frame for survival models.
dataCorrect = as.data.frame(dataCorrect)

# Fit a Cox model using age as the time scale and current winning status.
# At each event age, compare winners and non-winners who are then at risk.
# The intervals must have valid ages and start < stop.
coxph(Surv(start, stop, event)~win, data=dataCorrect)

# Estimate product-limit survival curves using the start-stop records,
# stratified by the winning status recorded for each interval.
# Actors may contribute to both strata at different ages, so these are not
# ordinary survival curves for two fixed groups of actors.
km = survfit(Surv(start, stop, event)~win, data=dataCorrect)

# Collect the curve estimates, confidence limits, and numbers at risk.
# The first stratum is win=0; the second is win=1.
plots = data.frame(
  group1 = 1:length(km$time) %in% 1:km$strata[1],
  time = km$time,
  surv = km$surv,
  lower = km$lower,
  upper = km$upper,
  atrisk = km$n.risk)

# Remove rows with missing values before plotting.
plots = na.exclude(plots)

# Open a PDF graphics device: subsequent plotting commands write to this file.
pdf("Km_oscar.pdf", width=8,height=5)

# Define semi-transparent red and blue colours for the confidence bands.
col1 = rgb(255, 0, 0, max = 255, alpha = 125)
col2 = rgb(0, 0, 255, max = 255, alpha = 125)

# Set up the plotting area without drawing points or lines.
# Display survival probabilities as percentages on the vertical axis.
plot(plots$time[plots$group1], plots$surv[plots$group1], xlab="Age (years)",
     ylab="Survival probability (%)", yaxt="n", main="", type="n")
axis(2, axTicks(2), axTicks(2)*100, las=2)

# Create step functions from the recorded numbers at risk for each stratum.
# Despite their names, ff1 refers to win=0 and ff0 refers to win=1.
ff1 = stepfun(plots$time[plots$group1], c(plots$atrisk[plots$group1], NA))
ff0 = stepfun(plots$time[!plots$group1], c(plots$atrisk[!plots$group1], NA))

# Add the step-function values at the age-axis ticks above the plot.
axis(side=3, at=axTicks(1), ff1(axTicks(1)), col=col2, col.axis=col2, tick=FALSE)
axis(side=3, at=axTicks(1), ff0(axTicks(1)), col=col1, col.axis=col1, line=2, tick=FALSE)

# Draw shaded confidence bands: blue for non-winners, red for winners.
# The polygon follows the lower limits forward and the upper limits backward.
polygon(c(plots$time[plots$group1], rev(plots$time[plots$group1])),
        c(plots$lower[plots$group1], rev(plots$upper[plots$group1])), col=col2)

polygon(c(plots$time[!plots$group1], rev(plots$time[!plots$group1])),
        c(plots$lower[!plots$group1], rev(plots$upper[!plots$group1])), col=col1)

# Add the legend and close the PDF device to finish saving the plot.
legend("bottomleft", c("Win", "Without win"), col=c(col1,col2), pch=16)
dev.off()

# Refit the time-varying Cox model and assess the proportional-hazards assumption.
# cox.zph uses scaled Schoenfeld residuals to assess whether the coefficient
# for winning status is constant over age; a systematic trend suggests otherwise.
model = coxph(Surv(start, stop, event)~win, data=dataCorrect)
plot(cox.zph(model))

# Open a second PDF to illustrate individual follow-up histories.
pdf("oscar5.pdf", width=7,height=5)

# Set up an empty plot: age on the horizontal axis, actor on the vertical axis.
# Note: the loop below selects the first 50 eligible actors, not a random sample,
# despite the wording of the plot title.
plot(c(0,155),c(0,50), type="n", xlab="Age(years)", ylab="Actor",
     main="Follow-up of 50 random nominated actors")

# Assign each selected actor a separate horizontal line.
j = 1
for(i in which(!is.na(data$Agefirstnom))[1:50])
{
  # Grey: time before first nomination, outside the analysed follow-up period.
  lines(c(0, data$Agefirstnom[i]), c(j,j), col="grey70", lwd=5)
  
  # Initially colour follow-up by whether the actor ever won:
  # red (2) for never-winners and green (3) for ever-winners.
  lines(c(data$Agefirstnom[i], data$Agemax[i]), c(j,j), lwd=5,
        col=as.numeric(data$Wins[i]>0)+2)
  
  # For actors who won after their first nomination, redraw the pre-win
  # interval in red, leaving only the post-win interval green.
  if(data$Agefirstnom[i] < data$Agefirstwin[i] &
     !is.na(data$Agefirstnom[i] < data$Agefirstwin[i]))
    lines(c(data$Agefirstnom[i], data$Agefirstwin[i]), c(j,j), col=2, lwd=5)
  
  j=j+1
}

# Explain the three follow-up states and save the PDF.
legend("bottomright", c("Without nomination", "Nominated without win", "Oscar winner"),
       bty="n", lwd=5, col=c("grey70",2,3), lty=1)
dev.off()
