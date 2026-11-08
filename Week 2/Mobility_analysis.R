install.packages('mice')
install.packages('ggplot2')
library(mice)
library(ggplot2)
library(dplyr)
library(xtable)
library(splines)
d <- read.csv('home.csv')
head(d)

# Turn variables into categorical variables
cat_var <- c('fall_followup', 'sex', 'living_alone', 'previous_fall', 'walking_aid')
for (var in cat_var)
{
  d[[var]] <- factor(d[[var]])
}

summary(d)
################################################################################
### Baseline table ###

fall <- d[d$previous_fall==1,]
no_fall <- d[d$previous_fall==0,]

# Functions for the baseline table
# Calculate the mean and standard deviation
mean_sd <- function(x)
{
  sprintf('%.1f (%.1f)', mean(x), sd(x))
}

median_iqr <- function(x)
{
  iqr <- quantile(x,probs=c(0.25, 0.75), na.rm=TRUE)
  sprintf('%i (%i-%i)', median(x, na.rm=TRUE), iqr[1], iqr[2])
}

# Count the participants of a subgroup and calculate the percentage of the (sub)dataset
n_pct <- function(x, value)
{
  x <- as.character(x)
  if (is.na(value)){
    n <- sum(is.na(x))
  } else {
    n <- sum(x==value)
  }
  pct <- 100 * n / length(x)
  sprintf('%d (%.1f%%)', n, pct)
}

# Function for adding rows for categorical variables
row_n_pct <- function(df, var)
{
  label<-var #Keep the name of the variable
  if (var == 'Male' | var == 'Female'){
    col <- 'sex'
  }
  else{
    col <- var
    var <- 1 #Look for true values in dataframe
  }
  
  df <- add_row(df, 
                Variable = paste0(label, " n (%)"),
                Overall = n_pct(d[[col]], var),
                `Previous fall` = n_pct(fall[[col]], var),
                `No previous fall` = n_pct(no_fall[[col]], var)
  )
}

# Make the baseline table
table_df <- d |>
  summarise(
    Overall = mean_sd(age),
    `Previous fall` = mean_sd(age[previous_fall==1]),
    `No previous fall` = mean_sd(age[previous_fall==0])
  ) |>
  mutate(Variable = 'Age mean (SD)') |>
  select(Variable, everything())

baseline_var <- c('Male', 'Female', 'living_alone', 'walking_aid')
for (var in baseline_var)
{
  table_df <- row_n_pct(table_df, var)
}

# Add Baseline Barthel index to the summary table
table_df <- add_row(table_df,
                    Variable = 'Baseline Barthel index median (IQR [Q1]-[Q3])',
                    Overall = median_iqr(d$baseline_barthel),
                    `Previous fall` = median_iqr(fall$baseline_barthel),
                    `No previous fall` = median_iqr(no_fall$baseline_barthel)
)

table_df <- add_row(table_df,
                    Variable = 'Missing Barthel index n (%)',
                    Overall = n_pct(d$baseline_barthel, NA),
                    `Previous fall` = n_pct(fall$baseline_barthel, NA),
                    `No previous fall` = n_pct(no_fall$baseline_barthel, NA)
)

# Count the participants of the whole dataset, participants with previous falls and participants without previous falls.
n_overall <- nrow(d)
n_fall <- nrow(fall)
n_no_fall <- nrow(no_fall)

# Add number of participants to the column names
colnames(table_df) <- c('Variable', sprintf('Overall n=%i', n_overall), sprintf('Previous fall n=%i', n_fall), sprintf('No previous fall n=%i', n_no_fall))
xtable(table_df)

################################################################################
# Regression analysis
d_na <- d[is.na(d$baseline_barthel),]    # NA cases
d_cpl <- d[!is.na(d$baseline_barthel),] # Complete cases

# Simple linear regression TUG ~ barthel on complete cases
m1_tug <- lm(followup_tug ~ baseline_barthel, data=d_cpl)
summary(m1_tug)

m2_tug_adj <- lm(followup_tug ~ baseline_barthel + age + sex + previous_fall + living_alone, data=d_cpl)
summary(m2_tug_adj)

# Splines TUG ~ barthel on complete cases
spline_barthel <- ns(d_cpl$baseline_barthel, df = 3)
knots_barthel = attr(spline_barthel, "knots")
bk_barthel = attr(spline_barthel, "Boundary.knots")

m3_tug_spline <- lm(followup_tug ~ spline_barthel, data=d_cpl)
summary(m3_tug_spline)

m4_tug_spline_adj <- lm(followup_tug ~ spline_barthel + age + sex + previous_fall + living_alone, data=d_cpl)
summary(m4_tug_spline_adj)

# Compare the 4 models
# Does a spline improve the unadjusted model?
anova(m1_tug, m3_tug_spline)
# Does a spline improve the adjusted model?
anova(m2_tug_adj, m4_tug_spline_adj)
# Do covariates improve fit in a linear model?
anova(m1_tug, m2_tug_adj)
# Do covariates improve fit in a non-linear model?
anova(m3_tug_spline, m4_tug_spline_adj)

AIC(m1_tug)
AIC(m2_tug_adj)
AIC(m3_tug_spline)
AIC(m4_tug_spline_adj)

################################################################################  
# Impute missing data
imp <- mice(d, m=25, maxit=50)
results = list()
for(k in 1:25)
{
  tmp <- complete(imp, k)
  X = ns(tmp$baseline_barthel, knots=knots_barthel, Boundary.knots=bk_barthel)
  results[[k]] <- lm(tmp$followup_tug ~ X + tmp$age + tmp$sex + tmp$previous_fall, tmp$living_alone)
}

est <- pool(results)
summary(est, conf.int=TRUE)

