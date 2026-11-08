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
m_tug_compl <- lm(followup_tug ~ baseline_barthel, data=d_cpl)
summary(m_tug_compl)

plot(d$baseline_barthel, d$followup_tug)
abline(m_tug_compl, col='red')


# Explore missingness of baseline_barthel
sum(is.na(d$baseline_barthel))

summary(na_df)
summary(obs_df)

d$barthel_na <- as.integer(is.na(d$baseline_barthel))
miss_model <- glm(barthel_na ~ age + sex + living_alone + previous_fall + walking_aid, data=d, family=binomial)
summary(miss_model)
miss_odds <- exp(cbind(OR=coef(miss_model), confint(miss_model)))



imp <- mice(d, m=25, maxit=50)
results <- list()
for(k in 1:25)
{
  tmp <- complete(imp, k)
  results[[k]] <- glm(baseline_barthel~followup_tug, data=tmp)
}

est <- pool(results)
summary(est)
m1 <- lm(d$baseline_barthel ~ d$followup_tug)
summary(m1)

m2 <- lm(d$baseline_barthel ~ d$fall_followup)
summary(m2)
