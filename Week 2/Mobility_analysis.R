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
# Regression analysis TUG
d_na <- d[is.na(d$baseline_barthel),]    # NA cases
d_cpl <- d[!is.na(d$baseline_barthel),] # Complete cases

summary(d_na)
summary(d_cpl)
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
# Explore missingness of baseline_barthel
sum(is.na(d$baseline_barthel))

summary(d_na)
summary(d_cpl)

d$barthel_na <- as.integer(is.na(d$baseline_barthel))
miss_model <- glm(barthel_na ~ age + sex + living_alone + previous_fall + walking_aid, data=d, family=binomial)
summary(miss_model)
miss_odds <- exp(cbind(OR=coef(miss_model), confint(miss_model)))

# Impute missing data
imp <- mice(d, m=25, maxit=50)
results = list()
for(k in 1:25)
{
  tmp <- complete(imp, k)
  X = ns(tmp$baseline_barthel, knots=knots_barthel, Boundary.knots=bk_barthel)
  results[[k]] <- lm(tmp$followup_tug ~ X + tmp$age + tmp$sex + tmp$previous_fall + tmp$living_alone)
}

est <- pool(results)
summary(est, conf.int=TRUE)

# 1. Fit the corrected adjusted model in every imputed dataset.
# Include living_alone in the formula, not as the second lm() argument.
models <- vector("list", imp$m)

for (i in seq_len(imp$m)) {
  completed_data <- complete(imp, i)
  
  models[[i]] <- lm(
    followup_tug ~
      splines::ns(
        baseline_barthel,
        knots = knots_barthel,
        Boundary.knots = bk_barthel
      ) + age + sex + previous_fall + living_alone,
    data = completed_data
  )
}

# These calculations assume non-aliased coefficients and equal model sizes.
if (any(vapply(models, function(m) anyNA(coef(m)), logical(1)))) {
  stop("At least one model has an aliased coefficient; resolve this before pooling.")
}

sample_sizes <- vapply(models, nobs, numeric(1))
parameter_counts <- vapply(models, function(m) length(coef(m)), integer(1))

if (length(unique(sample_sizes)) != 1L ||
    length(unique(parameter_counts)) != 1L) {
  stop("Models have different sample sizes or parameter counts; check the fits.")
}

n <- sample_sizes[1]
k <- parameter_counts[1]

# 2. Choose a fixed covariate profile.
# Defaults: median OBSERVED age and most common OBSERVED categories.
# Change these to a clinically meaningful profile if preferred.
most_common <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) stop("Cannot choose a category: all values are missing.")
  values <- unique(x)
  values[which.max(tabulate(match(x, values)))]
}

profile <- data.frame(
  age = median(d$age, na.rm = TRUE),
  sex = most_common(d$sex),
  previous_fall = most_common(d$previous_fall),
  living_alone = most_common(d$living_alone)
)

cat("Covariate profile used for all predictions:\n")
print(profile)

make_newdata <- function(scores) {
  nd <- profile[rep(1L, length(scores)), , drop = FALSE]
  nd$baseline_barthel <- scores
  rownames(nd) <- NULL
  nd
}

# Reproduce the design matrix used by predict.lm().
# This preserves the fitted spline specification and factor coding.
new_design <- function(model, newdata) {
  terms_without_response <- delete.response(terms(model))
  mf <- model.frame(
    terms_without_response,
    data = newdata,
    na.action = na.pass,
    xlev = model$xlevels
  )
  X <- model.matrix(
    terms_without_response,
    data = mf,
    contrasts.arg = model$contrasts
  )
  X[, names(coef(model)), drop = FALSE]
}

# Pool one scalar estimate and its sampling variance across imputations.
# n and k provide the complete-data degrees of freedom (n - k).
pool_one <- function(Q, U) {
  pooled <- mice::pool.scalar(Q = Q, U = U, n = n, k = k)
  se <- sqrt(pooled$t)
  critical <- qt(0.975, df = pooled$df)
  
  data.frame(
    estimate = pooled$qbar,
    std.error = se,
    df = pooled$df,
    conf.low = pooled$qbar - critical * se,
    conf.high = pooled$qbar + critical * se
  )
}

# 3. Predict at a grid of Barthel values within the observed range.
observed_scores <- d$baseline_barthel[!is.na(d$baseline_barthel)]
bar_grid <- seq(min(observed_scores), max(observed_scores), length.out = 100)
curve_data <- make_newdata(bar_grid)

# In each imputation: predicted means and their within-imputation variances.
parts <- lapply(models, function(model) {
  X <- new_design(model, curve_data)
  V <- vcov(model)
  
  list(
    mean = as.numeric(X %*% coef(model)),
    variance = pmax(0, rowSums((X %*% V) * X))
  )
})

# Pool separately at each point on the curve.
curve_results <- lapply(seq_along(bar_grid), function(j) {
  Q <- vapply(parts, function(p) p$mean[j], numeric(1))
  U <- vapply(parts, function(p) p$variance[j], numeric(1))
  cbind(baseline_barthel = bar_grid[j], pool_one(Q, U))
})

pooled_curve <- do.call(rbind, curve_results)
rownames(pooled_curve) <- NULL

curve_plot <- ggplot(pooled_curve, aes(x = baseline_barthel, y = estimate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(linewidth = 1) +
  geom_rug(
    data = d,
    aes(x = baseline_barthel),
    inherit.aes = FALSE,
    alpha = 0.15,
    na.rm = TRUE
  ) +
  labs(
    x = "Baseline Barthel Index",
    y = "Predicted mean follow-up TUG (seconds)",
    caption = paste(
      "Pooled across", imp$m, "imputed datasets.",
      "Shading: pointwise 95% confidence intervals.",
      "Covariates held at the printed profile."
    )
  ) +
  theme_minimal()

print(curve_plot)

# 4. Predict at two meaningful Barthel scores.
# Change these if 70 and 90 are not well supported in your data.
scores <- c(70, 90)

if (any(scores < min(observed_scores) | scores > max(observed_scores))) {
  warning("At least one chosen score is outside the observed Barthel range.")
}

point_data <- make_newdata(scores)
point_parts <- lapply(models, function(model) {
  X <- new_design(model, point_data)
  V <- vcov(model)
  
  # Contrast direction: score 2 minus score 1 (here, 90 minus 70).
  contrast <- X[2, ] - X[1, ]
  
  list(
    mean = as.numeric(X %*% coef(model)),
    variance = pmax(0, rowSums((X %*% V) * X)),
    difference = as.numeric(contrast %*% coef(model)),
    difference_variance = max(0, as.numeric(contrast %*% V %*% contrast))
  )
})

pooled_points <- do.call(rbind, lapply(seq_along(scores), function(j) {
  Q <- vapply(point_parts, function(p) p$mean[j], numeric(1))
  U <- vapply(point_parts, function(p) p$variance[j], numeric(1))
  cbind(baseline_barthel = scores[j], pool_one(Q, U))
}))
rownames(pooled_points) <- NULL

pooled_difference <- pool_one(
  Q = vapply(point_parts, function(p) p$difference, numeric(1)),
  U = vapply(point_parts, function(p) p$difference_variance, numeric(1))
)

cat("\nPooled predicted mean TUG at the selected scores:\n")
print(pooled_points)

cat("\nPooled difference: Barthel", scores[2], "minus", scores[1], "\n")
print(pooled_difference)
