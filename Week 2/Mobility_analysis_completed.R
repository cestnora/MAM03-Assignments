# MAM03 Week 2: Functional independence, mobility and falls
# Run from the folder containing home.csv.

pkgs <- c("mice", "ggplot2", "dplyr")
missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs)) install.packages(missing_pkgs)

library(mice)
library(ggplot2)
library(dplyr)
set.seed(2026)

# 1. Import and prepare data 
d <- read.csv("home.csv")
d <- d %>% mutate(
  sex = factor(sex, levels = c("Female", "Male")),
  living_alone = factor(living_alone, levels = c(0, 1), labels = c("No", "Yes")),
  previous_fall = factor(previous_fall, levels = c(0, 1), labels = c("No", "Yes")),
  walking_aid = factor(walking_aid, levels = c(0, 1), labels = c("No", "Yes")),
  fall_followup = as.integer(fall_followup),
  barthel_missing = as.integer(is.na(baseline_barthel))
)
stopifnot(nrow(d) == dplyr::n_distinct(d$participant_id),
          all(na.omit(d$fall_followup) %in% 0:1),
          all(na.omit(d$baseline_barthel) %in% seq(0, 100, 5)))

# 2. Baseline table (saved for the report) ----------------------------------
fmt_mean <- function(x) sprintf("%.1f (%.1f)", mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))
fmt_median <- function(x) {
  q <- quantile(x, c(.25, .75), na.rm = TRUE)
  sprintf("%.0f (%.0f--%.0f)", median(x, na.rm = TRUE), q[1], q[2])
}
fmt_level <- function(x, level) sprintf("%d (%.1f%%)", sum(x == level, na.rm = TRUE),
                                 100 * mean(x == level, na.rm = TRUE))
fmt_missing <- function(x) sprintf("%d (%.1f%%)", sum(is.na(x)), 100 * mean(is.na(x)))
groups <- list(Overall = d, `Previous fall` = filter(d, previous_fall == "Yes"),
               `No previous fall` = filter(d, previous_fall == "No"))
baseline_table <- data.frame(
  Variable = c("Age, mean (SD)", "Female, n (%)", "Living alone, n (%)",
               "Walking aid, n (%)", "Barthel, median (IQR)", "Barthel missing, n (%)"),
  Overall = c(fmt_mean(groups[[1]]$age), fmt_level(groups[[1]]$sex, "Female"),
              fmt_level(groups[[1]]$living_alone, "Yes"), fmt_level(groups[[1]]$walking_aid, "Yes"),
              fmt_median(groups[[1]]$baseline_barthel), fmt_missing(groups[[1]]$baseline_barthel)),
  `Previous fall` = c(fmt_mean(groups[[2]]$age), fmt_level(groups[[2]]$sex, "Female"),
                      fmt_level(groups[[2]]$living_alone, "Yes"), fmt_level(groups[[2]]$walking_aid, "Yes"),
                      fmt_median(groups[[2]]$baseline_barthel), fmt_missing(groups[[2]]$baseline_barthel)),
  `No previous fall` = c(fmt_mean(groups[[3]]$age), fmt_level(groups[[3]]$sex, "Female"),
                         fmt_level(groups[[3]]$living_alone, "Yes"), fmt_level(groups[[3]]$walking_aid, "Yes"),
                         fmt_median(groups[[3]]$baseline_barthel), fmt_missing(groups[[3]]$baseline_barthel))
)
names(baseline_table)[2:4] <- c(paste0("Overall (n=", nrow(groups[[1]]), ")"),
                                paste0("Previous fall (n=", nrow(groups[[2]]), ")"),
                                paste0("No previous fall (n=", nrow(groups[[3]]), ")"))
print(baseline_table, row.names = FALSE)
write.csv(baseline_table, "baseline_table_for_report.csv", row.names = FALSE)

# 3. Complete-case regression and model selection ---------------------------
cc <- filter(d, !is.na(baseline_barthel)) %>% mutate(barthel10 = baseline_barthel / 10)
# The spline's internal knots are selected automatically by ns() from the
# observed Barthel distribution, then held fixed across all imputed datasets.
spline_template <- splines::ns(cc$baseline_barthel, df = 3)
barthel_knots <- attr(spline_template, "knots")
barthel_boundary <- attr(spline_template, "Boundary.knots")
cat("Complete cases:", nrow(cc), "of", nrow(d), "\n")
cat("Automatically selected spline knots:", barthel_knots,
    "(boundaries:", barthel_boundary, ")\n")

# Primary adjustment set: age, sex, living alone and previous falls.
# Walking-aid use is reserved for imputation/sensitivity analyses because it
# overlaps conceptually with the functional-mobility construct measured by Barthel.
tug_unadj_linear <- lm(followup_tug ~ baseline_barthel, data = cc)
tug_unadj_spline <- lm(followup_tug ~ splines::ns(baseline_barthel, knots = barthel_knots,
                                                   Boundary.knots = barthel_boundary), data = cc)
tug_adj_linear <- lm(followup_tug ~ baseline_barthel + age + sex + living_alone + previous_fall, data = cc)
tug_adj_spline <- lm(followup_tug ~ splines::ns(baseline_barthel, knots = barthel_knots,
                                                 Boundary.knots = barthel_boundary) +
                       age + sex + living_alone + previous_fall, data = cc)

fall_unadj_linear <- glm(fall_followup ~ barthel10, family = binomial, data = cc)
fall_unadj_spline <- glm(fall_followup ~ splines::ns(baseline_barthel, knots = barthel_knots,
                                                      Boundary.knots = barthel_boundary),
                         family = binomial, data = cc)
fall_adj_linear <- glm(fall_followup ~ barthel10 + age + sex + living_alone + previous_fall,
                       family = binomial, data = cc)
fall_adj_spline <- glm(fall_followup ~ splines::ns(baseline_barthel, knots = barthel_knots,
                                                    Boundary.knots = barthel_boundary) +
                         age + sex + living_alone + previous_fall,
                       family = binomial, data = cc)

# TUG: linear versus spline (unadjusted, then adjusted)
print(anova(tug_unadj_linear, tug_unadj_spline))
print(anova(tug_adj_linear, tug_adj_spline))
# TUG: covariate adjustment tests
print(anova(tug_unadj_linear, tug_adj_linear))
print(anova(tug_unadj_spline, tug_adj_spline))
# Falls: linear versus spline (unadjusted, then adjusted)
print(anova(fall_unadj_linear, fall_unadj_spline, test = "LRT"))
fall_shape_test <- anova(fall_adj_linear, fall_adj_spline, test = "LRT")
print(fall_shape_test)
# Falls: covariate adjustment tests
print(anova(fall_unadj_linear, fall_adj_linear, test = "LRT"))
print(anova(fall_unadj_spline, fall_adj_spline, test = "LRT"))

model_fit <- data.frame(
  Model = c("TUG linear adjusted", "TUG spline adjusted", "Falls linear adjusted", "Falls spline adjusted"),
  AIC = c(AIC(tug_adj_linear), AIC(tug_adj_spline), AIC(fall_adj_linear), AIC(fall_adj_spline)),
  BIC = c(BIC(tug_adj_linear), BIC(tug_adj_spline), BIC(fall_adj_linear), BIC(fall_adj_spline))
)
print(model_fit, row.names = FALSE)
# Selected complete-case models
print(summary(tug_adj_spline))
print(summary(fall_adj_linear))
cat("Fall logistic model converged:", fall_adj_linear$converged, "\n")

# Odds ratios per 10-point higher Barthel score.
or10 <- function(model, term = "barthel10") {
  z <- coef(summary(model))[term, ]
  c(OR = exp(z[1]), lower = exp(z[1] - 1.96 * z[2]),
    upper = exp(z[1] + 1.96 * z[2]), p = z[4])
}
cat("Unadjusted fall OR per 10 points:\n"); print(or10(fall_unadj_linear))
cat("Adjusted fall OR per 10 points:\n"); print(or10(fall_adj_linear))

# Optional sensitivity analyses: add walking-aid use to the primary models.
cat("\nSensitivity analyses adding walking-aid use\n")
print(summary(update(tug_adj_spline, . ~ . + walking_aid)))
print(summary(update(fall_adj_linear, . ~ . + walking_aid)))

# Diagnostics: examine plots in the R plotting pane
oldpar <- par(mfrow = c(2, 2)); plot(tug_adj_spline); par(oldpar)
oldpar <- par(mfrow = c(1, 2))
plot(fitted(fall_adj_linear), residuals(fall_adj_linear, type = "deviance"),
     xlab = "Fitted fall probability", ylab = "Deviance residual", main = "Falls: residuals")
abline(h = 0, lty = 2)
plot(cooks.distance(fall_adj_linear), ylab = "Cook's distance", xlab = "Observation",
     main = "Falls: influence")
par(oldpar)

# Largest fall-model Cook's distances
print(sort(cooks.distance(fall_adj_linear), decreasing = TRUE)[1:5])

# 4. Missingness exploration ------------------------------------------------
d$barthel_missing <- as.integer(is.na(d$baseline_barthel))
# Barthel missingness by observed characteristics
print(d %>% group_by(barthel_missing) %>% summarise(
  n = n(), mean_age = mean(age), mean_TUG = mean(followup_tug),
  fall_followup_pct = 100 * mean(fall_followup),
  previous_fall_pct = 100 * mean(previous_fall == "Yes"),
  walking_aid_pct = 100 * mean(walking_aid == "Yes"), .groups = "drop"))
missing_model <- glm(barthel_missing ~ age + sex + living_alone + previous_fall +
                       walking_aid + followup_tug + fall_followup,
                     family = binomial, data = d)
# Associations with Barthel missingness (not causal effects)
print(summary(missing_model))

# 5. Multiple imputation ----------------------------------------------------
# Only Barthel is incomplete. PMM preserves its observed range and 5-point scale.
mi_data <- d %>% select(-participant_id, -barthel_missing)
method <- mice::make.method(mi_data); method[] <- ""; method["baseline_barthel"] <- "pmm"
pred <- mice::make.predictorMatrix(mi_data); pred[,] <- 0
pred["baseline_barthel", setdiff(names(mi_data), "baseline_barthel")] <- 1
imp <- mice(mi_data, m = 30, maxit = 20, method = method,
            predictorMatrix = pred, seed = 2026, printFlag = FALSE)
print(imp$loggedEvents)
plot(imp)                                  # trace/convergence plot: display only
densityplot(imp, ~ baseline_barthel)       # observed versus imputed: display only
imp_long <- complete(imp, "long")$baseline_barthel
cat("Imputed Barthel range:", range(imp_long),
    "; all values on 5-point scale:", all(imp_long %% 5 == 0), "\n")
stopifnot(min(imp_long) >= 0, max(imp_long) <= 100, all(imp_long %% 5 == 0))

completed <- lapply(seq_len(imp$m), function(i) {
  z <- complete(imp, i)
  z$barthel10 <- z$baseline_barthel / 10
  z
})
tug_mi <- lapply(completed, function(z) lm(
  followup_tug ~ splines::ns(baseline_barthel, knots = barthel_knots,
                             Boundary.knots = barthel_boundary) +
    age + sex + living_alone + previous_fall, data = z))
fall_mi <- lapply(completed, function(z) glm(
  fall_followup ~ barthel10 + age + sex + living_alone + previous_fall,
  family = binomial, data = z))

# Pooled MI: adjusted TUG spline model
tug_pooled <- pool(as.mira(tug_mi))
print(summary(tug_pooled, conf.int = TRUE))
# Pooled MI: adjusted fall logistic model
fall_pooled <- pool(as.mira(fall_mi))
fall_pool_summary <- summary(fall_pooled, conf.int = TRUE)
print(fall_pool_summary)
fall_barthel <- subset(fall_pool_summary, term == "barthel10")
# Pooled fall OR per 10 points and 95% CI
print(c(OR = exp(fall_barthel$estimate), lower = exp(fall_barthel$`2.5 %`),
        upper = exp(fall_barthel$`97.5 %`), p = fall_barthel$p.value))

# 6. Adjusted predictions and report figure --------------------------------
# Predictions use one explicit reference profile, not population-average risks.
profile <- data.frame(
  age = median(d$age),
  sex = factor("Female", levels = levels(d$sex)),
  living_alone = factor("No", levels = levels(d$living_alone)),
  previous_fall = factor("No", levels = levels(d$previous_fall))
)
newdata <- function(scores) {
  z <- profile[rep(1, length(scores)), , drop = FALSE]
  z$baseline_barthel <- scores
  z$barthel10 <- scores / 10
  z
}
model_matrix <- function(fit, nd) {
  tt <- delete.response(terms(fit))
  mf <- model.frame(tt, nd, xlev = fit$xlevels, na.action = na.pass)
  X <- model.matrix(tt, mf, contrasts.arg = fit$contrasts)
  X[, names(coef(fit)), drop = FALSE]
}
pool_scalar_ci <- function(Q, U, n, k) {
  p <- mice::pool.scalar(Q = Q, U = U, n = n, k = k)
  se <- sqrt(p$t); crit <- qt(.975, p$df)
  c(est = p$qbar, lo = p$qbar - crit * se, hi = p$qbar + crit * se)
}
cc_predict <- function(fit, nd, logistic = FALSE) {
  p <- if (logistic) predict(fit, nd, type = "link", se.fit = TRUE) else
    predict(fit, nd, se.fit = TRUE)
  crit <- if (logistic) qnorm(.975) else qt(.975, df.residual(fit))
  lo <- p$fit - crit * p$se.fit; hi <- p$fit + crit * p$se.fit
  if (logistic) data.frame(est = plogis(p$fit), lo = plogis(lo), hi = plogis(hi))
  else data.frame(est = p$fit, lo = lo, hi = hi)
}
mi_predict <- function(fits, nd, logistic = FALSE) {
  bits <- lapply(fits, function(fit) {
    X <- model_matrix(fit, nd); V <- vcov(fit)
    list(q = as.numeric(X %*% coef(fit)),
         u = pmax(0, rowSums((X %*% V) * X)))
  })
  do.call(rbind, lapply(seq_len(nrow(nd)), function(j) {
    pooled <- pool_scalar_ci(vapply(bits, function(x) x$q[j], numeric(1)),
                             vapply(bits, function(x) x$u[j], numeric(1)),
                             n = nrow(completed[[1]]), k = length(coef(fits[[1]])))
    if (logistic) data.frame(est = as.numeric(plogis(pooled["est"])),
                             lo = as.numeric(plogis(pooled["lo"])),
                             hi = as.numeric(plogis(pooled["hi"])))
    else data.frame(est = unname(pooled["est"]), lo = unname(pooled["lo"]),
                    hi = unname(pooled["hi"]))
  }))
}

scores <- c(70, 90)
grid <- seq(min(cc$baseline_barthel), max(cc$baseline_barthel), length.out = 100)
make_curve <- function(model, fits, outcome, logistic = FALSE) {
  cc <- cc_predict(model, newdata(grid), logistic)
  mi <- mi_predict(fits, newdata(grid), logistic)
  bind_rows(
    data.frame(Barthel = grid, cc, Outcome = outcome, Analysis = "Complete case"),
    data.frame(Barthel = grid, mi, Outcome = outcome, Analysis = "Multiple imputation")
  )
}
curves <- bind_rows(
  make_curve(tug_adj_spline, tug_mi, "TUG time (seconds)"),
  make_curve(fall_adj_linear, fall_mi, "Fall probability", logistic = TRUE)
)
curve_plot <- ggplot(curves, aes(Barthel, est, colour = Analysis, fill = Analysis,
                                  linetype = Analysis)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = .15, colour = NA) +
  geom_line(linewidth = .8) +
  facet_wrap(~Outcome, scales = "free_y", nrow = 1) +
  scale_x_continuous(breaks = seq(50, 100, 10), limits = c(50, 100)) +
  labs(x = "Baseline Barthel Index", y = "Adjusted prediction",
       caption = "Pointwise 95% confidence intervals; covariates held at the reference profile.") +
  theme_minimal() + theme(legend.title = element_blank())
print(curve_plot)
ggsave("Figure1_adjusted_Barthel_curves_revised.png", curve_plot,
       width = 8.5, height = 4.2, dpi = 300)

cc_tug_points <- cc_predict(tug_adj_spline, newdata(scores))
cc_fall_points <- cc_predict(fall_adj_linear, newdata(scores), logistic = TRUE)
mi_tug_points <- mi_predict(tug_mi, newdata(scores))
mi_fall_points <- mi_predict(fall_mi, newdata(scores), logistic = TRUE)
make_row <- function(outcome, analysis, p, unit = "", diff_unit = unit) data.frame(
  Outcome = outcome, Analysis = analysis,
  `Barthel 70` = sprintf("%.3f (%.3f--%.3f)%s", p$est[1], p$lo[1], p$hi[1], unit),
  `Barthel 90` = sprintf("%.3f (%.3f--%.3f)%s", p$est[2], p$lo[2], p$hi[2], unit),
  `Difference (70 minus 90)` = sprintf("%.3f%s", p$est[1] - p$est[2], diff_unit),
  check.names = FALSE
)
prediction_table <- bind_rows(
  make_row("TUG", "Complete case", cc_tug_points, " s"),
  make_row("TUG", "Multiple imputation", mi_tug_points, " s"),
  make_row("Falls", "Complete case", transform(cc_fall_points, est = 100*est, lo = 100*lo, hi = 100*hi), "%", " pp"),
  make_row("Falls", "Multiple imputation", transform(mi_fall_points, est = 100*est, lo = 100*lo, hi = 100*hi), "%", " pp")
)
print(prediction_table, row.names = FALSE)
write.csv(prediction_table, "predictions_for_report.csv", row.names = FALSE)

# Reference profile used for predictions:
print(profile)

# Saved outputs: 
# baseline_table_for_report.csv
# predictions_for_report.csv
# Figure1_adjusted_Barthel_curves.png
