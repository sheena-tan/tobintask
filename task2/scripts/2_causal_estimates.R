# exercise 2 (causal estimates)

library(tidyverse)
library(here)
library(MatchIt)
library(marginaleffects)

# load data
healthcare_data <- read_csv(file = here("task2/data/healthcare_data.csv"))

healthcare_data <- healthcare_data |>
  mutate_if(is.character, as.factor)

###############################################################################
# EXERCISE 2
###############################################################################

# propensity score matching
psm_quick <-
  matchit(
    plan ~ gender + assignment_type + health_status + language,
    data = healthcare_data,
    method = "quick",
    estimand = "ATE"
  )

plot(summary(psm_quick))

psm_data <- match.data(psm_quick)

psm_fit <- glm(
  healthcare_spend ~ plan * (gender + assignment_type + health_status + language),
  data = psm_data, weights = weights
)

avg_comparisons(
  psm_fit,
  variables = "plan",
  wts = "weights"
  )

###############################################################################
# EXERCISE 3
###############################################################################

# simple average
avg_spend <- aggregate(healthcare_spend ~ plan, data = healthcare_data, FUN = mean)

# regression model with fixed effects
lm_fit <- glm(healthcare_spend ~ plan + gender + health_status + language, data = healthcare_data)

summary(lm_fit)

###############################################################################
# EXERCISE 4
###############################################################################

# modified psm_fit
psm_fit_fixed <- glm(
  healthcare_spend ~ plan + gender + health_status + language,
  data = psm_data, weights = weights
)

avg_comparisons(
  psm_fit_fixed,
  variables = "plan",
  wts = "weights"
)
