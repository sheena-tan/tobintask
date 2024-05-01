# exercise 1 (cleaning, randomness)

library(tidyverse)
library(here)
library(tseries)

# load data
healthcare_data <- read_csv(file = here("task2/data/healthcare_data.csv"))

healthcare_data <- healthcare_data |>
  mutate_if(is.character, as.factor)

###############################################################################
# EXERCISE 1
###############################################################################

# Inspect data ----
skimr::skim(healthcare_data)

# confirm random auto-assignment
dummy_auto <- healthcare_data |>
  mutate(plan = ifelse(plan == "Plan_A", 1, 0)) |>
  filter(assignment_type == "Automatically Assigned")

runs.test(as.factor(dummy_auto$plan))

# test for random choice
dummy_choice <- healthcare_data |>
  mutate(plan = ifelse(plan == "Plan_A", 1, 0)) |>
  filter(assignment_type == "Member Choice")

runs.test(as.factor(dummy_choice$plan))
