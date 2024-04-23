# exercise 1 (cleaning, joins) and exercise 2 (mutate)

library(tidyverse)
library(here)

# load data
mma_main <- read_csv(here("task1/data/datatask_main.csv"))
mma_treat <- read_csv(here("task1/data/datatask_treat.csv"))

mma_main <- mma_main |>
  janitor::clean_names()

mma_treat <- mma_treat |>
  janitor::clean_names()


###############################################################################
# EXERCISE 1
###############################################################################

# Inspect data ----
skimr::skim(mma_main)
skimr::skim(mma_treat)
# check missingness: n_missing = 0 for all variables
# check duplicates: n_unique = 1305 for both datasets

# lowercase primary key `prov_id`
mma_main_join <- mma_main |>
  mutate(prov_id = tolower(prov_id))

mma_treat_join <- mma_treat |>
  mutate(prov_id = tolower(prov_id))

# Join data ----
mma_data <- mma_treat_join |>
  left_join(mma_main_join)

skimr::skim(mma_data)

###############################################################################
# EXERCISE 2
###############################################################################

# 2. Using the formulas provided in the attached excerpt (i.e. datatask_saidin.pdf), construct a
# Saidin Index score for all hospitals in each year.
# Output: Name the variable you create in this step saidin and save it with your merged
# dataset to a separate file.
# Hint: Just apply the formulas given to the data. There are variations on the Saidin Index
# with further nuances, but you don’t need to worry about any of those complications here.
