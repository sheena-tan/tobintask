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
# What was the total number of hospitals each year?
mma_data |>
  group_by(year) |>
  mutate(prov_total = n_distinct(prov_id)) |>
  skimr::skim(prov_total)
# N_t each year [2001-2010] constant = 1305

# testing algorithm for tech_1
mma_saidin <- mma_data |>
  group_by(year) |>
  mutate(sum_1 = sum(tech_1)) |> #num hospitals with tech_1
  mutate(weight_1 = 1 - (1/1305) * sum_1) |> #weight a_k,t
  mutate(index_1 = tech_1 * weight_1) #intermediate value before sum

# repeat for all `tech_n`
for (i in 1:31) {
  mma_saidin <- mma_saidin |>
    group_by(year) |>
    mutate(
      !!paste0("sum_", i) := sum(!!sym(paste0("tech_", i))),
      !!paste0("weight_", i) := 1 - (1/1305) * !!sym(paste0("sum_", i)),
      !!paste0("index_", i) := !!sym(paste0("tech_", i)) * !!sym(paste0("weight_", i))
    )
}

# sum across `index_n` to find saidin index for each hospital per year
mma_saidin <- mma_saidin |>
  select(prov_id, year, starts_with("index")) |>
  pivot_longer(cols = starts_with("index"), names_to = "index") |>
  group_by(prov_id, year) |>
  summarise(sum(value)) |>
  rename(saidin = `sum(value)`)

# join to merged data
mma_data <- mma_saidin |>
  left_join(mma_data)

glimpse(mma_data)

# write out data
save(mma_data, file = here("task1/data/mma_data.rds"))
