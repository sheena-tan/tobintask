# exercise 4-5 (difference in difference model analysis and visualization)

library(tidyverse)
library(here)
library(did)
library(fixest)

# load data
load(here("task1/data/mma_data.rds"))


###############################################################################
# EXERCISE 4
###############################################################################
# event study model
did_event <- feols(saidin ~ i(year, treat, ref = 2004) | prov_id + year, data = mma_data)

# table of estimates and CI
did_estimates <- summary(did_event)
did_ci <- confint(did_event)

did_estimates_ci <- did_estimates$coefficients |>
  as.data.frame() |>
  tibble::rownames_to_column("year") |>
  left_join(
    as.data.frame(did_ci) |>
      tibble::rownames_to_column("year") |>
      rename(),
    by = join_by(year)
  ) |>
  janitor::clean_names() |>
  rename(
    tr_effect = did_estimates_coefficients,
    tr_hi = x97_5_percent,
    tr_lo = x2_5_percent
  ) |>
  mutate(
    year = as.numeric(str_extract(year, "\\d{4}"))
  )


# table of means
did_means <- mma_data |>
  group_by(year) |>
  filter(treat == 0) |>
  summarize(mean(saidin)) |>
  rename(cr_mean = `mean(saidin)`) |>
  left_join(
    mma_data |>
      group_by(year) |>
      filter(treat == 1) |>
      summarize(mean(saidin)) |>
      rename(tr_mean = `mean(saidin)`)
  )

# joins tables into new dataset
mma_did <- did_estimates_ci |>
  left_join(did_means)

# write out dataset
save(mma_did, file = here("task1/data/mma_did.rds"))

###############################################################################
# EXERCISE 5
###############################################################################

did_means |>
  ggplot(aes(year, cr_mean)) +
  geom_line(color = "#619CFF") +
  geom_line(aes(year, tr_mean), color = "#F8766D")+
  theme_minimal() +
  labs(
    x = "Year",
    y = "Saidin Index Scores"
  )
