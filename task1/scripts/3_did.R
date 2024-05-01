# exercise 4-5 (difference in difference model analysis and visualization)

library(tidyverse)
library(here)
library(fixest)
library(ggtext)

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
mma_did <- did_means |>
  left_join(did_estimates_ci) |>
  mutate_all(~replace(., is.na(.), 0))

# write out dataset
save(mma_did, file = here("task1/data/mma_did.rds"))

###############################################################################
# EXERCISE 5
###############################################################################

# 95 CI error bars
mma_test <- mma_did |>
  filter(year >= 2004) |>
  mutate(
    tr_ref = tr_mean - tr_effect,
    tr_ref_lo = tr_mean - tr_hi,
    tr_ref_hi = tr_mean - tr_lo
  ) |>
  full_join(
    mma_did |>
      filter(year < 2004)
  )

# visualization: significant effect
did_plot <- mma_test |>
  ggplot(aes(year)) +
  geom_line(aes(y = cr_mean), color = "#619CFF", alpha = 0.5) +
  geom_point(aes(y = cr_mean), color = "#619CFF") +
  geom_line(aes(y = tr_ref), color = "#00BA38", linetype = "dashed", alpha = 0.7) +
  geom_point(aes(y = tr_ref), color = "#00BA38") +
  geom_errorbar(aes(ymin = tr_ref_lo, ymax = tr_ref_hi), color = "#00BA38", width = 0.1) +
  geom_ribbon(aes(ymin = tr_ref_lo, ymax = tr_ref_hi), fill = "#00BA38", alpha = 0.1) +
  geom_point(aes(y = tr_mean), color = "#F8766D") +
  geom_line(aes(y = tr_mean), color = "#F8766D", alpha = 0.5) +
  geom_vline(xintercept = 2004, linetype = "dotted") +
  scale_x_continuous(n.breaks = 9) +
  scale_y_continuous(n.breaks = 6) +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    axis.title.x = element_blank(),
    plot.title.position = "plot",
    plot.title = element_markdown(),
    plot.subtitle = element_markdown()
  ) +
  labs(
    y = "Saidin Index Scores",
    title = "Technology adoption rates
    <b style='color: #000000;'>significantly increase </b><b style='color: #00BA38;'>before</b>
    and <b style='color: #F8766D;'>after</b> MMA."
  ) +
  annotate("text", x = 2008, y = 3.5, label = "control", color = "#619CFF", size = 3) +
  annotate("text", x = 2005.2, y = 6.4, label = "treatment", color = "#F8766D" , size = 3) +
  annotate("text", x = 2009.1, y = 7.4, label = "counterfactual", color = "#00BA38" , size = 2.7) +
  annotate("text", x = 2004, y = 8.5, label = "Year of\nAdoption", size = 3)

# write out plot
ggsave(here("task1/figures/did_plot.png"), did_plot)
