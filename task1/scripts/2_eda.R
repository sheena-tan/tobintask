# exercise 3 (data visualization)

library(tidyverse)
library(here)
library(ggridges)
library(ggtext)
library(viridis)

# load data
load(here("task1/data/mma_data.rds"))

mma_data <- mma_data |>
  mutate(year = as.factor(year)) |>
  mutate(year = fct_rev(year))

###############################################################################
# EXERCISE 3a
###############################################################################

# Summary statistics ----
mma_2004 <- mma_data |>
  filter(year == 2004)

describe(mma_2004$saidin, IQR = TRUE, skew = FALSE)

# Visualize distribution ----
p1 <- ggplot(mma_2004, aes(saidin)) +
  geom_boxplot() +
  xlim(0, 12) +
  theme_void()

p2 <- ggplot(mma_2004, aes(saidin)) +
  geom_density() +
  xlim(0, 12) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(x = "Saidin Index Scores")

saidin_2004 <- p1/p2 + plot_layout(heights = unit(c(1, 5), c("cm", "null")))

# Distribution of Saidin Index scores by year ----
# density ridges plot
saidin_density_ridge <- mma_data |>
  ggplot(aes(x = saidin, y = year)) +
  geom_density_ridges(aes(fill = year, color = year), scale = 3) +
  scale_x_continuous(limits = c(0, 25)) +
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE) +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = 'none',
    plot.title.position = "plot",
    plot.caption.position =  "plot",
    plot.title = element_markdown(),
    plot.subtitle = element_markdown()
  ) +
  labs(
    x = "Saidin Index Score",
    title = "Over the years, medical technologies have become <b>increasingly available</b>.",
    subtitle = "The first growth seems to have occured from <b style='color: #B4DE2CFF;'>2002</b> to <b style='color: #35B779FF;'>2003</b>, increasing steadily afterwards."
  )

# stacked boxplots
saidin_boxplot <- mma_data |>
  ggplot(aes(x = saidin, y = year)) +
  geom_boxplot(aes(fill = year, color = year)) +
  scale_x_continuous(limits = c(0, 25)) +
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE) +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = 'none',
    plot.title.position = "plot",
    plot.caption.position =  "plot",
    plot.title = element_markdown(),
    plot.subtitle = element_markdown(lineheight = 0.1)
  ) +
  labs(
    x = "Saidin Index Score",
    subtitle = "Rapid growth seen from <b style='color: #B4DE2CFF;'>2002</b> to
    <b style='color: #1F9E89FF;'>2005</b> has <b>since slowed</b> in more recent years."
  )

# write out plots
ggsave(here("task1/figures/saidin_2004.png"), saidin_2004,
       width = 7.8, height = 6.0, units = "in")

ggsave(here("task1/figures/saidin_density_ridge.png"), saidin_density_ridge,
       width = 7.8, height = 6.4, units = "in")

ggsave(here("task1/figures/saidin_boxplot.png"), saidin_boxplot,
       width = 7.8, height = 6.4, units = "in")


###############################################################################
# EXERCISE 3b
###############################################################################

# check for mutual exclusivity
mma_data |>
  filter(nonprof == 1 & govt == 1)

mma_data |>
  filter(nonprof == 0 & govt == 0)

# create independent type variable
mma_types <- mma_data |>
  mutate(
    hospital_type = case_when(
      nonprof == 1 & govt == 0 ~ "nonprof",
      govt == 1 & nonprof == 0 ~ "govt",
      govt == 0 & nonprof == 0 ~ "neither",
      TRUE ~ NA_character_
    )
  )

# limit analysis to 2004
mma_types_2004 <- mma_types |> filter(year == 2004)

# visualization: should expect to see bigger diff between nonprof and other two than govt vs neither
plot_types_2004 <- mma_types_2004 |> ggplot(aes(saidin)) +
  geom_density(aes(color = hospital_type)) +
  theme_ggdist() +
  theme(
    axis.title.y = element_blank(),
    legend.position = 'none',
    plot.title.position = "plot",
    plot.subtitle = element_markdown(lineheight = 0.01)
  ) +
  labs(
    x = "Saidin Index Score",
    subtitle = "<b style='color: #619CFF;'>Non-profit</b> hospitals differ more from
    <b style='color: #F8766D;'>government</b> hospitals and hospitals that are
    <b style='color: #00BA38;'>neither</b>
    \nthan they do from each other in terms of <b>technology adoption</b>."
  ) +
  annotate("text", x = 3.4, y = 0.2, label = "non-profit", color = "#619CFF", size = 3) +
  annotate("text", x = 1.5, y = 0.492, label = "government", color = "#F8766D" , size = 3) +
  annotate("text", x = 1.8, y = 0.38, label = "neither", color = "#00BA38", size = 3)

# assuming normality due to sample size
# one-way ANOVA
anova_types <- aov(saidin ~ hospital_type, data = mma_types_2004)
summary(anova_types)

# Tukey's test
TukeyHSD(anova_types)

# write out plots
ggsave(here("task1/figures/plot_types_2004.png"), plot_types_2004,
       width = 7.8, height = 6.0, units = "in")

###############################################################################
# EXERCISE 3c
###############################################################################

#   c) Do hospitals with more beds tend to have higher Saidin index scores?

