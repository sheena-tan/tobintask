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

# b) Looking at the Saidin Index scores you’ve calculated, do different types of hospitals
# have different rates of technology adoption?


###############################################################################
# EXERCISE 3c
###############################################################################

#   c) Do hospitals with more beds tend to have higher Saidin index scores?

