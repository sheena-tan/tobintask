# exercise 3 (data visualization)

library(tidyverse)
library(here)
library(ggridges)
library(ggtext)
library(viridis)

# load data
load(here("task1/data/mma_data.rds"))

# Distribution of Saidin Index scores by year ----
mma_data <- mma_data |>
  mutate(year = as.factor(year)) |>
  mutate(year = fct_rev(year))

# density ridges
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
ggsave(here("task1/figures/saidin_density_ridge.png"), saidin_density_ridge,
       width = 7.8, height = 6.4, units = "in")

ggsave(here("task1/figures/saidin_boxplot.png"), saidin_boxplot,
       width = 7.8, height = 6.4, units = "in")

# b) Looking at the Saidin Index scores you’ve calculated, do different types of hospitals
# have different rates of technology adoption?
#   c) Do hospitals with more beds tend to have higher Saidin index scores?
#   Output: Provide written answers to each question, with supporting evidence, in your
# write up. (Also make sure to include the code used to produce your results in your .do
#            file.)

# looking at 2004 only
# output to include explanations of graph/table(s)
