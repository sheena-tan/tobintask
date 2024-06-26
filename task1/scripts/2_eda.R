# exercise 3 (data visualization, statistical testing)

library(tidyverse)
library(here)
library(ggridges)
library(ggdist)
library(patchwork)
library(ggtext)
library(viridis)
library(psych)

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
mma_2004 |>
  filter(nonprof == 1 & govt == 1)

mma_2004 |>
  filter(nonprof == 0 & govt == 0)

# create independent type variable
mma_types <- mma_2004 |>
  mutate(
    hospital_type = case_when(
      nonprof == 1 & govt == 0 ~ "nonprof",
      govt == 1 & nonprof == 0 ~ "govt",
      govt == 0 & nonprof == 0 ~ "neither",
      TRUE ~ NA_character_
    )
  )

# visualization: expecting bigger diff between nonprof and others than govt vs neither
plot_types_2004 <- mma_types |>
  ggplot(aes(saidin)) +
  geom_density(aes(color = hospital_type)) +
  theme_ggdist() +
  theme(
    axis.title.y = element_blank(),
    legend.position = 'none',
    plot.title.position = "plot",
    plot.title = element_markdown()
  ) +
  labs(
    x = "Saidin Index Score",
    title = "Technology adoption rates at <b style='color: #619CFF;'>non-profit</b> hospitals
    seem to <b>differ significantly</b> from other hospitals."
  ) +
  annotate("text", x = 3.4, y = 0.2, label = "non-profit", color = "#619CFF", size = 3) +
  annotate("text", x = 1.5, y = 0.492, label = "government", color = "#F8766D" , size = 3) +
  annotate("text", x = 1.8, y = 0.38, label = "neither", color = "#00BA38", size = 3)

# assuming normality due to sample size
# one-way ANOVA
anova_types <- aov(saidin ~ hospital_type, data = mma_types)
summary(anova_types)

# Tukey's test
TukeyHSD(anova_types)

# write out plot
ggsave(here("task1/figures/plot_types_2004.png"), plot_types_2004)

###############################################################################
# EXERCISE 3c
###############################################################################

# visualization: expecting positive correlation
plot_beds_2004 <- mma_2004 |>
  ggplot(aes(x = beds, y = saidin)) +
  geom_point(alpha = 0.4, color = "#619CFF") +
  theme_minimal() +
  stat_smooth(method = "lm", formula = y ~ x, geom = "smooth") +
  scale_x_continuous(limits = c(0, 1000)) +
  labs(
    x = "Beds",
    y = "Saidin Score",
    title = "Hospitals with more beds seem to have higher Saidin index scores.",
    subtitle = "Hospital size and technological capacity appear positively correlated."
  ) +
  theme(
    plot.title.position = "plot",
    plot.title = element_markdown(),
    plot.subtitle = element_markdown()
  )

# logistic regression
lm_beds <- lm(saidin ~ beds, data = mma_2004)
summary(lm_beds)
confint(lm_beds)

# write out plot
ggsave(here("task1/figures/plot_beds_2004.png"), plot_beds_2004,
       width = 7.8, height = 6.0, units = "in")
