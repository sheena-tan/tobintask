# Introduction

This project uses data provided by Yale's Tobin Center for Economic Policy as part of a data task for a pre-doctoral research position. For the first task, the data draws from a larger research project aiming to understand hospital response to reimbursement rate adjustment received as part of the Medicare Modernization Act (MMA) of 2003, and whether their adoption of medical technologies increased as a result of those adjustments. Task 2 uses sample Medicaid data to investigate the effects of managed care.

# Repository Setup
In addition to the project memos (`memo.html` and `memo.qmd`), this repository contains:

- `task1/` and `task2/` subdirectories corresponding to Task 1 and Task 2 of the data task, respectively
- `data/` subdirectories containing the data for each task
- `figures/` subdirectores containing the figures produced from each task
- `scripts/` subdirectories containing the R Scripts for each task

## Task 1 Setup
The data subdirectory contains:

- `datatask_main.csv` and `datatask_treat.csv`: the provided datasets for Task 1
- `mma_data.rds`: a dataset that combines the two provided datsets
- `mma_did.rds`: a dataset containing estimates from a difference-in-difference analysis

The figures subdirectory contains:

- `did_plot.png`: visualization of a difference-in-difference analysis
- `plot_beds_2004.png`: scatterplot of Saidin scores and beds in 2004
- `plot_types_2004.png`: density plots of Saidin scores by hospital type in 2004
- `saidin_2004.png`: density-boxplot of Saidin Index scores in 2004
- `saidin_boxplot.png`: stacked barplots of Saidin Index scores by year
- `saidin_density_ridge.png`: density ridge plot of Saidin Index scores by year

The scripts subdirectory contains 

- `1_intial_setup.R`: exercise 1 (cleaning, joins) and exercise 2 (mutate)
- `2_eda.R`: exercise 3 (data visualization, statistical testing)
- `3_did.R`: exercise 4-5 (difference in difference model analysis and visualization)

## Task 2 Setup
The data subdirectory contains:

- `healthcare_data.csv`: the provided dataset for Task 2

The figures subdirectory contains:

- `psm_plot.png`: a Love plot of before and after a propensity score matching analysis

The scripts subdirectory contains 

- `1_intial_setup.R`: exercise 1 (cleaning, randomness testing)
- `2_causal_estimates.R`: exercise 2-4 (causal estimates)

