# Prepare World Bank mortality indicators in a common country-year format.

library(dplyr)
library(tidyr)
library(readr)
library(janitor)

# Convert one World Bank mortality data frame from wide to long format.
# `value_name` becomes the name of the indicator-specific value column.
make_mortality_long <- function(data, value_name) {
  data |>
    pivot_longer(
      cols = starts_with("X"),
      names_to = "year",
      names_prefix = "X",
      values_to = value_name
    ) |>
    mutate(year = as.numeric(year)) |>
    select(iso, year, all_of(value_name))
}

# Read every World Bank mortality file, then apply the same transformation.
mortality_files <- c(
  maternal_mortality = "data/raw/maternal_mortality.csv",
  infant_mortality = "data/raw/infant_mortality.csv",
  neonatal_mortality = "data/raw/neonatal_mortality.csv",
  under5_mortality = "data/raw/under5_mortality.csv"
)

mortality_long <- Map(
  f = function(file, indicator) {
    read_csv(file, show_col_types = FALSE) |>
      make_mortality_long(value_name = indicator)
  },
  file = mortality_files,
  indicator = names(mortality_files)
)

names(mortality_long) <- names(mortality_files)

# Convenient individual data frames, each with iso, year, and its indicator.
maternal_mortality_long <- mortality_long$maternal_mortality
infant_mortality_long <- mortality_long$infant_mortality
neonatal_mortality_long <- mortality_long$neonatal_mortality
under5_mortality_long <- mortality_long$under5_mortality

# Prepare disaster records as country-year indicators.
disaster <- read_csv("data/raw/disaster.csv", show_col_types = FALSE) |>
  clean_names() |>
  filter(
    between(year, 2000, 2019),
    disaster_type %in% c("Earthquake", "Drought")
  ) |>
  select(year, iso, disaster_type) |>
  mutate(
    drought = as.integer(disaster_type == "Drought"),
    earthquake = as.integer(disaster_type == "Earthquake")
  ) |>
  group_by(year, iso) |>
  summarise(
    earthquake = max(earthquake),
    drought = max(drought),
    .groups = "drop"
  ) |>
  select(year, iso, earthquake, drought)

# Prepare the lagged binary armed-conflict exposure.
# A country-year with fewer than 25 total battle-related deaths is coded 0;
# 25 or more deaths is coded 1.  The year is then advanced for a one-year lag.
conflict <- read_csv("data/raw/conflict.csv", show_col_types = FALSE) |>
  group_by(iso, year) |>
  summarise(best = sum(best, na.rm = TRUE), .groups = "drop") |>
  mutate(
    armed_conflict = if_else(best >= 25, 1L, 0L),
    year = year + 1
  ) |>
  select(iso, year, armed_conflict)

# Merge the prepared mortality, disaster, conflict, and covariate data sets.
covariates <- read_csv("data/raw/covariates.csv", show_col_types = FALSE)

final_data <- Reduce(
  f = function(x, y) full_join(x, y, by = c("iso", "year")),
  x = list(
    maternal_mortality_long,
    infant_mortality_long,
    neonatal_mortality_long,
    under5_mortality_long,
    disaster,
    conflict,
    covariates
  )
)

write_csv(final_data, "data/processed/final_data_codex.csv", na = "")
