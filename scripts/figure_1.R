############ PACKAGES ###############################################################
library(tidyverse)
library(here)

############# FILE PATH SET UP #####################################################

# Read the processed data.
processed_data <- here("data", "processed")
final_data <- read.csv(
  file.path(processed_data, "final_data.csv"),
  header = TRUE)

############# FILTERING COUNTRIES ##################################################
# Select countries whose maternal mortality was higher in 2017 than in 2000.
increased_countries <- final_data|>
  dplyr::select(iso, year, maternal_mortality) |>
  dplyr::filter(year < 2018) |>
  arrange(iso, year) |>
  group_by(iso) |>
  mutate(diff_matmor = maternal_mortality - maternal_mortality[1L])|>
  filter(any(diff_matmor > 0 & year == 2017)) |>
  ungroup()

maternal_mortality_plot <- ggplot(
  data = increased_countries,
  aes(x = year, y = maternal_mortality, colour = iso)
) +
  geom_line(linewidth = 0.6, na.rm = TRUE) +
  labs(
    title = "Maternal Mortality Trends in Countries with an Increase",
    subtitle = "Countries with higher maternal mortality in 2017 than in 2000",
    x = "Year",
    y = "Maternal mortality (per 100,000 live births)",
    colour = "Country (ISO)"
  ) +
  theme_minimal()

maternal_mortality_plot

dir.create(here("output"), recursive = TRUE, showWarnings = FALSE)
ggsave(
  filename = here("output", "figure_1.png"),
  plot = maternal_mortality_plot,
  width = 10,
  height = 7,
  dpi = 300
)
