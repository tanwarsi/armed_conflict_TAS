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

# Join country names from the project's ISO lookup table.
country_names <- read.table(
  here("data", "raw", "countries.txt"),
  header = TRUE,
  stringsAsFactors = FALSE
) |>
  dplyr::transmute(iso = ISO, country_name)

increased_countries <- increased_countries |>
  dplyr::left_join(country_names, by = "iso") |>
  dplyr::mutate(country_name = dplyr::coalesce(country_name, iso))

# Label each country at its 2017 endpoint.
endpoint_labels <- increased_countries |>
  dplyr::filter(year == 2017, !is.na(maternal_mortality))

maternal_mortality_plot <- ggplot(
  data = increased_countries,
  aes(x = year, y = maternal_mortality, colour = country_name, group = iso)
) +
  geom_line(linewidth = 0.45, alpha = 0.8, na.rm = TRUE) +
  geom_text(
    data = endpoint_labels,
    aes(label = country_name),
    hjust = -0.15,
    size = 3,
    show.legend = FALSE,
    check_overlap = TRUE
  ) +
  scale_colour_viridis_d(option = "D", end = 0.9) +
  scale_y_log10() +
  scale_x_continuous(
    breaks = seq(2000, 2015, by = 5),
    limits = c(2000, 2018),
    expand = expansion(mult = c(0.01, 0.08))
  ) +
  labs(
    title = "Maternal mortality trends",
    subtitle = "Countries where maternal mortality was higher in 2017 than in 2000",
    x = NULL,
    y = "Maternal mortality (deaths per 100,000 live births; log scale)",
    colour = "Country"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, colour = "grey30"),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10, colour = "grey25"),
    panel.grid.major = element_line(colour = "grey85", linewidth = 0.35),
    panel.grid.minor = element_line(colour = "grey92", linewidth = 0.2),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 8),
    plot.caption = element_text(size = 9, colour = "grey40", hjust = 0),
    plot.margin = margin(14, 30, 14, 14)
  )

maternal_mortality_plot

dir.create(here("output"), recursive = TRUE, showWarnings = FALSE)
ggsave(
  filename = here("output", "figure_1.png"),
  plot = maternal_mortality_plot,
  width = 10,
  height = 7,
  dpi = 300
)
