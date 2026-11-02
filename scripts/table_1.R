############ PACKAGES ####################################################
library(tidyverse)
library(here)
library(table1) # Creates tables in HTML, LaTeX, or Microsoft Word formats.
library(kableExtra)

############# FILE PATH SETUP ############################################
processed_data <- here("data", "processed")
final_data <- read.csv(
  file.path(processed_data, "final_data.csv"),
  header = TRUE
)

##################### TABLE 1 ############################################

# Preserve missing conflict status as an explicit stratum.
table_data <- final_data
table_data$armed_conflict <- addNA(factor(table_data$armed_conflict))
levels(table_data$armed_conflict)[is.na(levels(table_data$armed_conflict))] <- "Missing"

# Set binary variables to readable labels.
table_data$earthquake <- factor(
  table_data$earthquake,
  levels = c(0, 1),
  labels = c("No", "Yes")
)
table_data$drought <- factor(
  table_data$drought,
  levels = c(0, 1),
  labels = c("No", "Yes")
)
table_data$oecd <- factor(
  table_data$oecd,
  levels = c(0, 1),
  labels = c("No", "Yes")
)

# Map conflict labels by source value rather than relying on factor order.
conflict_levels <- levels(table_data$armed_conflict)
conflict_labels <- c(
  `0` = "No conflict¹",
  `1` = "Conflict",
  Missing = "Missing"
)
levels(table_data$armed_conflict) <- unname(
  conflict_labels[conflict_levels]
)

# Variable labels include units and scaling where applicable.
label(table_data$earthquake) <- "Earthquake"
label(table_data$drought) <- "Drought"
label(table_data$gdp_1000) <- "GDP per capita (current USD, thousands)"
label(table_data$oecd) <- "OECD member"
label(table_data$rainfall_1000) <- "Rainfall (mm/year, thousands)"
label(table_data$temp) <- "Temperature (°C)"
label(table_data$male_edu) <- "Male education (age-standardised years per capita)"
label(table_data$age_dep) <- "Age dependency ratio (per 100 working-age individuals)"
label(table_data$urban) <- "Urban population (%)"
label(table_data$pop_dens) <- "Population living in areas >1,000 people/km² (%)"

caption <- "Descriptive statistics of country-year observations"
footnote <- paste0(
  "Categorical variables are reported as n (%). Continuous variables are reported as median [IQR].<br>",
  "¹ No conflict: fewer than 25 battle-related deaths per country-conflict-year. ",
  "GDP per capita is in current US dollars, scaled by 1,000. Rainfall is mean ",
  "population-weighted annual rainfall in mm, scaled by 1,000. Temperature is ",
  "mean population-weighted annual temperature in °C. Urban population and ",
  "population living in high-density areas are percentages. The age dependency ",
  "ratio is dependents (under 15 or over 64 years) per 100 working-age ",
  "individuals. Male education is age-standardised and expressed as years per ",
  "capita. Earthquake and drought indicate absence or presence. Armed conflict ",
  "is lagged by one year.<br>",
  "GDP, gross domestic product; OECD, Organisation for Economic Co-operation and Development."
)

# Show continuous variables as median [25th, 75th percentile].
render_median_iqr <- function(x, name, ...) {
  if (length(x) == 0) {
    return(c("", "Median [IQR]" = "Missing"))
  }

  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(c("", "Median [IQR]" = "Missing"))
  }

  stats <- quantile(x, probs = c(0.25, 0.5, 0.75), names = FALSE)
  c(
    "",
    "Median [IQR]" = sprintf("%.2f [%.2f, %.2f]", stats[2], stats[1], stats[3])
  )
}

table_1_strat <- table1(
  ~ drought + 
    earthquake +
    gdp_1000 + 
    oecd + 
    pop_dens + 
    rainfall_1000 +
  temp + male_edu + age_dep + urban | armed_conflict,
  data = table_data,
  caption = caption,
  footnote = footnote,
  render.continuous = render_median_iqr
)

table_1_strat

# Export the table as a PDF.
table_1_pdf <- t1kable(table_1_strat, format = "latex")
save_kable(
  table_1_pdf,
  file = here("output", "table_1.pdf")
)
