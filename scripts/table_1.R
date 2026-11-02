############ PACKAGES ####################################################
library(tidyverse)
library(here)
library(table1) #(LaTex, HTML, or Microsoft Word; for creating Table 1)
library(kableExtra)



############# FILE PATH SET UP ##########################################

# read final_data in 
here("data", "processed")
processed_data <- here("data", "processed")
output_data<-here("output")
final_data <- read.csv(
  file.path(processed_data, "final_data.csv"),
  header = TRUE
)

#look at the variables
summary(final_data)

##################### Table 1, Stratified by armed conflict variable################

#make armed_conflict  binary
final_data$armed_conflict <- addNA(factor(final_data$armed_conflict))#make armed_conflict a factor
levels(final_data$armed_conflict)[is.na(levels(final_data$armed_conflict))] <- "Missing" #turn NA into "Missing"

#make other variables binary
table_data <- final_data
table_data$earthquake <- factor(table_data$earthquake,
                                levels = c(0, 1),
                                labels = c("No", "Yes"))
table_data$drought<- factor(table_data$drought,
                                levels = c(0, 1),
                                labels = c("No", "Yes"))
table_data$oecd <- factor(table_data$oecd,
                                levels = c(0, 1),
                                labels = c("No", "Yes"))
#Labels
levels(table_data$armed_conflict) <- c(
  "No conflict¹",
  "Conflict",
  "Missing"
)
label(table_data$earthquake) <- "Earthquake"
label(table_data$drought) <- "Drought"
label(table_data$gdp_1000) <- "GDP per Capita (USD)"
label(table_data$oecd) <- "OECD"
label(table_data$rainfall_1000) <- "Rainfall (mm/year)"
label(table_data$temp) <- "Temperature (degrees Celsius)"
label(table_data$male_edu) <- "Male Education (years/capita)"
label(table_data$age_dep) <- "Age Dependency Ratio"
label(table_data$urban) <- "Urban Residence"
label(table_data$pop_dens) <- "Population Density (> 1 000 people/km²)"

caption<-"Table 1. Descriptive Statistics of Country-Year Observations"
footnote <- "Reported as n(%) unless otherwise specified. <br>  ¹ No conflict: fewer than 25 battle-related deaths per country-conflict-year. <br>
GDP per capita is in current US dollars, and its unit is scaled up by 1,000. Population density represents the percentage of the population living
in a density of >1,000 people/km2. Urban residence represents the percentage of the population living in urban areas. The age dependency ratio 
represents the proportion of dependents (aged under 15 years or over 64 years) per 100 working-age individuals AU.  Male education is expressed as years per capita
and is age-standardised. Temperature is in degrees Celsius and is the mean population-weighted annual temperature. Rainfall is the mean population weighted annual
rainfall in millimetres per year, scaled down by 1,000. Earthquake and drought are binary variables representing their absence or presence.
All armed conflict variables were lagged by 1 year. <br>
GDP,gross domesticproduct; OECD, Organisation for Economic Co-operation and Development."

table_1_strat <- table1(
    ~drought + 
   earthquake + 
      gdp_1000+ 
      oecd +
      pop_dens + 
      rainfall_1000 +
      temp +
      male_edu +
      age_dep +
      urban 
      |armed_conflict, data=  table_data, caption=caption, footnote=footnote)

table_1_strat

#Output as a PDF, save as apdf
here("output")
table_1_pdf<- t1kable(table_1_strat, format = "latex")
save_kable(
  table_1_pdf,
  file = here("output", "table_1.pdf")
)
