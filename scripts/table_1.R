############ Packages ####################################################
library(dplyr)
library(tidyr)
library(readr)
library(janitor)
library(ggplot2)
library(flextable) # (LaTex, HTML, or Microsoft Word)
library(xtable) #(Latex or HTML)
library(table1) #(LaTex, HTML, or Microsoft Word; for creating Table 1)
library(texreg)# (LaTex, HTML, or Microsoft Word; for creating table from model outputs)


library(tidyverse)
library(janitor)
library(here)

############# FILE PATH SET UP ##########################################

# read a CSV file from a subfolder
here("data", "processed")
processed_data <- here("data", "processed")
final_data <- read.csv(
  file.path(processed_data, "final_data.csv"),
  header = TRUE
)
#look at the variables
summary(final_data)
class(final_data$oecd)

#Table 1, no straification
table_1_no_strat <- table1(
    ~drought + 
    earthquake + 
      gdp_1000+ 
      oecd +
      pop_dens + 
      rainfall_1000 +
      temp +
      male_edu +
      age_dep +
      urban +
      pop_dens, data=  final_data)
table_1_no_strat
#####################Table 1,straified by armed conflict variable################
final_data$armed_conflict <- addNA(factor(final_data$armed_conflict))#make armed_conflict a factor
levels(final_data$armed_conflict)[is.na(levels(final_data$armed_conflict))] <- "Missing" #turn NA into "Missing"

table_data <- final_data
table_data$earthquake <- factor(table_data$earthquake,
                                levels = c(0, 1),
                                labels = c("No", "Yes"))


label(table_data$earthquake) <- "Earthquake"
label(table_data$drought) <- "Drought"
label(table_data$gdp_1000) <- "GDP per Capita (USD), by 1,000."
label(table_data$oecd) <- "OECD"
label(table_data$rainfall_1000) <- "Rainfall (mm/year)"
label(table_data$temp) <- "Temperature (degrees Celsius)"
label(table_data$male_edu) <- "Male Education (years/capita)"
label(table_data$age_dep) <- "Population Density (>1,000 people/km^2)"
label(table_data$urban) <- "Population Density (>1,000 people/km^2)"

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
      urban +
      pop_dens|armed_conflict, data=  table_data)
table_1_strat