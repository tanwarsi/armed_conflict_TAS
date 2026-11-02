############ PACKAGES ####################################################
library(tidyverse)
library(janitor)
library(here)
library(dplyr)
library(tidyr)
library(readr)
library(janitor)
library(ggplot2)
library(table1) #(LaTex, HTML, or Microsoft Word; for creating Table 1)

############# FILE PATH SET UP ##########################################

# read a CSV file from a subfolder
here("data", "processed")
processed_data <- here("data", "processed")
output_data<-here("output")
final_data <- read.csv(
  file.path(processed_data, "final_data.csv"),
  header = TRUE
)
#look at the variables
summary(final_data)


##################### Table 1,straified by armed conflict variable################

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
levels(table_data$armed_conflict) <- c("No conflict<sup>1</sup>", "Conflict", "Missing")
label(table_data$earthquake) <- "Earthquake"
label(table_data$drought) <- "Drought"
label(table_data$gdp_1000) <- "GDP per Capita (USD)"
label(table_data$oecd) <- "OECD"
label(table_data$rainfall_1000) <- "Rainfall (mm/year)"
label(table_data$temp) <- "Temperature (degrees Celsius)"
label(table_data$male_edu) <- "Male Education (years/capita)"
label(table_data$age_dep) <- "Age Dependency Ratio"
label(table_data$urban) <- "Urban Residence"
label(table_data$pop_dens) <- "Population Density (&gt; 1 000 people/km<sup>2</sup>)"

caption<-"Table 1. Descriptive Statistics of Country-Year Observations"
footnote<-" Reported as n(%) unless otherwise specified 
<br> <sup>1</sup> No conflict:<25 battle-related deaths  per  country–conflict–year."
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
      pop_dens|armed_conflict, data=  table_data, caption=caption, footnote=footnote)

table_1_strat

#Output as a PDF, save as apdf
old_wd <- getwd()
setwd(output_data)
output_data<-here("output")
table_1_pdf<- t1kable(table_1_strat, format = "latex")
save_kable(table_1_pdf, file= file.path(output_data, "table_1.pdf"))

