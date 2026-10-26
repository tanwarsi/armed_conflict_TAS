############## PACKAGES #################################################
library(tidyverse)
library(janitor)
library(here)

############# FILE PATH SET UP ##########################################
raw_data <- here("data", "raw")
processed_data <- here("data", "processed")

################# PREPARING WORLD BANK DATA ###############################

#create a function that coverts to long format
make_longer <- function(filename) {
  indicator_name <- tools::file_path_sans_ext(filename)
  #read.csv(file.path("./data/raw", filename), header = TRUE) 
  read.csv(file.path(raw_data, filename), header = TRUE)|>
  pivot_longer(cols = starts_with("X"),
                 names_to = "year",
                 names_prefix = "X", # removes X from year column
                 values_to = indicator_name) |> 
  mutate(year = as.numeric(year)) |> # change year to numeric
  select(iso, year, all_of(indicator_name))
}

#change the rest of the data to the same format
matmor_long<-make_longer("maternal_mortality.csv")
infantmor_long<-make_longer("infant_mortality.csv")
neomor_long<-make_longer("neonatal_mortality.csv")
under5mor_long<-make_longer("under5_mortality.csv")



#################PREPARING DISASTER DATA###################################

#read the file
disaster_raw <- read.csv(
  file.path(raw_data, "disaster.csv"),
  header = TRUE
)
#clean the file
disaster_raw <- disaster_raw |>
  clean_names()

#filtering for 2000-2019 only droughts and earthquakes
disaster <- disaster_raw |>
  filter(year>=2000 & year <=2019)|>
    filter(disaster_type=="Drought"|disaster_type=="Earthquake")

#selecting ISO, Year, and Disaster Type
disaster <-  
  disaster |> 
  select(iso, year, disaster_type)

#dummy vars drought and earthquake created
disaster <- disaster |> 
  mutate(
    drought= if_else(disaster_type== "Drought", 1, 0), 
    earthquake= if_else(disaster_type== "Earthquake", 1, 0)
  )

#retain only ISO, year, drought, earthquake
#change variable names to match the other data sets, retain only 1 country entry per year
disaster <- disaster |>
  group_by(iso, year) |>
  summarise(
    drought = max(drought),
    earthquake = max(earthquake),
    .groups = "drop"
  )|>
  select(iso, year, drought, earthquake)

###########################PREPARING CONFLICT DATA#########################

#reading in the file
#conflict_raw <-read.csv("./data/raw/conflict.csv", header = TRUE)
conflict_raw <- read.csv(
  file.path(raw_data, "conflict.csv"),
  header = TRUE
)


#making only 1 best estimate count per year, per country
conflict <- conflict_raw |>
  group_by(iso, year) |>
  summarise(best = sum(best))

# binary variable indicating the presence of conflict for each country–year observation 
# 0 = no, <25 battle-related deaths; 
# 1 = yes, >=25 battle-related deaths) 


#defining conflict based on conditions above, 
conflict <-conflict |>
  mutate(defined_conflict= if_else(best<25, 0, 1))

#expressing conflict as a lagged term
  conflict <-conflict |>
    mutate(year=year+1)


############################ADDING COVARIATES##############################
#reading in the file
covariates <- read.csv(
  file.path(raw_data, "covariates.csv"),
  header = TRUE
)

###########################MERGE DATA######################################

merged<- list(
  matmor_long,
  infantmor_long,
  neomor_long,
  under5mor_long,
  disaster,
  conflict,
  covariates )|>
    reduce(full_join, by = c("iso", "year")
)

#write into CSV
dir.create(processed_data, recursive = TRUE, showWarnings = FALSE)

write_csv(
  merged,
  file.path(processed_data, "final_data.csv")
)