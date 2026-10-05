#Set up
getwd()
library(tidyverse)
library(janitor)

#################PREPARING WORLD BANK DATA###############################
# Read in mortality data
matmor <- read.csv("./data/raw/maternal_mortality.csv", header = TRUE)
# neonatalmor <- read.csv("./data/raw/neonatal_mortality.csv", header = TRUE)
# under5mor <- read.csv("./data/raw/under5_mortality.csv", header = TRUE)
# infantmor <- read.csv("./data/raw/infant_mortality.csv", header = TRUE)

# Change maternal mortality wide to long format
matmor_long <- matmor |>
  pivot_longer(cols = starts_with("X"),
                 names_to = "year",
                 names_prefix = "X", # removes X from year column
                 values_to = "maternal_mortality") |> 
  mutate(year = as.numeric(year)) |> # change year to numeric
  select(iso, year, maternal_mortality)


#create a function that coverts to long format
make_longer <- function(filename) {
  indicator_name <- tools::file_path_sans_ext(filename)
  read.csv(file.path("./data/raw", filename), header = TRUE) |>
  pivot_longer(cols = starts_with("X"),
                 names_to = "year",
                 names_prefix = "X", # removes X from year column
                 values_to = indicator_name) |> 
  mutate(year = as.numeric(year)) |> # change year to numeric
  select(iso, year, all_of(indicator_name))
}

#change the rest of the data to the same format
infantmor_long<-make_longer("infant_mortality.csv")
neomor_long<-make_longer("neonatal_mortality.csv")
under5mor_long<-make_longer("under5_mortality.csv")



#################PREPARING DISASTER DATA###############################

#read the file
disaster_raw <- read.csv("./data/raw/disaster.csv", header = TRUE)

#clean the file
clean_names(disaster_raw)

#filtering for 2000-2019 only droughts and earthquakes
disaster <- disaster_raw |>
  filter(Year>=2000 & Year <=2019)|>
    filter(Disaster.Type=="Drought"|Disaster.Type=="Earthquake")

#selecting ISO, Year, and Disaster Type
disaster <-  
  disaster |> 
  select(ISO, Year, Disaster.Type)

#dummy vars drought and earthquake created
disaster <- disaster |> 
  mutate(
    drought= if_else(Disaster.Type== "Drought", 1, 0), 
    earthquake= if_else(Disaster.Type== "Earthquake", 1, 0)
  )

#retain only ISO, year, drought, earthquake
#change variable names to match the other data sets
disaster <- disaster |>
  mutate(iso = ISO) |>
  mutate(year = Year) |>
  select(iso, year, drought, earthquake)

###########################PREPARING CONFLICT DATA###############################

#reading in the file
conflict_raw <-read.csv("./data/raw/conflict.csv", header = TRUE)


#making only 1 best estimate count per year, per country
conflict <- conflict_raw |>
  group_by(iso, year) |>
  summarise(best = sum(best))

# binary variable indicating the presence of conflict for each country–year observation 
# 0 = no, <25 battle-related deaths; 
# 1 = yes, >=25 battle-related deaths) 


#defining conflict based on conditions above, 
conflict <-conflict |>
  mutate(defined_conflict= if_else(best<25, "no", "yes"))

#expressing conflict as a lagged term
  conflict <-conflict |>
    mutate(year=year+1)

###########################MERGE DATA###############################

library(purrr)

#merge all  data
merged<- list(
  conflict,
  disaster,
  matmor_long,
  infantmor_long,
  neomor_long,
  under5mor_long) |>
    reduce(full_join, by = c("iso", "year")
)

#write into CSV
write_csv(merged, "./data/processed/final_data.csv")

