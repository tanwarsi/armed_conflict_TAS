#Set up
getwd()
library(tidyverse)
library(janitor)

#################PREPARING WORLD BANK DATA###############################
# Read in mortality data
matmor <- read.csv("./data/raw/maternal_mortality.csv", header = TRUE)
neonatalmor <- read.csv("./data/raw/neonatal_mortality.csv", header = TRUE)
under5mor <- read.csv("./data/raw/under5_mortality.csv", header = TRUE)
infantmor <- read.csv("./data/raw/infant_mortality.csv", header = TRUE)

# Change maternal mortality wide to long format

matmor_long <- matmor |>
  pivot_longer(cols = starts_with("X"),
                 names_to = "year",
                 names_prefix = "X", # removes X from year column
                 values_to = "maternal_mortality") |> 
  mutate(year = as.numeric(year)) |> # change year to numeric
  select(iso, year, maternal_mortality)


#create a function that  coverts to long format
make_longer <- function(filename) {
  read.csv(file.path("./data/raw", filename), header = TRUE) |>
  pivot_longer(cols = starts_with("X"),
                 names_to = "year",
                 names_prefix = "X", # removes X from year column
                 values_to = "indicator2") |> 
  mutate(year = as.numeric(year)) |> # change year to numeric
  select(iso, year, indicator)
}
infantmor_long<-make_longer(infantmor)
neomor_long<-make_longer(neonatalmor)
under5mor_long<-make_longer(under5mor)

#make_longer <- function(dataname){
 # dataname |>
  #pivot_longer(cols = starts_with("X"),
   #              names_to = "year",
    #             names_prefix = "X", # removes X from year column
     #            values_to = "indicator2") |> 
  #mutate(year = as.numeric(year)) |> # change year to numeric
 # select(iso, year, indicator)
#}

#paths <- list.files("./data/raw", pattern = "[.]csv$", full.names = TRUE)
#paths
#files <- map(paths, read.csv::read_csv)

#read_fun <- function(dataname, varname){
 #  dataname |>
   # pivot_longer(cols = starts_with("X"),
    #             names_to = "year",
     #            names_prefix = "X",
      #           values_to = varname) |>
    #mutate(year = as.numeric(year)) |>
    #select(iso, year, varname)
#}


#####for reference###############
# infantmor <- read.csv("./data/raw/infant_mortality.csv", header = TRUE)
# neonatalmor <- read.csv("./data/raw/neonatal_mortality.csv", header = TRUE)
# under5mor <- read.csv("./data/raw/under5_mortality.csv", header = TRUE)
# matmor <- read.csv("./data/raw/maternal_mortality.csv", header = TRUE)

# wbfun <- function(dataname, varname){
#   dataname |>
#     dplyr::select(Country.Name, X2000:X2019) |>
#     pivot_longer(cols = starts_with("X"),
#                  names_to = "year",
#                  names_prefix = "X",
#                  values_to = varname) |>
#     mutate(year = as.numeric(year)) |>
#     arrange(Country.Name, year)
# }

# matmor <- wbfun(dataname = matmor0, varname = "matmor")
# infmor <- wbfun(dataname = infmor0, varname = "infmor")
# neomor <- wbfun(dataname = neomor0, varname = "neomor")
# un5mor <- wbfun(dataname = un5mor0, varname = "un5mor")

#################PREPARING DISASTER DATA###############################

#read the file
disaster <- read.csv("./data/raw/disaster.csv", header = TRUE)

#clean the file
clean_names(disaster)

#filtering
disaster2 <- disaster |>
  filter(Year>=2000 & Year <=2019)|>
    filter(Disaster.Type=="Drought"|Disaster.Type=="Earthquake")

#selecting ISO, Year, and Disaster Type
disaster2 <-  
  disaster2 |> 
  select(ISO, Year, Disaster.Type)

#dummy vars drought and earthquake created
disaster2 <- disaster2 |> 
  mutate(
    drought= if_else(Disaster.Type== "Drought", 1, 0), 
    earthquake= if_else(Disaster.Type== "Earthquake", 1, 0)
  )

#retain only ISO, Year, drought, earthquake
disaster2 <-  
  disaster2 |> 
  select(ISO, Year, drought, earthquake)

###########################PREPARING CONFLICT DATA###############################

#reading in the file
conflict <-read.csv("./data/raw/conflict.csv", header = TRUE)

#0 = no, <25 battle-related deaths
# 1 = minor conflict, 25–999 battle-related deaths;
# 2 = war, �1,000 battle-related deaths
# you need to think about the lag

#defining conflict
conflict <-conflict |>
  mutate(defined_conflict= if_else(best<25, "no", "yes"))

###########################MERGE DATA###############################
