library(tidyverse)
library(here)
library(plm)
library(texreg)
library(modelsummary)

#Load in final_data file
processed_data <- here("data", "processed")
final_data <- read.csv(
              file.path(processed_data, "final_data.csv"), 
              header = TRUE)

#Log transformation of gdp variable
final_data <-final_data|>  
  mutate(logGDP= log(gdp_1000))

#Create formula
preds <- as.formula(" ~ armed_conflict + logGDP + oecd + pop_dens + urban +
+                   age_dep + male_edu + temp + rainfall_1000 + earthquake + drought")

#Run the 4 models
maternal_mortalitymod <- plm(update.formula(preds, maternal_mortality ~ .), 
                            index = c("iso", "year"), 
                             effect = "twoways", 
                             model = "within",
                             data = final_data)

un5mormod <- plm(update.formula(preds, under5_mortality ~ .),
                   index = c("iso", "year"), 
                   effect = "twoways", 
                   model = "within",
                   data = final_data)

infmormod <- plm(update.formula(preds, infant_mortality ~ .), 
                  index = c("iso", "year"), 
                   effect = "twoways", 
                   model = "within",
                   data = final_data)

neomormod <- plm(update.formula(preds, neonatal_mortality ~ .), 
                  index = c("iso", "year"), 
                   effect = "twoways", 
                   model = "within",
                   data = final_data)
neomormod
table_2 <- modelsummary(
            list("Maternal Mortality" = maternal_mortalitymod,
                "Neonatal Morality" = neomormod,
                "Under Five Mortality" = un5mormod ,
                "Infant Mortality" = infmormod ),
                coef_map = c("armed_conflict" = "Armed conflict", 
                            "logGDP" = "GDP per Capita",
                            "oecd"= "OECD Member",
                            "pop_dens"= "Population Density",
                            "urban"= "Urban Residence",
                            "age_dep"= "Age Dependency Ratio",
                            "male_edu"= "Male Educution",
                            "temp"= "Temperature",
                            "rainfall_1000"="Rainfall"
                          ),
                gof_omit = ".*",
                statistic = "conf.int",
                conf_level = 0.95,
                title = "Table 2. The association between armed conflict and maternal and child mortality (95% CI) ",
                notes = "Each column is the output from a panel regression with fixed effects (country and year as fixed effects) with the presence of armed conflict as the main predictor, adjusted for the covariates shown. Coefficients are interpreted as the absolute change in the dependent variable following a 1- unit change in the independent variable for continuous covariates and a change from yes to no for binary covaraites",
                output = here("output", "table_2.html")
)
plmmod <- plm(maternal_mortality ~ armed_conflict + gdp_1000 + oecd + pop_dens + urban +
              age_dep + male_edu + temp + rainfall_1000 + earthquake + drought,
              index = c("iso", "year"),
              effect = "twoways",
              model = "within",
              data = final_data)
plmmod