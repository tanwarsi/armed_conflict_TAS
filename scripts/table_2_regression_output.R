library(tidyverse)
library(here)
library(plm)
library(texreg)
library(modelsummary)
library(pagedown)

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
#use model summary to generate the table
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
                stars = c('*' = 0.05, '**' = 0.01, '***' = 0.001),
                title = "Table 2. The association between armed conflict and maternal and child mortality (adjusted beta coefficients, 95% confidence intervals) ",
                notes = "
                Data are given as adjusted beta coefficient (95% Confidence Interval). 
                Standard errors (in parentheses) are model-based OLS standard errors.
                Each column is the output from a panel regression model with fixed effects (country and year as fixed effects) 
                examining the association between the presence of armed conflict (main predictor) and the dependent variable, adjusted for the covariates shown. 
                Coefficients are interpreted as the absolute change in the dependent variable following a 1- unit change in the independent 
                variable for continuous covariates and a change from yes to no for binary covariates. 
                GDP per capita is in current US dollars, and its unit is scaled up by 1,000. Population
                density represents the percentage of the population living in a density of >1,000 people/km2. 
                 Urban residence represents the percentage of the population living in urban areas. The age 
                 dependency ratio represents the proportion of dependents (aged under 15 years or over 64 years) 
                 per 100 working-age individuals. Male education is 
                 expressed as years per capita and is age-standardised. Temperature is in degrees Celsius and 
                 is the mean population-weighted annual temperature. Rainfall is the mean population weighted
                annual rainfall in millimetres per year, scaled down by 1,000. Earthquake and drought are 
                binary variables representing their absence or presence. All armed conflict variables were 
                lagged by 1 year.",
                output = here("output", "table_2.pdf")
)
table_2

chrome_print(
  input = here::here("output", "table_2.html"),
  output = here::here("output", "table_2.pdf")
)