library(tidyverse)
library(here)
library(plm)
library(texreg)
library(modelsummary)
library(pagedown)
library(mice)
library(naniar)

##### Load in final_data file
processed_data <- here("data", "processed")
final_data <- read.csv(
              file.path(processed_data, "final_data.csv"), 
              header = TRUE)

##### observe missingness
summary(final_data)
final_data |>
  arrange(year, iso) |>
  dplyr::select(-iso, -year) |>
  vis_miss() #missingness obseved for the best variable and maternal mortality MOSTLY

##### Log transformation of gdp variable
final_data <-final_data|>  
 mutate(logGDP= log(gdp_1000))

#convert iso to numeric
midata <- final_data |>
  mutate(iso_num = as.numeric(as.factor(final_data$iso))) |>
  select(-iso)

#dry run
mice0  <- mice(midata, seed = 100, m = 5, maxit = 0, print = F)

#edit method and prediction matrix
meth[c("urban", "male_edu", "temp", "rainfall_1000", "logGDP", "pop_dens",
       "maternal_mortality", "infant_mortality", "neonatal_mortality",
        "under5_mortality")] <- "2l.lmer" #change imputation method
meth <- mice0$method

pred[c("urban", "male_edu", "temp", "rainfall_1000", "logGDP", "pop_dens",
       "maternal_mortality", "infant_mortality", "neonatal_mortality", 
       "under5_mortality"), "iso_num"]<- -2 #--> treat iso_num as a class variable, specify which values are imputed
pred <- mice0$predictorMatrix

##### Perform MI with 10 imputed data sets, 20 iterations per data set
mice.multi.out  <- mice(midata, seed = 100, m = 10, maxit = 20,
                        method = meth,
                        predictorMatrix = pred)

##### Save MI output as an R object
save(mice.multi.out, file = "output//mice_out.RData")

##### Check for convergance
plot(mice.multi.out)

##### Fit maternal morality regression model on the 10 MI data set:
fit.mi.matmor <- with(mice.multi.out, 
                      lm(maternal_mortality ~ -1 + armed_conflict + logGDP + oecd + pop_dens + urban + 
                           age_dep + male_edu + temp + rainfall_1000 + earthquake + drought + 
                           as.factor(iso_num) + as.factor(year)))
fit.mi.infantmor <- with(mice.multi.out, 
                      lm(infant_mortality ~ -1 + armed_conflict + logGDP + oecd + pop_dens + urban + 
                           age_dep + male_edu + temp + rainfall_1000 + earthquake + drought + 
                           as.factor(iso_num) + as.factor(year)))
fit.mi.under5mor  <- with(mice.multi.out, 
                      lm(under5_mortality ~ -1 + armed_conflict + logGDP + oecd + pop_dens + urban + 
                           age_dep + male_edu + temp + rainfall_1000 + earthquake + drought + 
                           as.factor(iso_num) + as.factor(year)))
fit.mi.neonatalmor <- with(mice.multi.out, 
                      lm(maternal_mortality ~ -1 + armed_conflict + logGDP + oecd + pop_dens + urban + 
                           age_dep + male_edu + temp + rainfall_1000 + earthquake + drought + 
                           as.factor(iso_num) + as.factor(year)))

##### Use pool() to summarize the regression estimates and standard errors
out.mi.matmor <- pool(fit.mi.matmor)
out.mi.infantmor <- pool(fit.mi.infantmor)
out.mi.under5mor <- pool(fit.mi.under5mor)
out.mi.neonatalmor <- pool(fit.mi.neonatalmor)

summary(out.mi.matmor) #summary() function will produce a table with estimates, standard errors, etc.

##### use model summary to generate the table
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
                output = here("output", "table_2.html")
)
table_2
#Output as PDF
chrome_print(
  input = here::here("output", "table_2.html"),
  output = here::here("output", "table_2.pdf")
)


