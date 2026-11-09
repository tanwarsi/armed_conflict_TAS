############ PACKAGES ##############################################
library(tidyverse)
library(here)
library(plm)
library(texreg)

############# LOAD FILE ############################################
processed_data <- here("data", "processed")
final_data <- read.csv(
  file.path(processed_data, "final_data.csv"),
  header = TRUE
)

############# PLM REGRESSION #######################################

plmmod <- plm(maternal_mortality ~ armed_conflict + gdp_1000 + oecd + pop_dens + urban +
              age_dep + male_edu + temp + rainfall_1000 + earthquake + drought,
              index = c("iso"), model = "within", data = final_data)
plmmod

############# LM REGRESSION #########################################

lmmod <- lm(maternal_mortality ~ -1 + armed_conflict + gdp_1000 + oecd + pop_dens + urban +
              age_dep + male_edu + temp + rainfall_1000 + earthquake + drought +
              iso, 
            data = final_data)
lmmod 
#notice the exact same coefficicents for armed_conflict --> drought but
#  then a dummy variable for each country

htmlreg(list(lmmod, plmmod))

#year and country as fixed effects
lmmod <- lm(maternal_mortality ~ -1 + armed_conflict + gdp_1000 + oecd + pop_dens + urban +
              age_dep + male_edu + temp + rainfall_1000 + earthquake + drought +
              iso + as.factor(year), 
            data = final_data)

plmmod <- plm(maternal_mortality ~ armed_conflict + gdp_1000 + oecd + pop_dens + urban +
              age_dep + male_edu + temp + rainfall_1000 + earthquake + drought,
              index = c("iso", "year"),
              effect = "twoways",
              model = "within",
              data = final_data)
lmmod
plmmod
#still get same coeff but he lmmmod gives coeff for each year and country
#for every 1 increase in gdp, maternal mortality increases by one  I SHOULD BE GETTING 2.97

final_data <-final_data|>
    mutate(logGDP= log(gdp_1000))

head(final_data)
preds <- as.formula(" ~ armed_conflict + logGDP + oecd + pop_dens + urban +
                  age_dep + male_edu + temp + rainfall_1000 + earthquake + drought")

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
maternal_mortalitymod
neomormod
un5mormod
infmormod

htmlreg(list(maternal_mortalitymod, neomormod, un5mormod, infmormod), file = "regression_table.html")
library(modelsummary)
armed_conflict_coeff <- modelsummary(
                        list("Maternal Mortality" = maternal_mortalitymod,
                             "Neonatal Morality" = neomormod,
                            "Under 5 Mortality" = un5mormod ,
                            "Infant Mortality" = infmormod ),
                              coef_map = c("armed_conflict" = "Armed conflict"),
                            gof_omit = ".*",
                            title = "Table 2. The association between armed conflict and maternal and child mortality (standard errors) ",
                            notes = "",
                            output = here("output", "table_2.html")
                        )

armed_conflict_coeff <- modelsummary(
                        list("Maternal Mortality" = maternal_mortalitymod,
                             "Neonatal Morality" = neomormod,
                            "Under 5 Mortality" = un5mormod ,
                            "Infant Mortality" = infmormod ),
                            title = "Table 2. The association between armed conflict and maternal and child mortality (standard errors) ",
                            notes = "",
                            output = here("output", "table_2.html")
                        )