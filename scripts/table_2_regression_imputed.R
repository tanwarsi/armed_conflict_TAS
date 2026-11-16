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
fit.mi.matmor
##### Fit ..... morality regression model on MI data set:
##### Fit ..... morality regression model on MI data set:
##### Fit ..... morality regression model on MI data set:
##### Fit ..... morality regression model on MI data set:
##### Use pool()   to summarize the regression estimates and standard errors
out.mi.matmor <- pool(fit.mi.matmor)
summary(out.mi.matmor) #summary() function will produce a table with estimates, standard errors, etc.


