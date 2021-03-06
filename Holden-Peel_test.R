# Farm Income Forecast Project.
# The existence of smoothing in farm income forecasts

# Pre - Amble -------------------------------------------------------------------------
rm(list = ls())
cat("\f")
getwd()

library(tidyverse)
library(purrr)
library(stringr)
library(magrittr)
library(ggplot2)
library(stargazer)

# Data Import -------------------------------------------------------------------------
csh_fcst <- read_csv("./Data/forecasts_cash.csv")
frm_fcst <- read_csv("./Data/forecasts_farm.csv")

###########################################################################################
# Code Generalization.
j = 10                                    # j==1 means uses cash income file. Otherwise it uses farm income file
if(j== 1){
  inc_fcst <- csh_fcst
} else{
  inc_fcst <- frm_fcst
}

inc_idx <- which(str_detect(colnames(inc_fcst),"Net"))        # Catches the variable attached to the final cash/farm income estimate
income_estimate <- inc_fcst[[inc_idx]]
############################################################################################

# Test for biasedness -----------------------------------------------------------------
inc_fcst %<>%
  mutate(ehat_feb = income_estimate - `February forecast`,
         ehat_aug = income_estimate - `August forecast`,
         ehat_nov = income_estimate - `November forecast`,
         ehat_feb1 = income_estimate - `February(t+1) forecast`,
         ehat_init = income_estimate - `August (t + 1) "estimate"`,
         dif = income_estimate - lead(income_estimate),
         t = `Reference Year` - 1974,
         t2 = t^2)

# Holden-Peel Test ---------------------------------------------------------------------
fit <- list()                                                 # Create vector for lm() output
tile <- which(str_detect(colnames(inc_fcst),"ehat"))          # Extract column indexes with forecast error variables in the inc_fcst tibble

for (i in seq_along(tile)){
  
  eht_idx <- tile[i]                                            # Get column index of forecast variables from tile 
  fit[[i]] <- lm(inc_fcst[[eht_idx]]~1, data = inc_fcst)        # Perform intercept regression on each forecast error column. Put output into fit
  
}

fit %>%                                                       
  map(summary)                                                # Iterate through the columns of fit to summarize the output from lm()

# Output Tables -------------------------------------------------------------------------
stargazer(fit, title = "Biasedness Test",
          dep.var.labels = c("February Forecast", "August Forecast", 
                             "November Forecast", "February (t+1) Forecast", 
                             "August (t+1) Estimate"))