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
inc_fcst <- read_csv("./Data/forecasts_cash.csv")

# Test for biasedness -----------------------------------------------------------------
inc_fcst %<>%
  mutate(ehat_feb = `Net cash income estimate` - `February forecast`) %>%
  mutate(ehat_aug = `Net cash income estimate` - `August forecast`) %>%
  mutate(ehat_nov = `Net cash income estimate` - `November forecast`) %>%
  mutate(ehat_feb1 = `Net cash income estimate` - `February(t+1) forecast`) %>%
  mutate(ehat_init = `Net cash income estimate` - `August (t + 1) "estimate"`) %>%
  mutate(dif = `Net cash income estimate` - lead(`Net cash income estimate`)) %>%
  mutate(t = `Reference Year` - 1974) %>%
  mutate(t2 = t^2)

# Holden-Peel Test ---------------------------------------------------------------------
fit <- list()                                                 # Create vector for lm() output
tile <- which(str_detect(colnames(inc_fcst),"ehat"))          # Extract column indexes with forecast error variables in the inc_fcst tibble

for (i in seq_along(tile)){
  
  index <- tile[i]                                            # Get column index of forecast variables from tile 
  fit[[i]] <- lm(inc_fcst[[index]]~1, data = inc_fcst)        # Perform intercept regression on each forecast error column. Put output into fit
  
}

fit %>%                                                       
  map(summary)                                                # Iterate through the columns of fit to summarize the output from lm()

# Output Tables -------------------------------------------------------------------------
stargazer(fit, title = "Biasedness Test",
          dep.var.labels = c("February Forecast", "August Forecast", 
                             "November Forecast", "February (t+1) Forecast", 
                             "August (t+1) Estimate"))

# Forecast Accuracy ---------------------------------------------------------------------
# Tests to be carried out:
# 1. Has the forecast accuracy improved over time?
# 2. Is there a difference in the error pattern between earlier (February (t)) vs later
#    (February (t+1) forecasts?). Are some more consistent, less biased etc.
# 3. Changes in the magnitude and variance of forecasts over time.

acc <- lm(abs(ehat_aug) ~ `Reference Year`, data = inc_fcst)
summary(acc)

inc_fcst$`August (t + 1) "estimate"`[is.na(inc_fcst$`August (t + 1) "estimate"`)] <- inc_fcst$`Net cash income estimate`[is.na(inc_fcst$`August (t + 1) "estimate"`)]

trend <- lm(`August (t + 1) "estimate"` ~ t, data = inc_fcst)
summary(trend)

inc_fcst$var <- trend$resid^2

var <- lm(var ~ `Reference Year`, data = inc_fcst)
summary(var)