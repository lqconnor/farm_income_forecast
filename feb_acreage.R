# Farm Income Forecast Project.
# The existence of smoothing in farm income forecasts

#Pre - Amble ------------------------------
rm(list = ls())
cat("\f")
getwd()

library(tidyverse)
library(stringr)
library(magrittr)
library(ggplot2)

# Data Import ---------------------------------
wasde <- read_csv("./Data/psd_grains_pulses.csv")
inc_fcst <- read_csv("./Data/forecasts.csv")

# Variable Gen --------------------------------
inc_fcst <- mutate(inc_fcst, aug_rev = (`August forecast` - `February forecast`)/`February forecast`,                  # Change in August update on February forecast
                   nov_rev = (`November forecast` - `August forecast`)/`August forecast`,                              # Change in November update on August forecast
                   feb_rev = (`February(t+1) forecast` - `November forecast`)/`November forecast`,                     # Change in February(t+1) update on November forecast 
                   aug_est = (`August (t + 1) "estimate"` - `February(t+1) forecast`)/`February(t+1) forecast`,        # Change in August first estimate on February(t+1) forecast
                   final_est = (`Net farm income estimate` - `February(t+1) forecast`)/`February(t+1) forecast`,       # Change in final estimate on February(t+1) forecast
                   aug_est = (`Net farm income estimate` - `August (t + 1) "estimate"`)/`August (t + 1) "estimate"`)   # Change in final estimate on February(t+1) forecast

# ----------------------------------------------------------------------------------
# Plot correlations of changes in consecutive updates. 
# Random scatter/no correlation implies a rational forecast.
# Correlation between consecutive updates implies forecasts can be improved

ggplot(data = inc_fcst, aes(x=aug_rev, y=nov_rev)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

ggplot(data = inc_fcst, aes(x=nov_rev, y=feb_rev)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

ggplot(data = inc_fcst, aes(x=feb_rev, y=aug_est)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

ggplot(data = inc_fcst, aes(x=feb_rev, y=final_est)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

ggplot(data = inc_fcst, aes(x=aug_est, y=final_est)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)


# Test for biasedness -------------------------------------------------------------
inc_fcst %<>%
  mutate(ehat_feb = `Net farm income estimate` - `February forecast`) %>%
  mutate(ehat_aug = `Net farm income estimate` - `August forecast`) %>%
  mutate(ehat_nov = `Net farm income estimate` - `November forecast`) %>%
  mutate(ehat_feb1 = `Net farm income estimate` - `February(t+1) forecast`) %>%
  mutate(ehat_init = `Net farm income estimate` - `August (t + 1) "estimate"`)

# Holden-Peel Test

fit <- lm(ehat_feb ~ 1, data = inc_fcst)
summary(fit)

fit <- lm(ehat_aug ~ 1, data = inc_fcst)
summary(fit)

fit <- lm(ehat_nov ~ 1, data = inc_fcst)
summary(fit)

fit <- lm(ehat_feb1 ~ 1, data = inc_fcst)
summary(fit)

fit <- lm(ehat_init ~ 1, data = inc_fcst)
summary(fit)