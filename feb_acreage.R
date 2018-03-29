# Farm Income Forecast Project.
# The existence of smoothing in farm income forecasts

#Pre - Amble ------------------------------
rm(list = ls())
cat("\f")
getwd()

library(tidyverse)
library(stringr)
library(magrittr)
library(lubridate)
library(ggplot2)

# Data Import ------------------------------
wasde <- read_csv("./Data/psd_grains_pulses.csv")
inc_fcst <- read_csv("./Data/forecasts.csv")

# Variable Gen --------------------------------
inc_fcst <- mutate(inc_fcst, aug_rev = (`August forecast` - `February forecast`)/`February forecast`,
                   nov_rev = (`November forecast` - `August forecast`)/`August forecast`,
                   feb_rev = (`February(t+1) forecast` - `November forecast`)/`November forecast`,
                   aug_est = (`August (t + 1) "estimate"` - `February(t+1) forecast`)/`February(t+1) forecast`,
                   final_est = (`Net farm income estimate` - `February(t+1) forecast`)/`February(t+1) forecast`)


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


# Test for biasedness
fit <- lm(`Net farm income estimate` ~ `February forecast`, data = inc_fcst)
summary(fit)