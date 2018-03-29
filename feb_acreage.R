# Farm Income Forecast Project.
# Testing the effect of February Farm Income Forecasts on Planted Acreage

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
                   feb_rev = (`February(t+1) forecast` - `November forecast`)/`November forecast`)

ggplot(data = inc_fcst, aes(x=aug_rev, y=nov_rev)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

ggplot(data = inc_fcst) +
  geom_point(aes(x=nov_rev, y=feb_rev)) +
  geom_smooth(aes(x=nov_rev, y=feb_rev), method = lm)