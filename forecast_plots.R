# Farm Income Forecast Project.
# The existence of smoothing in farm income forecasts

#Pre - Amble ------------------------------
rm(list = ls())
cat("\f")
getwd()

library(tidyverse)
library(purrr)
library(stringr)
library(magrittr)
library(ggplot2)
library(stargazer)

# Data Import ---------------------------------
#wasde <- read_csv("./Data/psd_grains_pulses.csv")
#inc_fcst <- read_csv("./Data/forecasts.csv")
inc_fcst <- read_csv("./Data/forecasts_cash.csv")

####################################################################################################################
# Code Generalization. Catch file unique identifiers -------------------------------------------------------------
index <- which(str_detect(colnames(inc_fcst),"Net"))                                   # Catches the variable attached to the final cash/farm income estimate
income_estimate <- inc_fcst[[index]]

fl_source <- names(inc_fcst)                                                           # Catch file source descriptor
fl_source <- fl_source[grepl("Net", fl_source)]
fl_source <- sub("^[[:alpha:]]+[[:blank:]]([a-z]+)[[:blank:]].+$","\\1", fl_source)    # Regular Expression to extract cash or farm portion
####################################################################################################################

# Variable Gen --------------------------------
inc_fcst <- mutate(inc_fcst, aug_rev = (`August forecast` - `February forecast`)/`February forecast`,                    # Change in August update on February forecast
                   nov_rev = (`November forecast` - `August forecast`)/`August forecast`,                                # Change in November update on August forecast
                   feb_rev = (`February(t+1) forecast` - `November forecast`)/`November forecast`,                       # Change in February(t+1) update on November forecast 
                   aug_est = (`August (t + 1) "estimate"` - `February(t+1) forecast`)/`February(t+1) forecast`,          # Change in August first estimate on February(t+1) forecast
                   #final_est = (`Net farm income estimate` - `February(t+1) forecast`)/`February(t+1) forecast`,        # Change in final estimate on February(t+1) forecast
                   final_est = (income_estimate - `August (t + 1) "estimate"`)/`August (t + 1) "estimate"`)              # Change in final estimate on August(t+1) estimate


# Plot correlations of changes in consecutive updates. 
# Random scatter/no correlation implies a rational forecast.
# Correlation between consecutive updates implies forecasts can be improved

ggplot(data = inc_fcst, aes(x=aug_rev, y=nov_rev)) +
  geom_point() +
  geom_smooth(method = lm, se = TRUE) +
  labs(x = "Change at August Forecast", y = "Change at November Forecast")
ggsave(str_c("Plots/plot1_",fl_source,".jpg"))                             # Attach the file identifier (fl_source) to the plot file name

ggplot(data = inc_fcst, aes(x=nov_rev, y=feb_rev)) +
  geom_point() +
  geom_smooth(method = lm, se = TRUE) +
  labs(x = "Change at November Forecast", y = "Change at February (t+1) Forecast")
ggsave(str_c("Plots/plot2_",fl_source,".jpg"))

ggplot(data = inc_fcst, aes(x=feb_rev, y=aug_est)) +
  geom_point() +
  geom_smooth(method = lm, se = TRUE) +
  labs(x = "Change at February (t+1) Forecast", y = "Change at August Estimate")
ggsave(str_c("Plots/plot3_",fl_source,".jpg"))

ggplot(data = inc_fcst, aes(x=feb_rev, y=final_est)) +
  geom_point() +
  geom_smooth(method = lm, se = TRUE) +
  labs(x = "Change at February (t+1) Forecast", y = "Change at Final Estimate")
ggsave(str_c("Plots/plot4_",fl_source,".jpg"))

ggplot(data = inc_fcst, aes(x=aug_est, y=final_est)) +
  geom_point() +
  geom_smooth(method = lm, se = TRUE) +
  labs(x = "Change at August Estimate", y = "Change at Final Estimate")
ggsave(str_c("Plots/plot5_",fl_source,".jpg"))

