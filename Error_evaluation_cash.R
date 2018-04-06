# Farm Income Forecast Project.
# The existence of smoothing in farm income forecasts

# Pre - Amble ------------------------------
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
inc_fcst <- read_csv("./Data/forecasts_cash.csv")

index <- which(str_detect(colnames(inc_fcst),"Net"))        # Catches the variable attached to the final estimate cash/farm income estimate
income_estimate <- inc_fcst[[index]]

# Revision Variable Gen -----------------------------------------------------------
inc_fcst <- mutate(inc_fcst, 
                   aug_rev = (`August forecast` - `February forecast`)/`February forecast`,                              # Change in August update on February forecast
                   nov_rev = (`November forecast` - `August forecast`)/`August forecast`,                                # Change in November update on August forecast
                   feb_rev = (`February(t+1) forecast` - `November forecast`)/`November forecast`,                       # Change in February(t+1) update on November forecast 
                   aug_est = (`August (t + 1) "estimate"` - `February(t+1) forecast`)/`February(t+1) forecast`,          # Change in August first estimate on February(t+1) forecast
                   final_est = (income_estimate - `August (t + 1) "estimate"`)/`August (t + 1) "estimate"`)              # Change in final estimate on August(t+1) estimate

# Test for biasedness -------------------------------------------------------------
inc_fcst %<>%
  mutate(ehat_feb = income_estimate - `February forecast`,
         ehat_aug = income_estimate - `August forecast`,
         ehat_nov = income_estimate - `November forecast`,
         ehat_feb1 = income_estimate - `February(t+1) forecast`,
         ehat_init = income_estimate - `August (t + 1) "estimate"`,
         dif = income_estimate - lead(income_estimate),
         t = `Reference Year` - 1974,
         t2 = t^2,
         t3 = t^3,
         t4 = t^4)
  

# Forecast Accuracy ----------------------------------------------------------------
# Tests to be carried out:
# 1. Has the forecast accuracy improved over time?
# 2. Is there a difference in the error pattern between earlier (February (t)) vs later
#    (February (t+1) forecasts?). Are some more consistent, less biased etc.
# 3. Changes in the magnitude and variance of forecasts over time.

# Test 1. From Isengilda et al. (2013) Does big get bigger? ------------------------
#         Forecast efficiency test. Jumpy vs smoothed.
#         The constant in this regression measures the revision bias i.e.
#         whether the revision tends to be positive or negative. 
#         The beta coefficient measures the dependence of the prior and current revision i.e.
#         whether the sign of the previous revision helps to determine the sign of the current

aug_nov <- lm(nov_rev ~ aug_rev, data = inc_fcst)
summary(aug_nov)

nov_feb1 <- lm(feb_rev ~ nov_rev, data = inc_fcst)
summary(nov_feb1)

feb1_auge <- lm(aug_est ~ feb_rev, data = inc_fcst)
summary(feb1_auge)

fin_auge <- lm(final_est ~ aug_est, data = inc_fcst)
summary(fin_auge)

# Test 2. Size dependence. i.e. does the size of the final estimate determine the direction -----
#         of the forecast bias?

# Plot trend to visually inspect best fit
ggplot(inc_fcst, aes(y = income_estimate , x = t)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = lm, formula = y ~ x + I(x^2))

# Estimate the linear trend and get predicted trend values. 
l_trnd <- lm(income_estimate ~ t, data = inc_fcst)
summary(l_trnd)

inc_fcst$yhat <- predict(l_trnd, inc_fcst)

# Produce indicator variables for large negative and positive estimates.
inc_fcst <- inc_fcst %>%
  mutate(delta = (income_estimate - yhat)/income_estimate,
         deviation = income_estimate - yhat,
         var = deviation^2,
         large = factor(delta > 0.2),
         small = factor(delta < -0.2))

size_interact <- lm(ehat_feb1 ~ large, data = inc_fcst)
summary(size_interact)

size_interact <- lm(abs(ehat_feb1) ~ var, data = inc_fcst) # The Febrary (t+1) and the February (t) forecasts have become more spread as farm income has become more spread. Others not affected. Visual inspection agrees.
summary(size_interact)

size_interact <- lm(ehat_aug ~ t, data = inc_fcst)
summary(size_interact)

# Test 3. Revision bias based on size of forecast. As in Isengilda et al. (2013) do large
#         forecasts tend to get larger?


# Time Dependence --------------------------------------------------------------------
acc <- lm(abs(ehat_aug) ~ `Reference Year`, data = inc_fcst) # This test just checks the size rather than the direction of the bias.
summary(acc)

trend <- lm(income_estimate ~ t, data = inc_fcst)
summary(trend)

inc_fcst$var <- trend$resid^2

var <- lm(var ~ `Reference Year`, data = inc_fcst)
summary(var)