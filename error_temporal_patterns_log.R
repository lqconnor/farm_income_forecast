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
library(strucchange)

# Data Import ---------------------------------
#wasde <- read_csv("./Data/psd_grains_pulses.csv")
csh_fcst <- read_csv("./Data/forecasts_cash.csv")
frm_fcst <- read_csv("./Data/forecasts_farm.csv")
feb_18 <- read_csv("./Data/farmincome_wealthstatisticsdata_february2018.csv")


###########################################################################################
# Code Generalization.
j = 11                                   # j==1 means uses cash income file. Otherwise it uses farm income file
if(j== 1){
  inc_fcst <- csh_fcst
} else{
  inc_fcst <- frm_fcst
}

# Catch file source descriptor
fl_source <- names(inc_fcst)                                                           
fl_source <- fl_source[grepl("Net", fl_source)]
fl_source <- sub("^[[:alpha:]]+[[:blank:]]([a-z]+)[[:blank:]].+$","\\1", fl_source)    # Regular Expression to extract cash or farm portion of string

source("ex_ante_yld_trnd.R")                           # Adds the one step ahead trend projection for ex-ante prediction anaysis
index <- which(str_detect(colnames(inc_fcst),"Net"))                                   # Catches the variable attached to the final cash/farm income estimate
inc_fcst <- mutate(inc_fcst, income_estimate = inc_fcst[[index]])
inc_fcst$trend_feb <- log(inc_fcst$trend_feb)
inc_fcst$trend_aug <- log(inc_fcst$trend_aug)
inc_fcst <- select(inc_fcst,-income_estimate)

cathcer <- str_c("Net ",fl_source," income$")
incy <- filter(feb_18, Year <=2016 & Year >= 1975,
               State == "US",
               str_detect(VariableDescriptionTotal, cathcer)) %>%
  mutate(income_estimate = round(Amount/1000000, digits = 2)) %>%
  select(Year, income_estimate)
inc_fcst <- left_join(inc_fcst, incy, by = c("Reference Year" = "Year"))


#inc_fcst$trend_feb <- income_estimate
#inc_fcst$trend_aug <- income_estimate
############################################################################################

# Revision Variable Gen -----------------------------------------------------------
inc_fcst <- mutate(inc_fcst, 
                   aug_rev = (`August forecast` - `February forecast`)/`February forecast`,                              # Change in August update on February forecast
                   nov_rev = (`November forecast` - `August forecast`)/`August forecast`,                                # Change in November update on August forecast
                   feb_rev = (`February(t+1) forecast` - `November forecast`)/`November forecast`,                       # Change in February(t+1) update on November forecast 
                   aug1_rev = (`August (t + 1) "estimate"` - `February(t+1) forecast`)/`February(t+1) forecast`,         # Change in August first estimate on February(t+1) forecast
                   final_rev = (income_estimate - `August (t + 1) "estimate"`)/`August (t + 1) "estimate"`)              # Change in final estimate on August(t+1) estimate

# Generate Forecast Error -------------------------------------------------------------
inc_fcst %<>%
  mutate(ehat_feb = log(income_estimate) - log(`February forecast`),
         ehat_aug = log(income_estimate) - log(`August forecast`),
         ehat_nov = log(income_estimate) - log(`November forecast`),
         ehat_feb1 = log(income_estimate) - log(`February(t+1) forecast`),
         ehat_init = log(income_estimate) - log(`August (t + 1) "estimate"`),
         dif = log(income_estimate) - lead(log(income_estimate)),
         t = `Reference Year` - 1974,
         t2 = t^2,
         t3 = t^3,
         t4 = t^4)
  
summary(abs(inc_fcst$aug_rev))
summary(abs(inc_fcst$nov_rev))
summary(abs(inc_fcst$feb_rev))
summary(abs(inc_fcst$aug1_rev))
# Forecast Accuracy ----------------------------------------------------------------

#############################################################################################################
#Remember to comment out when running the farm income file. Include when running cash income file
inc_fcst$income_estimate <- log(inc_fcst$income_estimate)
#inc_fcst$`August (t + 1) "estimate"`[is.na(inc_fcst$`August (t + 1) "estimate"`)] <- log(inc_fcst$income_estimate[is.na(inc_fcst$`August (t + 1) "estimate"`)])

#############################################################################################################
# Plot trend to visually inspect best fit

ggplot(inc_fcst, aes(y = income_estimate , x = `Reference Year`)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = lm, formula = y ~ x)


# Comparison Plots
ggplot(inc_fcst, aes(y = income_estimate , x = `Reference Year`)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = lm, formula = y ~ x) +
  #geom_line(aes(y = log(`February forecast`) , x = `Reference Year`), color = "red") 
  geom_line(aes(y = log(`August forecast`) , x = `Reference Year`), color = "blue")

#############################################################################################################
# Estimate the linear trend and get predicted trend values. 
l_trnd <- lm(income_estimate ~ t, data = inc_fcst)
summary(l_trnd)

inc_fcst$yhat <- predict(l_trnd, inc_fcst)
#inc_fcst$var <- l_trnd$resid^2

trnd <- lm(income_estimate ~ t, data = inc_fcst)
summary(trnd)
inc_fcst$yhat_actual <- predict(trnd, inc_fcst)
# Produce indicator variables for large negative and positive estimates.
inc_fcst <- inc_fcst %>%
  mutate(delta = income_estimate - yhat,
         deviation = income_estimate - yhat_actual,
         var = deviation^2,
         large = factor(delta > 0.1),
         small = factor(delta < -0.1))
#############################################################################################################
# Behavior of Variance of Income over time
vrc <- lm(var ~ `Reference Year`, data = inc_fcst)
summary(vrc)

#############################################################################################################
# Structural Breaks in Farm Income estimates
inc_fcst <- inc_fcst %>%
  arrange(`Reference Year`)

index <- which(str_detect(colnames(inc_fcst),"Net"))                                   # Catches the variable attached to the final cash/farm income estimate
income_estimator <- inc_fcst[[index]]

incomes <- ts(income_estimator, start = c(1975, 1), end = c(2016, 1), frequency = 1)

bp <- breakpoints(incomes ~ 1, data = inc_fcst)
summary(bp)

#plot(bp)
plot(incomes)
lines(bp)

ci_incomes <- confint(bp)
lines(ci_incomes)

#############################################################################################################

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

###########################################################################################
# Time Dependence of Forecast Error Bias
fit <- list()                                                 # Create vector for lm() output
tile <- which(str_detect(colnames(inc_fcst),"ehat"))          # Extract column indexes with forecast error variables in the inc_fcst tibble

for (i in seq_along(tile)){
  
  eht_idx <- tile[i]                                            # Get column index of forecast variables from tile 
  fit[[i]] <- lm(inc_fcst[[eht_idx]]~ t, data = inc_fcst)        # Perform intercept regression on each forecast error column. Put output into fit
  
}

fit %>%                                                       
  map(summary)                                                # Iterate through the columns of fit to summarize the output from lm()

# Output Tables -------------------------------------------------------------------------
stargazer(fit, title = "Time Dependence of Forecast Error Bias",
          dep.var.labels = c("February Forecast", "August Forecast", 
                             "November Forecast", "February (t+1) Forecast", 
                             "August (t+1) Estimate"), type = 'text')

#fit <- lm(final_rev ~ aug1_rev + delta, data = inc_fcst)
#summary(fit)
###########################################################################################
# Time Dependence of Forecast Error Size
fit <- list()                                                 # Create vector for lm() output

for (i in seq_along(tile)){
  
  eht_idx <- tile[i]                                            # Get column index of forecast variables from tile 
  fit[[i]] <- lm(abs(inc_fcst[[eht_idx]]) ~ t, data = inc_fcst)        # Perform intercept regression on each forecast error column. Put output into fit
  
}

fit %>%                                                       
  map(summary)                                                # Iterate through the columns of fit to summarize the output from lm()

# Output Tables -------------------------------------------------------------------------
stargazer(fit, title = "Time Dependence of Forecast Error Size",
          dep.var.labels = c("February Forecast", "August Forecast", 
                             "November Forecast", "February (t+1) Forecast", 
                             "August (t+1) Estimate"), type = 'text')


###########################################################################################
# Dependence of Bias on Deviation Size
fit <- list()                                                 # Create vector for lm() output

for (i in seq_along(tile)){
  
  eht_idx <- tile[i]                                            # Get column index of forecast variables from tile 
  fit[[i]] <- lm(inc_fcst[[eht_idx]] ~ large + small, data = inc_fcst)        # Perform intercept regression on each forecast error column. Put output into fit
  
}

fit %>%                                                       
  map(summary)                                                # Iterate through the columns of fit to summarize the output from lm()

# Output Tables -------------------------------------------------------------------------
stargazer(fit, title = "Dependence of Bias on Deviation Size",
          dep.var.labels = c("February Forecast", "August Forecast", 
                             "November Forecast", "February (t+1) Forecast", 
                             "August (t+1) Estimate"), type = 'text')


###########################################################################################
# Dependence of Asbolute Mean Error on Deviation Size 
fit <- list()                                                 # Create vector for lm() output

for (i in seq_along(tile)){
  
  eht_idx <- tile[i]                                            # Get column index of forecast variables from tile 
  fit[[i]] <- lm(abs(inc_fcst[[eht_idx]]) ~ large + small, data = inc_fcst)        # Perform intercept regression on each forecast error column. Put output into fit
  
}

fit %>%                                                       
  map(summary)                                                # Iterate through the columns of fit to summarize the output from lm()

# Output Tables -------------------------------------------------------------------------
stargazer(fit, title = "Mean Error Dependence on Deviation Size",
          dep.var.labels = c("February Forecast", "August Forecast", 
                             "November Forecast", "February (t+1) Forecast", 
                             "August (t+1) Estimate"), type = 'text')


###########################################################################################
cathcer <- str_c("Net ",fl_source," income$")
incy <- filter(feb_18, Year <=2016 & Year >= 1971,
               State == "US",
               str_detect(VariableDescriptionTotal, cathcer)) %>%
  mutate(income_estimate =log(round(Amount/1000000, digits = 2)))
  

inc_fcst %<>%
  mutate(rl_mn = rollmean(incy$income_estimate, 5),
         rl_vr = rollapply(incy$income_estimate, 5, sd)) %>%
  filter(`Reference Year` > 1980)

# Variance Dependence of Bias
fit <- list()                                                 # Create vector for lm() output

for (i in seq_along(tile)){
  
  eht_idx <- tile[i]                                            # Get column index of forecast variables from tile 
  fit[[i]] <- lm(inc_fcst[[eht_idx]]~ rl_vr, data = inc_fcst)        # Perform intercept regression on each forecast error column. Put output into fit
  
}

fit %>%                                                       
  map(summary)                                                # Iterate through the columns of fit to summarize the output from lm()

# Output Tables -------------------------------------------------------------------------
stargazer(fit, title = "Variance Dependence of Bias",
          dep.var.labels = c("February Forecast", "August Forecast", 
                             "November Forecast", "February (t+1) Forecast", 
                             "August (t+1) Estimate"), type = 'text')

###########################################################################################
# Variance Dependence of Mean Absolute Forecast Error
fit <- list()                                                 # Create vector for lm() output

for (i in seq_along(tile)){
  
  eht_idx <- tile[i]                                            # Get column index of forecast variables from tile 
  fit[[i]] <- lm(abs(inc_fcst[[eht_idx]])~ rl_vr, data = inc_fcst)        # Perform intercept regression on each forecast error column. Put output into fit
  
}

fit %>%                                                       
  map(summary)                                                # Iterate through the columns of fit to summarize the output from lm()

# Output Tables -------------------------------------------------------------------------
stargazer(fit, title = "Variance Dependence of Mean Absolute Forecast Error",
          dep.var.labels = c("February Forecast", "August Forecast", 
                             "November Forecast", "February (t+1) Forecast", 
                             "August (t+1) Estimate"), type = 'text')

fit <- lm(rl_vr ~  t, data = inc_fcst)
summary(fit)
stargazer(fit, type = 'text')


