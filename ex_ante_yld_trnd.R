# Farm Income Forecast Project.
# The existence of smoothing in farm income forecasts

# # Pre - Amble ------------------------------
# rm(list = ls())
# cat("\f")
# getwd()
#
# library(tidyverse)
# library(purrr)
# library(stringr)
# library(magrittr)
# library(ggplot2)
# library(stargazer)
# library(strucchange)

# # Data Import ---------------------------------
# #wasde <- read_csv("./Data/psd_grains_pulses.csv")
# csh_fcst <- read_csv("./Data/forecasts_cash.csv")
# frm_fcst <- read_csv("./Data/forecasts_farm.csv")
#
# # Code Generalization.
# j = 1                                   # j==1 means uses cash income file. Otherwise it uses farm income file
# if(j== 1){
#   inc_fcst <- csh_fcst
# } else{
#   inc_fcst <- frm_fcst
# }

# Expected outcomes for February (t) forecast given information available at February (t)
indx <- which(str_detect(colnames(inc_fcst),"Net")) 
inc_fcst$trend_feb <- 0
inc_fcst$trend_aug <- 0

for (i in 1976:2016) {
  
inc_fcst$new = inc_fcst[[indx]]                                                                                          # Set new column to equal the final USDA income estimates
inc_fcst$new[inc_fcst$`Reference Year`== i] <- inc_fcst$`February(t+1) forecast`[inc_fcst$`Reference Year`== i]          # Set the current year equal to the previous February(t+1) forecast
inc_fcst$new[inc_fcst$`Reference Year`== i-1] <- inc_fcst$`August (t + 1) "estimate"`[inc_fcst$`Reference Year`== i-1]   # Replace the previous estimate with August(t+1) estimate of prior year
t <- inc_fcst$`Reference Year`[inc_fcst$`Reference Year`<= i]                                                            # Make object with only the values of time up to the current year
inc_est <- inc_fcst$new[inc_fcst$`Reference Year`<= i]                                                                   # Make object with estimates up to the current year
fit <- lm(inc_est ~ t)
summary(fit)
inc_fcst$trend_feb[inc_fcst$`Reference Year`== i] <- summary(fit)$coefficients[1,1] + summary(fit)$coefficients[2,1]*i   # Replace value of feb_trend with predicted value in the current year

}

inc_fcst$trend_feb[inc_fcst$`Reference Year`== 1975] <- inc_fcst[[indx]][inc_fcst$`Reference Year`==1975]


# Expected outcomes for August and November (t) forecast given information available atAugust (t)
indx <- which(str_detect(colnames(inc_fcst),"Net")) 
inc_fcst$trend <- 0

for (i in 1976:2016) {
  
  inc_fcst$new = inc_fcst[[indx]]
  inc_fcst$new[inc_fcst$`Reference Year`== i] <- inc_fcst$`August (t + 1) "estimate"`[inc_fcst$`Reference Year`== i]
  t <- inc_fcst$`Reference Year`[inc_fcst$`Reference Year`<= i]
  inc_est <- inc_fcst$new[inc_fcst$`Reference Year`<= i]
  fit <- lm(inc_est ~ t)
  summary(fit)
  inc_fcst$trend_aug[inc_fcst$`Reference Year`== i] <- summary(fit)$coefficients[1,1] + summary(fit)$coefficients[2,1]*i
  
}

inc_fcst$trend_aug[inc_fcst$`Reference Year`== 1975] <- inc_fcst[[indx]][inc_fcst$`Reference Year`==1975]