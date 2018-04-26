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
library(Hmisc)

# Data Import ---------------------------------
#wasde <- read_csv("./Data/psd_grains_pulses.csv")
csh_fcst <- read_csv("./Data/forecasts_cash.csv")
frm_fcst <- read_csv("./Data/forecasts_projection_farm.csv")
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

catcher <- str_c("Net ",fl_source," income$")
incy <- filter(feb_18, Year <=2016 & Year >= 1975,
               State == "US",
               str_detect(VariableDescriptionTotal, catcher)) %>%
  mutate(income_estimate = round(Amount/1000000, digits = 2)) %>%
  select(Year, income_estimate)
inc_fcst <- left_join(inc_fcst, incy, by = c("Reference Year" = "Year"))

catcher <- str_c("Net ",fl_source," income$")
incy <- filter(feb_18, Year <=2016 & Year >= 1975,
               State == "OH",
               str_detect(VariableDescriptionTotal, catcher)) %>%
  mutate(ohio_estimate = round(Amount/1000000, digits = 2)) %>%
  select(Year, ohio_estimate)
inc_fcst <- left_join(inc_fcst, incy, by = c("Reference Year" = "Year"))

inc_fcst <- mutate(inc_fcst, t = `Reference Year` - 1974) %>%
  arrange(`Reference Year`)

inc_fcst$ohio <- 0



oh_est <- inc_fcst$ohio_estimate[inc_fcst$`Reference Year` <= 2016 & inc_fcst$`Reference Year` >= 2016-15]

aug_est <- inc_fcst$`August (t + 1) "estimate"`[inc_fcst$`Reference Year` <= 2016 & inc_fcst$`Reference Year` >= 2016-15]

feb_fct <- inc_fcst$`February forecast`[inc_fcst$`Reference Year` <= 2016 & inc_fcst$`Reference Year` >= 2016-15]

feb1_fct <- inc_fcst$`February(t+1) forecast`[inc_fcst$`Reference Year` <= 2016 & inc_fcst$`Reference Year` >= 2016-15]

yr <- inc_fcst$t[inc_fcst$`Reference Year` <= 2016 & inc_fcst$`Reference Year` >= 2016-15]

trend <- lm(log(oh_est) ~ log(feb_fct) + log(lag(feb_fct)) + lag(log(aug_est),2), data = inc_fcst)
summary(trend)

# Five Year rolling forecast
j = 5
for (i in 1990:2016) {
  oh_est <- inc_fcst$ohio_estimate[inc_fcst$`Reference Year` <= i & inc_fcst$`Reference Year` >= i-j]
  
  aug_est <- inc_fcst$`August (t + 1) "estimate"`[inc_fcst$`Reference Year` <= i & inc_fcst$`Reference Year` >= i-j]
  
  feb_fct <- inc_fcst$`February forecast`[inc_fcst$`Reference Year` <= i & inc_fcst$`Reference Year` >= i-j]
  
  yr <- inc_fcst$t[inc_fcst$`Reference Year` <= i & inc_fcst$`Reference Year` >= i-j]
  
  trend <- lm(log(oh_est) ~ log(feb_fct) + lag(log(aug_est),2), data = inc_fcst)
  #trend <- lm(oh_est ~ feb_fct + lag(oh_est) + lag(aug_est), data = inc_fcst)
  summary(trend)
  
  inc_fcst$ohio[inc_fcst$`Reference Year`== i] <- summary(trend)$coefficients[1,1] + summary(trend)$coefficients[2,1]*log(inc_fcst$`February forecast`[inc_fcst$`Reference Year` == i]) +
    summary(trend)$coefficients[3,1]*log(inc_fcst$`August (t + 1) "estimate"`[inc_fcst$`Reference Year` == i-2])
    #summary(trend)$coefficients[3,1]*inc_fcst$`ohio_estimate`[inc_fcst$`Reference Year` == i-1]
}

inc_fcst$ohio[inc_fcst$`Reference Year`== 2017] <- summary(trend)$coefficients[1,1] + summary(trend)$coefficients[2,1]*log(inc_fcst$`February forecast`[inc_fcst$`Reference Year` == 2017]) +
  summary(trend)$coefficients[3,1]*log(inc_fcst$`August (t + 1) "estimate"`[inc_fcst$`Reference Year` == 2015])

inc_fcst$ohio[inc_fcst$`Reference Year`== 2018] <- summary(trend)$coefficients[1,1] + summary(trend)$coefficients[2,1]*log(inc_fcst$`February forecast`[inc_fcst$`Reference Year` == 2018]) +
  summary(trend)$coefficients[3,1]*log(inc_fcst$`August (t + 1) "estimate"`[inc_fcst$`Reference Year` == 2016])

inc_fcst <- mutate(inc_fcst, ohio_f = exp(ohio))
inc_fcst <- filter(inc_fcst, `Reference Year` > 1990 & `Reference Year` <= 2018)


ggplot(data = inc_fcst) +
  geom_line(aes(x = `Reference Year`, y = ohio_estimate)) +
  geom_line(aes(x = `Reference Year`, y = ohio_f), color = "red")
