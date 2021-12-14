#############################################################
## SIMSTUDY OF BIAS VS VARIANCE TRADEOFF
##
## This script contains the info displayed in the abstract
## lindanab4@gmail.com - 20211207
#############################################################

##############################
# 0 - Load libraries --------
##############################
library(dplyr)
data(summary_lowrel)
summary <- summary_lowrel

##############################
# 1 - Subselect settings -----
##############################
summary %>%
  filter(nobs < 150 & reliability < 0.2) %>%
  select(perc_bias) %>% pull() %>%
  quantile()

summary %>%
  filter(nobs < 150 & reliability < 0.2) %>%
  select(empse) %>% pull() %>%
  quantile()
