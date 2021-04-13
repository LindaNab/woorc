#############################################################
## ANALYSIS SCRIPTS USED FOR THE SCIENTIFIC RESEARCH
##
## This script contains code that explores the output of the sim
## study summarised in data(summary_perrier)
## lindanab4@gmail.com - 20210322
#############################################################

##############################
# 0 - Load librairies --------
##############################
library(dplyr)
data(summary_perrier) # summary of sim study

summary_perrier %>% select(method, nrep,perc_bias, bias, bias_mcse)
