#############################################################
## INPUT SCRIPTS USED FOR THE R PACKAGE SIMEXVSMECOR
##
## This script creates the data.frame that is available via data(summary)
## lindanab4@gmail.com - 20210322
#############################################################

##############################
# 0 - Load librairies --------
##############################

##############################
# 1 - Create data.frame summary
##############################
process_output(1:22)
summary <- summarise_sim(1:22)

##############################
# 2 - Add summary to package ---
##############################
usethis::use_data(summary, overwrite = TRUE)
