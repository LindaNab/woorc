#############################################################
## INPUT SCRIPTS USED FOR THE R PACKAGE SIMEXVSMECOR
##
## This script creates the data.frame that is available via data(summary) and
## the data.frame that is available via data(summar_perrier)
## lindanab4@gmail.com - 20210322
#############################################################

##############################
# 0 - Load librairies --------
##############################

##############################
# 1 - Create data.frame summary
##############################
process_output(1:23)
summary <- summarise_sim(1:23,
                         use_input = input,
                         processed_dir = "./output/processed/")

##############################
# 2 - Add summary to package ---
##############################
usethis::use_data(summary, overwrite = TRUE)

##############################
# 1 - Create data.frame summary_perrier
##############################
process_output(scen_nos = 1:7, output_dir = "./output/perrier/perrier_",
               processed_dir = "./output/perrier/processed/")
summary_perrier <- summarise_sim(scen_nos = 1:7,
                                 use_input = input_perrier,
                                 processed_dir = "./output/perrier/processed/")

##############################
# 2 - Add summary_perrier to package
##############################
usethis::use_data(summary_perrier, overwrite = TRUE)
