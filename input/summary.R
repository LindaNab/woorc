#############################################################
## INPUT SCRIPTS USED FOR THE R PACKAGE WOORC
##
## This script creates the data.frame that is available via data(summary)
## lindanab4@gmail.com - 20210322
#############################################################

##############################
# 0 - Load librairies --------
##############################

##############################
# 1 - Create data.frame summary_biasvar
##############################
process_output(type = "biasvsvar",
               scen_nos = 1:30)
summary_biasvsvar <- summarise_sim(1:30,
                                   use_input = input_biasvsvar,
                                   processed_dir = "./output/processed/biasvsvar_")
##############################
# 2 - Create data.frame summary_lowrel
##############################
process_output(type = "lowrel",
               scen_nos = 1:65)
summary_lowrel <- summarise_sim(1:65,
                                   use_input = input_lowrel,
                                   processed_dir = "./output/processed/lowrel_")

##############################
# 2 - Add summary to package ---
##############################
usethis::use_data(summary_biasvsvar, overwrite = TRUE)
usethis::use_data(summary_lowrel, overwrite = TRUE)
