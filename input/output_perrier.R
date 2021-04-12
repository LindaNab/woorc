#############################################################
## INPUT SCRIPTS USED FOR THE R PACKAGE SIMEXVSMECOR
##
## This script creates the .rds files containing the output of
## the sim study that ran using run_sim() of the Perrier repilication
## lindanab4@gmail.com - 20210412
#############################################################

##############################
# 0 - Load librairies --------
##############################
library(simexvsmecor)

##############################
# 1 - Run sim study using bash
##############################
args <- commandArgs(trailingOnly = TRUE)
arg1 <- as.numeric(args[1]) # nrep
arg2 <- as.numeric(args[2]) # scen_no
arg3 <- args[3] # location of output directory where .rds files should be saved
# print(paste0("arg1: ", arg1, ", arg2: ", arg2, ", arg3: ", arg3))
system.time(run_sim_perrier(arg1, arg2, arg3))

##############################
# 2 - Run sim study in R
##############################
# run_sim_perrier(nrep = 2, scen_no = 1)
# for (i in 1:9){
#   run_sim_perrier(nrep = 2, scen_no = i)
# }
