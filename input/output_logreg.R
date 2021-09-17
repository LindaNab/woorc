#############################################################
## INPUT SCRIPTS USED FOR THE R PACKAGE SIMEXVSMECOR PART II (logreg)
##
## This script creates the .rds files containing the output of
## the sim study that ran using run_sim_logreg()
## lindanab4@gmail.com - 20210915
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
system.time(run_sim_logreg(arg1, arg2, arg3))

##############################
# 2 - Run sim study in R
##############################
# run_sim_logreg(2, 1, "./output/")
# for (i in 1:18){
#   run_sim(1, i)
# }
