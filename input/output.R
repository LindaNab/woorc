#############################################################
## INPUT SCRIPTS USED FOR THE R PACKAGE WOORC
##
## This script creates the .rds files containing the output of
## the sim study that ran using run_sim()
## lindanab4@gmail.com - 20210322
#############################################################

##############################
# 0 - Load librairies --------
##############################
library(woorc)

##############################
# 1 - Run sim study using bash
##############################
args <- commandArgs(trailingOnly = TRUE)
arg1 <- as.numeric(args[1]) # nrep
arg2 <- as.character(args[2]) # type (biasvsvar or lowrel)
arg3 <- as.numeric(args[3]) # scen_no
arg4 <- args[4] # location of output directory where .rds files should be saved
# print(paste0("arg1: ", arg1, ", arg2: ", arg2, ", arg3: ", arg3, ", arg4: ", arg4))
system.time(run_sim(arg1, arg2, arg3, arg4))

##############################
# 2 - Run sim study in R
##############################
# run_sim(2, 1)
# for (i in 1:18){
#   run_sim(1, i)
# }
