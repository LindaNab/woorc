library(simexvsmecor)
args <- commandArgs(trailingOnly = TRUE)
arg1 <- as.numeric(args[1])
arg2 <- as.numeric(args[2])
arg3 <- args[3]

system.time(run_sim(arg1, arg2, arg3))
# run_sim(2, 1)
# print(paste0("arg1: ", arg1, ", arg2: ", arg2, ", arg3: ", arg3))
