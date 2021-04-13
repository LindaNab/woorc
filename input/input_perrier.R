#############################################################
## INPUT SCRIPTS USED FOR THE R PACKAGE SIMEXVSMECOR
##
## This script creates the data.frame that is available via data(input_perrier)
## lindanab4@gmail.com - 20210412
#############################################################

##############################
# 0 - Load librairies --------
##############################

##############################
# 1 - Create data.frame input_perrier
##############################
input_perrier <- data.frame(matrix(nrow = 9, ncol = 5))
colnames(input_perrier) <- c("scen_no",
                             "nobs", # input param generate_data_perrier()
                             "nrep", # input param generate_data_perrier()
                             "tau_sq", # input param generate_data_perrier()
                             "beta") # estimand
input_perrier$scen_no <- 1:NROW(input_perrier)
input_perrier$beta <- -100
# base setting
input_perrier[1, 2:4] <- c(3000, 2, 2/3)
input_perrier[2, 2:4] <- c(3000, 3, 2/3)
input_perrier[3, 2:4] <- c(3000, 4, 2/3)
input_perrier[4, 2:4] <- c(3000, 8, 2/3)
input_perrier[5, 2:4] <- c(3000, 10, 2/3)
input_perrier[6, 2:4] <- c(3000, 15, 2/3)
input_perrier[7, 2:4] <- c(3000, 20, 2/3)
input_perrier[8, 2:4] <- c(3000, 40, 2/3)
input_perrier[9, 2:4] <- c(3000, 50, 2/3)


##############################
# 2 - Add input_perrier to package
##############################
usethis::use_data(input_perrier, overwrite = TRUE)
