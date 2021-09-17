#############################################################
## INPUT SCRIPTS USED FOR THE R PACKAGE SIMEXVSMECOR PART II (logreg)
##
## This script creates the data.frame that is available via data(input_logreg)
## lindanab4@gmail.com - 20210915
#############################################################

##############################
# 0 - Load librairies --------
##############################

##############################
# 1 - Create data.frame input_logreg-
##############################
input_logreg <- data.frame(matrix(nrow = 23, ncol = 9))
colnames(input_logreg) <- c("scen_no",
                            "nobs", # input_logreg param generate_data()
                            "gamma", # input_logreg param generate_data()
                            "tau_sq", # input_logreg param generate_data()
                            "nrep", # input_logreg param generate_data()
                            "phi", # input_logreg param generate_data()
                            "beta", # input_logreg param generate_data()
                            "reliability", # calculated from param
                            "attenuation") # calculated from param
input_logreg$scen_no <- 1:NROW(input_logreg)
# base setting
input_logreg[1, 2:7] <- c(4000, 0, 2, 2, 0.1, 0.1)
# # reliability setting
# less reliable (increasing tau_sq)
input_logreg[2, 2:7] <- c(4000, 0, 20, 2, 0.1, 0.1)
input_logreg[3, 2:7] <- c(4000, 0, 10, 2, 0.1, 0.1)
input_logreg[4, 2:7] <- c(4000, 0, 4, 2, 0.1, 0.1)
# more reliable (decreasing tau_sq)
input_logreg[5, 2:7] <- c(4000, 0, 1.5, 2, 0.1, 0.1)
input_logreg[6, 2:7] <- c(4000, 0, 1, 2, 0.1, 0.1)
input_logreg[7, 2:7] <- c(4000, 0, 0.5, 2, 0.1, 0.1)
input_logreg[8, 2:7] <- c(4000, 0, 0.25, 2, 0.1, 0.1)
input_logreg[9, 2:7] <- c(4000, 0, 0.1, 2, 0.1, 0.1)
# sample size setting
input_logreg[10, 2:7] <- c(500, 0, 2, 2, 0.1, 0.1)
input_logreg[11, 2:7] <- c(1000, 0, 2, 2, 0.1, 0.1)
input_logreg[12, 2:7] <- c(2000, 0, 2, 2, 0.1, 0.1)
input_logreg[13, 2:7] <- c(10000, 0, 2, 2, 0.1, 0.1)
# # changing number of replicates
input_logreg[14, 2:7] <- c(4000, 0, 2, 3, 0.1, 0.1)
input_logreg[15, 2:7] <- c(4000, 0, 2, 5, 0.1, 0.1)
input_logreg[16, 2:7] <- c(4000, 0, 2, 10, 0.1, 0.1)
# pseudo r-squared setting
# changing r-squared of model
input_logreg[17, 2:7] <- c(4000, 0, 2, 2, 0.06, 0.1) # Nagelkerke R2 = 0.12
input_logreg[18, 2:7] <- c(4000, 0, 2, 2, 0.08, 0.1) # Nagelkerke R2 = 0.26
input_logreg[19, 2:7] <- c(4000, 0, 2, 2, 0.2, 0.1) # Nagelkerke R2 = 0.69 (base R2 0.44)
# covariate dependency
# changing gamma
input_logreg[20, 2:7] <- c(4000, 0.01, 2, 2, 0.1, 0.1) # crude = 0.28 (R2 = 0.44)
input_logreg[21, 2:7] <- c(4000, 0.1, 2, 2, 0.1, 0.1) # crude = 0.81 (R2 = 0.41)
input_logreg[22, 2:7] <- c(4000, 0.2, 2, 2, 0.1, 0.1) # crude = 0.56 (R2 = 0.57) (base crude 0.06)
# null-effect
input_logreg[23, 2:7] <- c(4000, 0, 2, 2, 0.1, 0.1)

# reliability is equal to the var(X) / var(X_star)
calc_reliability_logreg <- function(gamma,
                             tau_sq){
  reliability <-
    (gamma ^ 2 * ((1 / 12) * (80 - 18) ^ 2) + 1) /
    (gamma ^ 2 * ((1 / 12) * (80 - 18) ^ 2) + 1 + tau_sq)
  return(reliability)
}
# attenuation is equal to var(X|Z) / var(X_star|Z)
calc_attenuation_logreg <- function(tau_sq){
  attenuation <-
    1 / (1 + tau_sq)
  return(attenuation)
}
# add to input_logreg data.frame
input_logreg$reliability <-
  do.call(calc_reliability_logreg,
          args = list(gamma = input_logreg$gamma,
                      tau_sq = input_logreg$tau_sq))
# the effect that one will find when regressing Y ~ X_star + Z is 0.2 (the
# correct effect) times the number in the column "attenuation"
input_logreg$attenuation <-
  do.call(calc_attenuation_logreg,
          args = list(tau_sq = input_logreg$tau_sq))


##############################
# 2 - Add input_logreg to package ---
##############################
usethis::use_data(input_logreg, overwrite = TRUE)
