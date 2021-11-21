#############################################################
## INPUT SCRIPTS USED FOR THE R PACKAGE WOORC
##
## This script creates the data.frame that is available via data(input)
## lindanab4@gmail.com - 20211105
#############################################################

##############################
# 0 - Load libraries --------
##############################

##############################
# 1 - Create data.frames input
##############################
# bias vs variance tradeoff
input_biasvsvar <- data.frame(matrix(nrow = 5 * 3 * 2, ncol = 6))
colnames(input_biasvsvar) <- c("scen_no",
                               "nobs", # input param generate_data()
                               "tau_sq", # input param generate_data()
                               "beta", # input param generate_data()
                               "sigma_sq", # input param generate_data()
                               "reliability") # calculated from param
input_biasvsvar$scen_no <- 1:NROW(input_biasvsvar)
input_biasvsvar$beta <- 3
nobs <- c(20, 40, 60, 80, 100)
reliability <- c(0.3, 0.6, 0.9)
rel_nobs <- expand.grid(nobs = nobs, reliability = reliability)
rel_nobs_25 <- cbind(rel_nobs, sigma_sq = 25)
rel_nobs_5 <- cbind(rel_nobs, sigma_sq = 5)
input_biasvsvar[1:15, c("nobs", "reliability", "sigma_sq")] <- rel_nobs_25[, c("nobs", "reliability", "sigma_sq")]
input_biasvsvar[16:30, c("nobs", "reliability", "sigma_sq")] <- rel_nobs_5[, c("nobs", "reliability", "sigma_sq")]
input_biasvsvar$tau_sq <- (0.25 / input_biasvsvar$reliability) - 0.25

# variance and bias for low reliability
input_lowrel <- data.frame(matrix(nrow = 13 * 5, ncol = 6))
colnames(input_lowrel) <- c("scen_no",
                            "nobs", # input param generate_data()
                            "tau_sq", # input param generate_data()
                            "beta", # input param generate_data()
                            "sigma_sq", # input param generate_data()
                            "reliability") # calculated from param
input_lowrel$scen_no <- 1:NROW(input_lowrel)
input_lowrel$beta <- 3
nobs <- c(300, 150, 600, 50, 25)
reliability <-
  c(0.99, 0.95, seq(from = 0.9, to = 0.1, by = -0.1), 0.05, 0.01)
rel_nobs <- expand.grid(reliability = reliability, nobs = nobs)
input_lowrel[, c("nobs", "reliability")] <-
  rel_nobs[, c("nobs", "reliability")]
input_lowrel$tau_sq <- (0.25 / input_lowrel$reliability) - 0.25
input_lowrel$sigma_sq <- 5

##############################
# 2 - Add input to package ---
##############################
usethis::use_data(input_lowrel, overwrite = TRUE)
usethis::use_data(input_biasvsvar, overwrite = TRUE)
