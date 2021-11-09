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
# 1 - Create data.frame input-
##############################
input <- data.frame(matrix(nrow = 4 * 12, ncol = 5))
colnames(input) <- c("scen_no",
                     "nobs", # input param generate_data()
                     "tau_sq", # input param generate_data()
                     "beta", # input param generate_data()
                     "reliability") # calculated from param
input$scen_no <- 1:NROW(input)
input$beta <- 3
nobs <- c(300, 150, 600, 50)
reliability <- c(0.99, 0.95, seq(from = 0.8, to = 0.1, by = -0.1), 0.05, 0.01)
rel_nobs <- expand.grid(reliability = reliability, nobs = nobs)
input[, c("nobs", "reliability")] <- rel_nobs[, c("nobs", "reliability")]
input$tau_sq <- (0.25 / input$reliability) - 0.25

##############################
# 2 - Add input to package ---
##############################
usethis::use_data(input, overwrite = TRUE)
