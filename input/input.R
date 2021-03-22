input <- data.frame(matrix(nrow = 18, ncol = 11))
colnames(input) <- c("scen_no",
                     "nobs",
                     "gamma",
                     "omega_sq",
                     "tau_sq",
                     "sigma_sq",
                     "beta",
                     "reliability",
                     "r_squared",
                     "attenuation",
                     "confounding")
input$scen_no <- 1:NROW(input)
# base setting
input[1, 2:7] <- c(500, 0, 50, 30, 100, 0.2)
# reliability setting
# less reliable
input[2, 2:7] <- c(500, 0, 50, 200, 100, 0.2)
input[3, 2:7] <- c(500, 0, 50, 100, 100, 0.2)
input[4, 2:7] <- c(500, 0, 50, 50, 100, 0.2)
# more reliable
input[5, 2:7] <- c(500, 0, 50, 25, 100, 0.2)
input[6, 2:7] <- c(500, 0, 50, 20, 100, 0.2)
input[7, 2:7] <- c(500, 0, 50, 15, 100, 0.2)
input[8, 2:7] <- c(500, 0, 50, 10, 100, 0.2)
input[9, 2:7] <- c(500, 0, 50, 5, 100, 0.2)
# r-squared setting
input[10, 2:7] <- c(500, 0, 50, 30, 20, 0.2)
input[11, 2:7] <- c(500, 0, 50, 30, 5, 0.2)
input[12, 2:7] <- c(500, 0, 50, 30, 1, 0.2)
# sample size setting
input[13, 2:7] <- c(125, 0, 50, 30, 100, 0.2)
input[14, 2:7] <- c(250, 0, 50, 30, 100, 0.2)
input[15, 2:7] <- c(1000, 0, 50, 30, 100, 0.2)
# confounding
input[15, 2:7] <- c(500, 1, 50, 30, 100, 0.2)
input[16, 2:7] <- c(500, 4, 50, 30, 100, 0.2)
input[17, 2:7] <- c(500, 8, 50, 30, 100, 0.2)
# null-effect
input[18, 2:7] <- c(500, 0, 50, 30, 100, 0)
# reliability is equal to the var(X) / var(X_star)
calc_reliability <- function(gamma,
                             omega_sq,
                             tau_sq){
  reliability <-
    (gamma ^ 2 * 25 + omega_sq)/(gamma ^ 2 * 25 + omega_sq + tau_sq)
  return(reliability)
}
# r_squared is equal to 1 - var(Y|X,Z) / var(Y)
calc_r_squared <- function(sigma_sq,
                           beta,
                           omega_sq){
  r_squared <-
    1 - sigma_sq / (beta ^ 2 * omega_sq + 0.2 ^ 2 * 25 + sigma_sq)
  return(r_squared)
}
# attenuation is equal to var(X|Z) / var(X_star|Z)
calc_attenuation <- function(omega_sq,
                             tau_sq){
  attenuation <-
    omega_sq / (omega_sq + tau_sq)
  return(attenuation)
}
# bias in crude model if X is included but Z not is equal to 0.2 (effect of Z on
# Y given X in outcome model) times E[Z|X], where E[Z|X] = cov(Z,X) / var(X).
# Where cov(Z,X) / var(Z) = gamma and var(Z) = 25, so cov(Z,X) / var(X) =
# gamma * 25 / (gamma ^ 2 * 25 + omega_sq)
calc_confounding <- function(gamma,
                             omega_sq){
  crude <-
    (0.2 + (0.2 * (gamma * 25 / (gamma^2 * 25 + omega_sq)))) / 0.2
  # output times 0.2 (correct effect) is equal to the crude effect
  return(crude)
}
input$reliability <-
  do.call(calc_reliability,
          args = list(gamma = input$gamma,
                      omega_sq = input$omega_sq,
                      tau_sq = input$tau_sq))
input$r_squared <-
  do.call(calc_r_squared,
          args = list(sigma_sq = input$sigma_sq,
                      beta = input$beta,
                      omega_sq = input$omega_sq))
# the effect that one will find when regressing Y ~ X_star + Z is 0.2 (the
# correct effect) times the number in the column "attenuation"
input$attenuation <-
  do.call(calc_attenuation,
          args = list(omega_sq = input$omega_sq,
                      tau_sq = input$tau_sq))
# the effect one will find when regression Y ~ X is 0.2 (the correct effect)
# times the number in the column "confounding"
input$confounding <-
  do.call(calc_confounding,
          args = list(gamma = input$gamma,
                      omega_sq = input$omega_sq))
usethis::use_data(input, overwrite = TRUE)
