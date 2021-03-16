input <- data.frame(matrix(nrow = 14, ncol = 10))
colnames(input) <- c("scen_no",
                     "nobs",
                     "gamma",
                     "omega_sq",
                     "tau_sq",
                     "sigma_sq",
                     "reliability",
                     "r_squared",
                     "attenuation",
                     "confounding")
input$scen_no <- 1:NROW(input)
# base setting
input[1, 2:6] <- c(500, 0, 50, 30, 100)
# reliability setting
input[2, 2:6] <- c(500, 0, 50, 25, 100)
input[3, 2:6] <- c(500, 0, 50, 20, 100)
input[4, 2:6] <- c(500, 0, 50, 15, 100)
input[5, 2:6] <- c(500, 0, 50, 10, 100)
input[6, 2:6] <- c(500, 0, 50, 5, 100)
# r-squared setting
input[7, 2:6] <- c(500, 0, 50, 30, 20)
input[8, 2:6] <- c(500, 0, 50, 30, 5)
input[9, 2:6] <- c(500, 0, 50, 30, 1)
# sample size setting
input[10, 2:6] <- c(250, 0, 50, 30, 100)
input[11, 2:6] <- c(1000, 0, 50, 30, 100)
# confounding effect
input[12, 2:6] <- c(500, 1, 50, 30, 100)
input[13, 2:6] <- c(500, 4, 50, 30, 100)
input[14, 2:6] <- c(500, 8, 50, 30, 100)
# reliability is equal to the var(X) / var(X_star)
calc_reliability <- function(gamma,
                             omega_sq,
                             tau_sq){
  reliability <-
    (gamma ^ 2 * 25 + omega_sq)/(gamma ^ 2 * 25 + omega_sq + tau_sq)
  return(reliability)
}
# r_squared is equal to 1 - var(Y|X,Z) / var(Y)
calc_r_squared <- function(sigma_sq){
  r_squared <-
    1 - sigma_sq / (4 + 1 + sigma_sq)
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
          args = list(sigma_sq = input$sigma_sq))
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
save(input,
        file = "./data/input.rda")
