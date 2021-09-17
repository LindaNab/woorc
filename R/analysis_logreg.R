#' Perform the uncorrected analysis PART II (logreg)
#'
#' @param data data to be used
#' @return numeric vector with the coefficient of the uncorrected analysis, the
#' standard error of the estimated coefficient and the confidence interval
perform_uncor_logreg <- function(data) {
  uncor_fit <- glm(Y ~ X_star_1 + Z1,
                   family = binomial(link = "logit"),
                   data = data)
  effect <- coef(uncor_fit)[2]
  se <- summary(uncor_fit)$coefficients[2, 2]
  ci <- effect + c(-1, 1) * qnorm(0.975) * se
  names(ci) <- c("lower", "upper")
  return(c(
    effect = unname(effect),
    se = se,
    ci = ci
  ))
}
#' Perform measurement error correction by means of regcal
#'
#' @param data data to be used
#' @return numeric vector with the coefficient of the corrected analysis, the
#' standard error of the estimated coefficient and the confidence interval based
#' on the bootstrap
perform_mecor_logreg <- function(data){
  cols_no_reps <- grep("X_star", colnames(data))
  var <- mean(apply(data[, cols_no_reps],
                    1,
                    var)) # estimate meas error var
  coef <- regcal_logreg(data, var)
  # bootstrap
  coefs_boot <- numeric(999)
  for (i in 1:999){
    rownum <- 1:nrow(data)
    rownum_new <- sample(rownum, nrow(data), replace = TRUE)
    data_new <- data[rownum_new, ]
    coefs_boot[i] <- regcal_logreg(data_new, var)[2]
  }
  se <- sd(coefs_boot)
  ci <- quantile(coefs_boot, probs = c(0.025, 0.975))
  names(ci) <- c("lower", "upper")

  return(c(
    effect = coef[2],
    se = se,
    ci = ci
  ))
}
#' Perform measurement error correction by means of regcal for logreg
#'
#' @param data data to be used
#' @param var assumed variance of measurement error
#' @return numeric vector with the coefficient of the corrected analysis
regcal_logreg <- function(data, var){
  uncor_fit <- glm(Y ~ X_star_1 + Z1,
                   family = binomial(link = "logit"),
                   data = data)
  beta_star <- coef(uncor_fit)
  beta_star[1:2] <- rev(coef(uncor_fit)[1:2])
  q <- with(data, cbind(X_star_1, Z1))
  Q <- scale(q, scale = F)
  matrix <- t(Q) %*% Q / (length(data$X_star_1) - 1)
  matrix1 <- matrix
  matrix1[1, 1] <- matrix1[1, 1] - var
  model_matrix <- solve(matrix1) %*% matrix
  n_model_matrix <- nrow(model_matrix)
  lambda1 <- 1 / model_matrix[1, 1]
  lambda2 <- model_matrix[2:n_model_matrix, 1] * - lambda1
  lambda0 <- mean(data$X_star_1) - lambda1 * mean(data$X_star_1) - t(lambda2) %*% colMeans(cbind(data$Z1))
  coef <- c(lambda1, lambda0, lambda2)
  model_matrix <- diag(3)
  model_matrix[1, 1:3] <- coef
  model_matrix
  beta <- beta_star %*% solve(model_matrix)
  beta[1:2] <- rev(beta[1:2])
  return(beta)
}
#' Perform measurement error correction by means of simex
#'
#' @param data data to be used
#' @return numeric vector with the coefficient of the corrected analysis, the
#' standard error of the estimated coefficient and the confidence interval based
#' on the jackknife variance component in simex
perform_simex <- function(data){
  cols_no_reps <- grep("X_star", colnames(data))
  naive_fit <- glm(Y ~ X_star_1 + Z1,
                   family = binomial(link = "logit"),
                   data = data,
                   x = TRUE)
  var <- mean(apply(data[, cols_no_reps],
                    1,
                    var))
  simex_fit <- simex::simex(naive_fit,
                            "X_star_1",
                            measurement.error = sqrt(var))
  effect <- simex_fit$coefficients[2]
  se <- sqrt(simex_fit$variance.jackknife[2, 2])
  ci <- effect + c(qnorm(0.025), qnorm(0.975)) * se
  names(ci) <- c("lower", "upper")
  return(c(
    effect = unname(effect),
    se = se,
    ci = ci
  ))
}
#' Get the estimated effect of the three different analyses
#'
#' @param data data used to estimate the effects
#' @return named vector with 'uncor.*' the uncorrected effect, se and ci,
#' 'mecor.*' the measurement error corrected effect by means of mecor, se and ci
#' and 'simex.*' the measurement error corrected effect by means of simex, se
#' and ci
get_est_effects_logreg <- function(data){
  effect_uncor <- perform_uncor_logreg(data)
  effect_mecor <- perform_mecor_logreg(data)
  effect_simex <- perform_simex(data)
  effects <- c(uncor = effect_uncor,
               mecor = effect_mecor,
               simex = effect_simex)
  return(effects)
}
#' Get R-squared of the outcome model (Y ~ X + Z1)
#'
#' @param data data used to estimate R-squared
#' @return named vector with r_squared, the r squared of the outcome model
get_r_squared_logreg <- function(data) {
  cor_fit <- glm(Y ~ X + Z1,
                 family = binomial(link = "logit"),
                 data = data)
  r_squared <- DescTools::PseudoR2(cor_fit, "Nagelkerke")
  return(r_squared)
}
#' Get effect of crude model (Y ~ X)
#'
#' @param data data used to estimate crude model
#' @return named vector with crude_effect
get_crude_logreg <- function(data) {
  cor_fit <- glm(Y ~ X,
                 family = binomial(link = "logit"),
                 data = data)
  return(unname(coef(cor_fit)[2]))
}
