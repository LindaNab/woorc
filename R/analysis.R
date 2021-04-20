#' Perform the uncorrected analysis
#'
#' @param data data to be used
#' @return numeric vector with the coefficient of the uncorrected analysis, the
#' standard error of the estimated coefficient and the confidence interval
perform_uncor <- function(data){
  uncor_fit <- lm(Y ~ X_star_1 + Z,
                  data = data)
  effect <- coef(uncor_fit)[2]
  se <- summary(uncor_fit)$coefficients[2, 2]
  ci <- confint(uncor_fit)[2, ]
  names(ci) <- c("lower", "upper")
  return(c(
    effect = unname(effect),
    se = se,
    ci = ci
  ))
}
#' Perform measurement error correction by means of mecor
#'
#' @param data data to be used
#' @return numeric vector with the coefficient of the corrected analysis, the
#' standard error of the estimated coefficient and the confidence interval based
#' on the bootstrap
perform_mecor <- function(data){
  cols_no_reps <- grep("X_star", colnames(data))[-1]
  var <- mean(apply(data[, cols_no_reps],
                    1,
                    var))
  mecor_fit <- mecor::mecor(
    Y ~ mecor::MeasErrorRandom(X_star_1,
                               var) + Z,
    data = data,
    method = "standard",
    B = 999
  )
  effect <- mecor_fit$corfit$coef[2]
  se <- summary(mecor_fit)$c$coefficients[2, 2]
  ci <-  summary(mecor_fit)$c$ci[2, 2:3]
  names(ci) <- c("lower", "upper")
  return(c(
    effect = unname(effect),
    se = se,
    ci = ci
  ))
}
#' Perform measurement error correction by means of simex
#'
#' @param data data to be used
#' @return numeric vector with the coefficient of the corrected analysis, the
#' standard error of the estimated coefficient and the confidence interval based
#' on the jackknife variance component in simex
perform_simex <- function(data){
  cols_no_reps <- grep("X_star", colnames(data))
  naive_fit <- lm(Y ~ X_star_1 + Z,
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
get_est_effects <- function(data){
  effect_uncor <- perform_uncor(data)
  effect_mecor <- perform_mecor(data)
  effect_simex <- perform_simex(data)
  effects <- c(uncor = effect_uncor,
               mecor = effect_mecor,
               simex = effect_simex)
  return(effects)
}
#' Get R-squared of the outcome model (Y ~ X + Z)
#'
#' @param data data used to estimate R-squared
#' @return named vector with r_squared, the r squared of the outcome model
get_r_squared <- function(data){
  cor_fit <- lm(Y ~ X + Z,
                data = data)
  r_squared <- summary(cor_fit)$r.squared
}
