#' Perform the uncorrected analysis
#'
#' @param data data to be used
#' @return numeric vector with the coefficient of the uncorrected analysis, the
#' standard error of the estimated coefficient and the confidence interval
perform_uncor <- function(data){
  uncor_fit <- lm(Y ~ X_star,
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
#' standard error of the estimated coefficient (delta) and associated confidence interval,
#' the standard error of the estimated coefficient (bootstrap) and the confidence interval based
#' on the bootstrap
perform_mecor <- function(data){
  mecor_fit <- mecor::mecor(
    Y ~ mecor::MeasError(X_star,
                         reference = X),
    data = data,
    method = "standard",
    B = 999
  )
  effect <- mecor_fit$corfit$coef["X"]
  se_delta <- summary(mecor_fit)$c$coefficients["X", "SE"]
  ci_delta <-  summary(mecor_fit)$c$ci["X", c("LCI", "UCI")]
  names(ci_delta) <- c("lower", "upper")
  se_btstrp <- summary(mecor_fit)$c$coefficients["X", "SE (btstr)"]
  ci_btstrp <-  summary(mecor_fit)$c$ci["X", c("LCI (btstr)", "UCI (btstr)")]
  names(ci_btstrp) <- c("lower", "upper")
  return(c(
    delta.effect = unname(effect),
    delta.se = se_delta,
    delta.ci = ci_delta,
    btstrp.effect = unname(effect), # for ease of creation of summary of sim study
    btstrp.se = se_btstrp,
    btstrp.ci = ci_btstrp
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
  effects <- c(uncor = effect_uncor,
               mecor = effect_mecor)
  ereturn(effects)
}
#' Get R-squared of the outcome model (Y ~ X)
#'
#' @param data data used to estimate R-squared
#' @return named vector with r_squared, the r squared of the outcome model
get_r_squared <- function(data){
  cor_fit <- lm(Y ~ X,
                data = data)
  r_squared <- summary(cor_fit)$r.squared
}
