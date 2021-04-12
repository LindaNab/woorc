#' Generate data equal to the simulation study conducted by Perrier et al.
#'
#' @param seed_no seed no. used to generate data
#' @param nobs number of observations (sample size)
#' @param nrep number of replicates of X_star (defaults to 2)
#' @param tau_sq extra variance due to measurement error in error-prone measure X_star (defaults to 2/3)
generate_data_perrier <- function(seed_no,
                                  nobs = 3000,
                                  nrep = 2,
                                  tau_sq = 2/3){
  set.seed(seed_no)
  X <- rnorm(nobs, 0, 1)
  X_star <- matrix(nrow = nobs,
                   ncol = nrep)
  colnames(X_star) <- paste0("X_star_", 1:nrep)
  for (i in 1:nrep){
    X_star[, i] <- X + rnorm(nobs, 0, sqrt(tau_sq))
  }
  Y <- 14900 - 100 * X + rnorm(nobs, 0, 1650)
  df <- cbind.data.frame(X, X_star, Y)
  return(df)
}
#' Generate data of a specific scenario number specified in input_perrier data available
#' in the package
#'
#' @param seed_no seed no. used to generate data
#' @param scen_no scenario no. in the input_perrier to generate data from
#' @return a data.frame with the data containing the variables X, X_star_1,
#' ..., X_star_nrep and Y
generate_data_perrier_scen_no <- function(seed_no,
                                          scen_no) {
  data(input_perrier)
  input_param <- input_perrier[scen_no,
                               c("nobs",
                                 "nrep",
                                 "tau_sq")]
  input_param$seed_no <- seed_no
  input_param_list <- as.list(input_param)
  data <- do.call(generate_data_perrier,
                  args = input_param_list)
  return(data)
}

#' Perform the uncorrected analysis
#'
#' @param data data to be used
#' @return numeric vector with the coefficient of the uncorrected analysis, the
#' standard error of the estimated coefficient and the confidence interval
perform_uncor_perrier <- function(data){
  uncor_fit <- lm(Y ~ X_star_1,
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
perform_mecor_perrier <- function(data){
  cols_no_reps <- grep("X_star", colnames(data))[-1]
  me <- with(data, mecor::MeasError(X_star_1,
                                    replicate = as.matrix(data[, cols_no_reps])))
  mecor_fit <- mecor::mecor(
    Y ~ me,
    data = data,
    method = "standard",
    B = 999
  )
  effect <- mecor_fit$corfit$coef[2]
  se <- summary(mecor_fit)$c$coefficients[2, 2]
  ci <-  summary(mecor_fit)$c$ci[2, 4:5]
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
perform_simex_perrier <- function(data){
  cols_no_reps <- grep("X_star", colnames(data))
  naive_fit <- lm(Y ~ X_star_1,
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
get_est_effects_perrier <- function(data){
  effect_uncor <- perform_uncor_perrier(data)
  effect_mecor <- perform_mecor_perrier(data)
  effect_simex <- perform_simex_perrier(data)
  effects <- c(uncor = effect_uncor,
               mecor = effect_mecor,
               simex = effect_simex)
  return(effects)
}
#' #' Get R-squared of the outcome model (Y ~ X)
#'
#' @param data data used to estimate R-squared
#' @return named vector with r_squared, the r squared of the outcome model
get_r_squared_perrier <- function(data){
  cor_fit <- lm(Y ~ X,
                data = data)
  r_squared <- summary(cor_fit)$r.squared
}
#' Run simulation study
#'
#' @param nrep number of replications
#' @param scen_no scenarios numbers to be used
#' @param output_file_loc directory where output of simulation will be saved
#' @return a file named perrier_scen_no_* with the output will be saved in
#' output_file_loc, which constitutes of the uncorrected effect, the corrected
#' effect by means of mecor and the corrected effect by means of simex, the
#' estimated R-squared of the outcome model and the seed no.
#' and the seed no.
#' @export
run_sim_perrier <- function(nrep = 1000,
                            scen_no,
                            output_file_loc = "./output/perrier/") {
  output_file <- paste0(output_file_loc, "perrier_scen_no", scen_no, ".Rds")
  for (i in 1:nrep) {
    seed_no <- sample(1:1e6, 1)
    cat("\f")
    print(paste0("Scen #", scen_no, " Run #", i, " with seed: ", seed_no))
    data <- generate_data_perrier_scen_no(seed_no,
                                          scen_no)
    effects <- get_est_effects_perrier(data)
    r_squared <- get_r_squared_perrier(data)
    output <- c(effects,
                r_squared = r_squared,
                seed_no = seed_no)
    save_output(output,
                output_file)
  }
}
