#' Generate data for simulation study
#'
#' @param seed_no seed no. used to generate data
#' @param nobs number of observations
#' @param tau_sq extra variance due to measurement error in error-prone measure X_star
#' @param beta estimand, defaults to 3
#' @param sigma_sq variance of the residual errors of the outcome model, defaults to 5
#' @return Data.frame with variables X, X_star and Y
generate_data <- function(seed_no,
                          nobs,
                          tau_sq,
                          sigma_sq = 5,
                          beta = 3){
  set.seed(seed_no)
  X <- rnorm(n = nobs, mean = 1, sd = sqrt(0.25))
  X_star <- X + rnorm(n = nobs, mean = 0, sd = sqrt(tau_sq))
  Y <- 80 + beta * X + rnorm(n = nobs, mean = 0, sd = sqrt(sigma_sq))
  df <- cbind.data.frame(X, X_star, Y)
  return(df)
}
#' Generate data of a specific scenario number specified in input data available
#' in the package
#'
#' @param seed_no seed no. used to generate data
#' @param scen_no scenario no. in the input to generate data from
#' @return a data.frame with the data containing the variables X, X_star and Y
generate_data_scen_no <- function(input,
                                  seed_no,
                                  scen_no) {
  input_param <- input[scen_no,
                       c("nobs",
                         "tau_sq",
                         "beta",
                         "sigma_sq")]
  input_param$seed_no <- seed_no
  input_param_list <- as.list(input_param)
  data <- do.call(generate_data,
                  args = input_param_list)
  return(data)
}
