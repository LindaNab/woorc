#' Generate data for simulation study
#'
#' @param seed_no seed no. used to generate data
#' @param nobs number of observations
#' @param gamma Cov(X,Z) / Var(X)
#' @param omega_sq Var(X|Z)
#' @param tau_sq extra variance due to measurement error in error-prone measure X_star
#' @param nrep number of replicates of X_star (defaults to 3)
#' @param sigma_sq Var(Y|X,Z)
#' @param beta estimand, defaults to 0.2
#' @return Data.frame with variables Z, X, X_star_1, ..., X_star_nrep and Y
generate_data <- function(seed_no,
                          nobs,
                          gamma,
                          omega_sq, # omega^2
                          nrep = 3,
                          tau_sq, # tau^2
                          sigma_sq, # sigma^2
                          beta = 0.2){
  set.seed(seed_no)
  Z <- rnorm(nobs, 32, sqrt(25))
  X <- 120 + gamma * Z + rnorm(nobs, 0, sqrt(omega_sq))
  X_star <- matrix(nrow = nobs,
                   ncol = nrep)
  colnames(X_star) <- paste0("X_star_", 1:nrep)
  for (i in 1:nrep){
    X_star[, i] <- X + rnorm(nobs, 0, sqrt(tau_sq))
  }
  Y <- 30 + beta * X + 0.2 * Z + rnorm(nobs, 0, sqrt(sigma_sq))
  df <- cbind.data.frame(Z, X, X_star, Y)
  return(df)
}
#' Generate data of a specific scenario number specified in input data available
#' in the package
#'
#' @param seed_no seed no. used to generate data
#' @param scen_no scenario no. in the input to generate data from
#' @return a data.frame with the data containing the variables Z, X, X_star_1,
#' ..., X_star_nrep and Y
generate_data_scen_no <- function(seed_no,
                                  scen_no) {
  data(input)
  input_param <- input[scen_no,
                       c("nobs",
                         "gamma",
                         "omega_sq",
                         "nrep",
                         "tau_sq",
                         "sigma_sq")]
  input_param$seed_no <- seed_no
  input_param_list <- as.list(input_param)
  data <- do.call(generate_data,
                  args = input_param_list)
  return(data)
}
