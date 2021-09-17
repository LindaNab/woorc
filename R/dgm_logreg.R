#' Generate data for simulation study PART II (log reg)
#'
#' @param seed_no seed no. used to generate data
#' @param nobs number of observations
#' @param gamma effect of Z on X
#' @param tau_sq extra variance due to measurement error in error-prone measure X_star
#' @param nrep number of replicates of X_star (defaults to 2)
#' @param phi effect of Z on Y (given X)
#' @param beta estimand (defaults to 0.05)
#' @return Data.frame with variables Z, X, X_star_1, ..., X_star_nrep and Y
generate_data_logreg <- function(seed_no,
                                 nobs,
                                 gamma,
                                 tau_sq, # tau^2
                                 nrep = 2,
                                 phi,
                                 beta = 0.1) {
  set.seed(seed_no)
  Z1 <- runif(nobs, 18, 80)
  X <- rnorm(nobs, 4 + gamma * Z1, sqrt(1))
  X_star <- matrix(nrow = nobs,
                   ncol = nrep)
  colnames(X_star) <- paste0("X_star_", 1:nrep)
  for (i in 1:nrep){
    X_star[, i] <- X + rnorm(nobs, 0, sqrt(tau_sq))
  }
  P <- -7 + beta * X + phi * Z1
  Y <- rbinom(nobs, 1, 1 / (1 + exp(- P)))
  df <- cbind.data.frame(Z1, X, X_star, Y)
  return(df)
}
#' Generate data part II (logreg) of a specific scenario number specified in input data available
#' in the package
#'
#' @param seed_no seed no. used to generate data
#' @param scen_no scenario no. in the input to generate data from
#' @return a data.frame with the data containing the variables Z1, Z2, X, X_star_1,
#' ..., X_star_nrep and Y
generate_data_logreg_scen_no <- function(seed_no,
                                         scen_no) {
  data(input_logreg)
  input_param <- input_logreg[scen_no,
                              c("nobs",
                                "gamma",
                                "tau_sq",
                                "nrep",
                                "phi",
                                "beta")]
  input_param$seed_no <- seed_no
  input_param_list <- as.list(input_param)
  data <- do.call(generate_data_logreg,
                  args = input_param_list)
  return(data)
}
