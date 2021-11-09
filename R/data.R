#' Input of Simulation Study
#'
#' A dataset containing the 48 scenarios that are used to input the simulation
#' study. The data generating mechanisms used in this study are:\cr
#' active_energy_expenditure ~ N(1, 0.25) \cr
#' active_energy_expenditure_star|active_energy_expenditure ~ N(active_energy_expenditure, \eqn{\tau^2}) \cr \cr
#' perc_lean_bm ~ N(80 + beta * active_energy_expenditure, 5)
#'
#' @format A data frame with 23 rows and 10 variables:
#' \describe{
#'   \item{scen_no}{scenario no.}
#'   \item{nobs}{number of observations (sample size)}
#'   \item{tau_sq}{\eqn{\tau^2}}
#'   \item{beta}{estimand}
#'   \item{reliability}{Var(active_energy_expenditure) / Var(active_energy_expenditure_star)}
#' }
#' @examples
#' data("input", package = "woorc")
"input"
#' Summary of Simulation Study
#'
#' A dataset containing a summary of the results from the 48 scenarios x 3
#' methods (= 144)
#'
#' @format A data frame with 144 rows and 19 variables:
#' \describe{
#'   \item{scen_no}{scenario no.}
#'   \item{nobs}{number of observations (sample size)}
#'   \item{tau_sq}{\eqn{\tau^2}}
#'   \item{beta}{estimand}
#'   \item{reliability}{Var(active_energy_expenditure) / Var(active_energy_expenditure_star)}
#'   \item{bias}{bias}
#'   \item{bias_mcse}{monte carlo standard error of the bias}
#'   \item{mse}{mean squared error}
#'   \item{mse_mcse}{monte carlo standard error of the mean squared error}
#'   \item{cover}{coverage}
#'   \item{cover_mcse}{monte carlo standard error of the coverage}
#'   \item{modelse}{model based standard error}
#'   \item{modelse_mcse}{monte carlo standard error of the model based standard error}
#'   \item{empse}{empirical standard error}
#'   \item{empse_mcse}{monte carlo standard error of the empirical standard error}
#'   \item{nsim}{number of replications}
#'   \item{nsim_mcse}{monte carlo standard error of the number of replications (NA)}
#'   \item{r_sqaured_est}{estimated R-squared of Y ~ X + Z}
#'   \item{perc_bias}{percentage bias}
#' }
#' @examples
#' data("summary", package = "woorc")
"summary"
