#' Input of Simulation Study // Performance RegCal when Reliability is Low
#'
#' A dataset containing the 65 scenarios that are used to input the simulation
#' study illustrating the performance of regression calibration when reliability of the
#' error prone exposure is low. The data generating mechanisms used in this study are:\cr
#' active_energy_expenditure ~ N(1, 0.25) \cr
#' active_energy_expenditure_star|active_energy_expenditure ~ N(active_energy_expenditure, \eqn{\tau^2}) \cr \cr
#' perc_lean_bm ~ N(80 + beta * active_energy_expenditure, \eqn{\sigma^2})
#'
#' @format A data frame with 65 rows and 6 variables:
#' \describe{
#'   \item{scen_no}{scenario no.}
#'   \item{nobs}{number of observations (sample size)}
#'   \item{tau_sq}{\eqn{\tau^2}}
#'   \item{beta}{estimand}
#'   \item{sigma_sq}{\eqn{\sigma^2}}
#'   \item{reliability}{Var(active_energy_expenditure) / Var(active_energy_expenditure_star)}
#' }
#' @examples
#' data("input_lowrel", package = "woorc")
"input_lowrel"
#' Input of Simulation Study // Bias vs Variance Tradeoff RegCal
#'
#' A dataset containing the 30 scenarios that are used to input the simulation
#' study illustrating the bias vs variance tradeoff between regression calibration
#' and the ordinary least squares estimator not corrected for measurement error.
#' The data generating mechanisms used in this study are:\cr
#' active_energy_expenditure ~ N(1, 0.25) \cr
#' active_energy_expenditure_star|active_energy_expenditure ~ N(active_energy_expenditure, \eqn{\tau^2}) \cr \cr
#' perc_lean_bm ~ N(80 + beta * active_energy_expenditure, \eqn{\sigma^2})
#'
#' @format A data frame with 30 rows and 6 variables:
#' \describe{
#'   \item{scen_no}{scenario no.}
#'   \item{nobs}{number of observations (sample size)}
#'   \item{tau_sq}{\eqn{\tau^2}}
#'   \item{beta}{estimand}
#'   \item{sigma_sq}{\eqn{\sigma^2}}
#'   \item{reliability}{Var(active_energy_expenditure) / Var(active_energy_expenditure_star)}
#' }
#' @examples
#' data("input_biasvsvar", package = "woorc")
"input_biasvsvar"
#' Summary of Simulation Study // Bias vs Variance Tradeoff RegCal
#'
#' A dataset containing a summary of the results from the 30 scenarios x 3
#' methods (= 90)
#'
#' @format A data frame with 90 rows and 21 variables:
#' \describe{
#'   \item{scen_no}{scenario no.}
#'   \item{method}{method used (uncor/mecor.delta/mecor.btstrp)}
#'   \item{nobs}{number of observations (sample size)}
#'   \item{tau_sq}{\eqn{\tau^2}}
#'   \item{beta}{estimand}
#'   \item{sigma_sq}{\eqn{\sigma^2}}
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
#' data("summary_biasvsvar", package = "woorc")
"summary_biasvsvar"
#' Summary of Simulation Study // Performance RegCal when Reliability is Low
#'
#' A dataset containing a summary of the results from the 65 scenarios x 3
#' methods (= 195)
#'
#' @format A data frame with 195 rows and 21 variables:
#' \describe{
#'   \item{scen_no}{scenario no.}
#'   \item{method}{method used (uncor/mecor.delta/mecor.btstrp)}
#'   \item{nobs}{number of observations (sample size)}
#'   \item{tau_sq}{\eqn{\tau^2}}
#'   \item{beta}{estimand}
#'   \item{sigma_sq}{\eqn{\sigma^2}}
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
#' data("summary_lowrel", package = "woorc")
"summary_lowrel"
