#' Input of Simulation Study
#'
#' A dataset containing the 14 scenarios that are used to input the simulation
#' study. The data generating mechanisms used in this study are:\cr
#' age ~ N(32, 25) \cr
#' blood_pressure ~ N(120 + \eqn{\gamma}Z, \eqn{\omega^2}) \cr
#' blood_pressure_star ~ N(blood_pressure, \eqn{\tau^2}) \cr
#' creatinine ~ N(30 + 0.2 blood_pressure + 0.2 age, \eqn{\sigma^2})
#'
#' @format A data frame with 14 rows and 10 variables:
#' \describe{
#'   \item{scen_no}{scenario no.}
#'   \item{nobs}{number of observations (sample size)}
#'   \item{gamma}{\eqn{\gamma}}
#'   \item{omega_sq}{\eqn{\omega^2}}
#'   \item{tau_sq}{\eqn{\tau^2}}
#'   \item{sigma_sq}{\eqn{\sigma^2}}
#'   \item{reliability}{Var(blood_pressure) / Var(blood_pressure_star)}
#'   \item{r_squared}{1 - Var(creatinine|blood_pressure, age) / Var(creatinine)}
#'   \item{attenuation}{Var(blood_pressure|age) / Var(blood_pressure_star|age)}
#'   \item{confounding}{0.2 E[age|blood_pressure]}
#' }
#' @examples
#' data("input", package = "simexvsmecor")
"input"
#' Summary of Simulation Study
#'
#' A dataset containing a summary of the results from the 14 scenarios x 3
#' methods (= 42)
#'
#' @format A data frame with 42 rows and 24 variables:
#' \describe{
#'   \item{scen_no}{scenario no.}
#'   \item{nobs}{number of observations (sample size)}
#'   \item{gamma}{\eqn{\gamma}}
#'   \item{omega_sq}{\eqn{\omega^2}}
#'   \item{tau_sq}{\eqn{\tau^2}}
#'   \item{sigma_sq}{\eqn{\sigma^2}}
#'   \item{reliability}{Var(blood_pressure) / Var(blood_pressure_star)}
#'   \item{r_squared}{1 - Var(creatinine|blood_pressure, age) / Var(creatinine)}
#'   \item{attenuation}{Var(blood_pressure|age) / Var(blood_pressure_star|age)}
#'   \item{confounding}{0.2 E[age|blood_pressure]}
#'   \item{bias}{bias}
#'   \item{bias_mcse}{monte carlo standard error of the bias}
#' }
#' @examples
#' data("summary", package = "simexvsmecor")
"summary"
