#' Input of Simulation Study
#'
#' A dataset containing the 23 scenarios that are used to input the simulation
#' study. The data generating mechanisms used in this study are:\cr
#' age ~ N(32, 25) \cr
#' blood_pressure|age ~ N(120 + \eqn{\gamma} age, \eqn{\omega^2}) \cr
#' blood_pressure_star|blood_pressure ~ N(blood_pressure, \eqn{\tau^2}) \cr
#' creatinine|blood_pressure,age ~ N(30 + 0.2 blood_pressure + 0.2 age, \eqn{\sigma^2})
#'
#' @format A data frame with 23 rows and 10 variables:
#' \describe{
#'   \item{scen_no}{scenario no.}
#'   \item{nobs}{number of observations (sample size)}
#'   \item{gamma}{\eqn{\gamma}}
#'   \item{omega_sq}{\eqn{\omega^2}}
#'   \item{nrep}{number of replicates}
#'   \item{tau_sq}{\eqn{\tau^2}}
#'   \item{sigma_sq}{\eqn{\sigma^2}}
#'   \item{beta}{estimand}
#'   \item{reliability}{Var(blood_pressure) / Var(blood_pressure_star)}
#'   \item{r_squared}{1 - Var(creatinine|blood_pressure, age) / Var(creatinine)}
#'   \item{attenuation}{Var(blood_pressure|age) / Var(blood_pressure_star|age)}
#'   \item{confounding}{0.2 E[age|blood_pressure]}
#' }
#' @examples
#' data("input", package = "simexvsmecor")
"input"
#' Input of Simulation Study PART II (logreg)
#'
#' A dataset containing the 23 scenarios that are used to input the simulation
#' study. The data generating mechanisms used in this study are:\cr
#' age ~ U(18, 80) \cr
#' na|age ~ N(4 + \eqn{\gamma} age, 1) \cr
#' na_star|na ~ N(na, \eqn{\tau^2}) \cr
#' hypertension|na,age ~ Bin(1, 1/(1+exp(-p))), where p = -7 + \eqn{\beta} na + \eqn{\phi} age
#'
#' @format A data frame with 23 rows and 10 variables:
#' \describe{
#'   \item{scen_no}{scenario no.}
#'   \item{nobs}{number of observations (sample size)}
#'   \item{gamma}{\eqn{\gamma}}
#'   \item{nrep}{number of replicates}
#'   \item{tau_sq}{\eqn{\tau^2}}
#'   \item{phi}{\eqn{\phi}}
#'   \item{beta}{estimand (0.1)}
#'   \item{reliability}{Var(na) / Var(na_star)}
#'   \item{attenuation}{Var(na|age) / Var(na_star|age)}
#' }
#' @examples
#' data("input_logreg", package = "simexvsmecor")
"input_logreg"
#' Summary of Simulation Study
#'
#' A dataset containing a summary of the results from the 14 scenarios x 3
#' methods (= 42)
#'
#' @format A data frame with 96 rows and 24 variables:
#' \describe{
#'   \item{scen_no}{scenario no.}
#'   \item{nobs}{number of observations (sample size)}
#'   \item{gamma}{\eqn{\gamma}}
#'   \item{omega_sq}{\eqn{\omega^2}}
#'   \item{nrep}{number of replicates}
#'   \item{tau_sq}{\eqn{\tau^2}}
#'   \item{sigma_sq}{\eqn{\sigma^2}}
#'   \item{beta}{estimand}
#'   \item{reliability}{Var(blood_pressure) / Var(blood_pressure_star)}
#'   \item{r_squared}{1 - Var(creatinine|blood_pressure, age) / Var(creatinine)}
#'   \item{attenuation}{Var(blood_pressure|age) / Var(blood_pressure_star|age)}
#'   \item{confounding}{0.2 E[age|blood_pressure]}
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
#' data("summary", package = "simexvsmecor")
"summary"
#' Input of Simulation Study Conducted by Perrier et al.
#'
#' A dataset containing 9 scenarios that are used to input the simulation
#' study conducted by Perrier et al. The data generating mechanisms used in this study are:\cr
#' X ~ N(0, 1) \cr
#' X_star|X ~ N(X, \eqn{\tau^2}) \cr
#' Y ~ N(14900 - 100 X, \eqn{1650^2}) \cr
#'
#' @format A data frame with 9 rows and 4 variables:
#' \describe{
#'   \item{scen_no}{scenario no.}
#'   \item{nobs}{number of observations (sample size)}
#'   \item{nrep}{number of replicates}
#'   \item{tau_sq}{\eqn{\tau^2}}
#'   \item{beta}{estimand (equal to 100)}
#' }
#' @examples
#' data("input_perrier", package = "simexvsmecor")
"input_perrier"
#' Summary of Simulation Study Replication Perrier et al.
#'
#' A dataset containing a summary of the results from the 8 scenarios x 3
#' methods (= 24)
#'
#' @format A data frame with 96 rows and 24 variables:
#' \describe{
#'   \item{scen_no}{scenario no.}
#'   \item{nobs}{number of observations (sample size)}
#'   \item{nrep}{number of replicates}
#'   \item{tau_sq}{\eqn{\tau^2}}
#'   \item{beta}{estimand}
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
#'   \item{r_sqaured_est}{estimated R-squared of Y ~ X}
#'   \item{perc_bias}{percentage bias}
#' }
#' @examples
#' data("summary", package = "summary_perrier")
"summary_perrier"
