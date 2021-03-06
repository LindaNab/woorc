% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{summary_biasvsvar}
\alias{summary_biasvsvar}
\title{Summary of Simulation Study // Bias vs Variance Tradeoff RegCal}
\format{
A data frame with 90 rows and 21 variables:
\describe{
  \item{scen_no}{scenario no.}
  \item{method}{method used (uncor/mecor.delta/mecor.btstrp)}
  \item{nobs}{number of observations (sample size)}
  \item{tau_sq}{\eqn{\tau^2}}
  \item{beta}{estimand}
  \item{sigma_sq}{\eqn{\sigma^2}}
  \item{reliability}{Var(active_energy_expenditure) / Var(active_energy_expenditure_star)}
  \item{bias}{bias}
  \item{bias_mcse}{monte carlo standard error of the bias}
  \item{mse}{mean squared error}
  \item{mse_mcse}{monte carlo standard error of the mean squared error}
  \item{cover}{coverage}
  \item{cover_mcse}{monte carlo standard error of the coverage}
  \item{modelse}{model based standard error}
  \item{modelse_mcse}{monte carlo standard error of the model based standard error}
  \item{empse}{empirical standard error}
  \item{empse_mcse}{monte carlo standard error of the empirical standard error}
  \item{nsim}{number of replications}
  \item{nsim_mcse}{monte carlo standard error of the number of replications (NA)}
  \item{r_sqaured_est}{estimated R-squared of Y ~ X + Z}
  \item{perc_bias}{percentage bias}
}
}
\usage{
summary_biasvsvar
}
\description{
A dataset containing a summary of the results from the 30 scenarios x 3
methods (= 90)
}
\examples{
data("summary_biasvsvar", package = "woorc")
}
\keyword{datasets}
