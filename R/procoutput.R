#' Tidy output of simulation study
#'
#' @param scen_nos scen no.'s that should be tidied up/ processed
#' @param output_dir directory where raw output files are to be found, defaults
#' to "./output/". Here, files named 'scen_no#' should be located
#' @param processed_dir directory where the processed output should be saved,
#' defaults to "./output/processed/"
#' @return for each scen no. a .rds file named 'scen_no#.rds" is created in
#' processed_dir with the tidy output of the sim study in long format
#'
#' @export
process_output <- function(scen_nos,
                           output_dir = "./output/",
                           processed_dir = "./output/processed/") {
  for (i in seq_along(scen_nos)) {
    processed_output <- make_output_long(scen_nos[i],
                                         output_dir)
    saveRDS(processed_output,
            paste0(processed_dir, "scen_no", i, ".Rds"))
  }
}
#' Reshape the output of the simulation study to long format
#'
#' @param scen_no scenario no.
#' @param output_dir directory where raw output files are to be found, defaults
#' to "./output/". Here, files named 'scen_no#' should be located
#' @return a data.frame with 6 columns: 'r_squared', 'method', 'effect', 'se',
#' 'ci.lower' and 'ci.upper'. The number of rows is 3 times the number of
#' replications used in the simulation study (number of replications is equal to
#' the number of rows in the raw output)
make_output_long <- function(scen_no,
                             output_dir = "./output/"){
  file <- paste0(output_dir, "scen_no", scen_no, ".Rds")
  output <- data.frame(readRDS(file = file))
  output <- cbind(id = 1:nrow(output),
                  output)
  effect <- reshape_output(output[, c(1, 2, 6, 10, 14)], # effect (x 3) and r-squared
                           v.names = "effect")
  se <- reshape_output(output[, c(1, 3, 7, 11)],
                       v.names = "se")
  ci_lower <- reshape_output(output[, c(1, 4, 8, 12)],
                             v.names = "ci.lower")
  ci_upper <- reshape_output(output[, c(1, 5, 9, 13)],
                             v.names = "ci.upper")
  output_long <- cbind(effect[-1], # remove id (equal to rownames)
                       se[-c(1, 2)], # remove id and method
                       ci_lower[-c(1, 2)], # remove id and method
                       ci_upper[-c(1, 2)]) # remove id and method
  return(output_long)
}
#' Reshape one evaluated parameter of the simulation output to long format
#'
#' @param output_one_par data.frame with one row 'id' and 3 columns with an
#' evaluated parameter preceding "uncor."/ "mecor."/ "simex.", optionally this
#' data.frame may include other columns
#' @param v.names name of the evaluated parameter, can be either "effect"/ "se"/
#' "ci.lower"/ ci.upper"
#' @return a data.frame with at least 4 columns: 'id', 'method' indicating
#' either "uncor"/ "mecor"/ "simex", and the evaluated parameters equal to
#' 'v.names'
reshape_output <- function(output_one_par,
                           v.names){
  out <- stats::reshape(
    output_one_par,
    varying = paste0(c("uncor.", "mecor.delta.", "mecor.btstrp."), v.names),
    direction = "long",
    v.names = v.names,
    idvar = "id",
    timevar = "method",
    times = c("uncor", "mecor.delta", "mecor.btstrp")
  )
  rownames(out) <- 1:nrow(out)
  return(out)
}
