#' Run simulation study
#'
#' @param nrep number of replications
#' @param scen_no scenarios numbers to be used
#' @param type simulation study type, either "biasvsvar" or "lowrel"
#' @param output_file_loc directory where output of simulation will be saved
#' @return a file named scen_no_* with the output will be saved in
#' output_file_loc, which constitutes of the uncorrected effect, the corrected
#' effect by means of mecor and the corrected effect by means of simex, the
#' estimated R-squared of the outcome model and the seed no.
#' @export
run_sim <- function(nrep,
                    type,
                    scen_no,
                    output_file_loc = "./"){
  if (type == "biasvsvar"){
    input <- input_biasvsvar
  } else if (type == "lowrel"){
    input <- input_lowrel
  }
  output_file <- paste0(output_file_loc, type, "_scen_no", scen_no, ".Rds")
    for (i in 1:nrep) {
      seed_no <- sample(1:1e6, 1)
      cat("\f")
      print(paste0("Type ", type, " Scen #", scen_no, " Run #", i, " with seed: ", seed_no))
      data <- generate_data_scen_no(input,
                                    seed_no,
                                    scen_no)
      effects <- get_est_effects(data)
      r_squared <- get_r_squared(data)
      output <- c(effects,
                  r_squared = r_squared,
                  seed_no = seed_no)
      save_output(output,
                  output_file)
    }
}
