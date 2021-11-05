#' Vector of evaluated parameters in the simulation study that will be summarised
#' using the rsimsum package
#' @return vector of length 12 with the names of the to be evaluated parameters
#' and the name of their monte carlo standerd error
eval_param_rsimsum <- function(){
  eval_param_rsimsum <- c("bias",
                          "mse",
                          "cover",
                          "modelse",
                          "empse",
                          "nsim")
  eval_param_rsimsum <- c(rbind(eval_param_rsimsum,
                                paste0(eval_param_rsimsum, "_mcse")))
  eval_param_rsimsum
}
#' Initialise summary object
#'
#' @param scen_nos scen no.'s to be used in the summary object
#' @param input input of simulation study
#' @return a data.frame with length(scen_nos) rows and length(eval_param_rsimsum())
#' +1 columns. See function eval_param_rsimsum() for evaluated parameters in
#' summary. Additionally, a column for the estimated r-squared is added.
init_summary <- function(scen_nos,
                         input){
  # parameters that will be evaluated
  eval_param <- eval_param_rsimsum()
  # init dataframe that will hold the results of the sim study
  summary_eval_param <- matrix(ncol = length(eval_param) + 1, # + col for r-squared
                               nrow = length(scen_nos))
  summary_eval_param <- data.frame(summary_eval_param)
  summary_eval_param$scen_no = scen_nos
  colnames(summary_eval_param)[1:length(eval_param)] <- eval_param
  colnames(summary_eval_param)[length(eval_param) + 1] <- "r_squared_est"
  # for all scen_nos, 3 methods are used
  summary <- expand.grid(scen_no = scen_nos,
                         method = c("uncor", "mecor.delta", "mecor.btstrp"))
  summary <- dplyr::left_join(summary,
                              input,
                              by = "scen_no")
  summary <- dplyr::left_join(summary,
                              summary_eval_param,
                              by = "scen_no")
  return(summary)
}
#' Summarise simulation study
#'
#' @param scen_nos scen no.'s to be summarised
#' @param use_input input of simulation study
#' @param processed_dir directory where the processed files of the simulation is to be found
#' found
#' @return a filled data.frame that is initiated using the function
#' init_summary(), and additionally a column with the percentage bias in each
#' scenario for each method
#'
#' @export
summarise_sim <- function(scen_nos,
                          use_input,
                          processed_dir = "./output/processed/"){
  summary <- init_summary(scen_nos,
                          use_input)
  for (i in seq_along(scen_nos)){
    summary <- summarise_one_scen_no(summary,
                                     scen_nos[i],
                                     use_input,
                                     processed_dir)
  }
  summary$perc_bias <- (summary$bias / summary$beta) * 100
  return(summary)
}
#' Summarise one scenario no.
#'
#' @param summary data.frame initiated by init_summary() to be filled with
#' summary parameters
#' @param scen_no scen no. to be summarised
#' @param use_input input of the simulation study
#' @param processed_dir directory where processed simulation output is to be
#' found, defaults to "./output/processed/"
#' @return this function will fill out the rows in the summary object of the
#' corresponding scen_no
summarise_one_scen_no <- function(summary,
                                  scen_no,
                                  use_input,
                                  processed_dir = "./output/processed/"){
  file <- paste0(processed_dir, "scen_no", scen_no, ".Rds")
  processed_output <- data.frame(readRDS(file = file))
  simsum <- rsimsum::simsum(data = processed_output,
                            estvarname = "effect",
                            true = use_input[use_input$scen_no == scen_no, "beta"],
                            se = "se",
                            methodvar = "method",
                            ci.limits = c("ci.lower", "ci.upper"),
                            ref = "uncor",
                            x = TRUE)
  # stats contains the params that will be pulled from the simsum object
  stats <- eval_param_rsimsum()[- grep("_mcse", eval_param_rsimsum())]
  methods <- c("uncor", "mecor.delta", "mecor.btstrp")
  for(i in 1:NROW(stats)){
    for (j in seq_along(methods)){
      summary <- fill_stat_and_mcse(summary,
                                    simsum,
                                    scen_no,
                                    stats[i],
                                    methods[j])
    }
  }
  r_squared <- # r-squared is for every method the same (since its r-squared of the outcome model using just X)
    mean(processed_output$r_squared[processed_output$method == "uncor"])
  summary[summary$scen_no == scen_no &
            summary$method %in% methods, "r_squared_est"] <- r_squared
  return(summary)
}
#' Fill summary with the evaluated parameter 'stat' and its mcse for a specific
#' scen_no and method using the simsum object from the package rsimsum
#'
#' @param summary the summary object initialised by init_summary()
#' @param simsum a simsum object created by the package rsimsum for a specific
#' scenario no.
#' @param scen_no scenario no. of the scenario that's evaluated
#' @param stat one of the parameters in eval_param_rsimsum()
#' @param method the method that's evaluated, either "uncor" / "mecor" / "simex"
#' @return this function will fill out the specific cells with 'stat' and its
#' 'stat'_mcse in the summary object (for a given scen_no and method)
fill_stat_and_mcse <- function(summary,
                               simsum,
                               scen_no,
                               stat,
                               method){
  summary[summary$scen_no == scen_no &
            summary$method == method, stat] <-
    simsum$summ[simsum$summ$stat == stat &
                  simsum$summ$method == method, "est"]
  summary[summary$scen_no == scen_no &
            summary$method == method, paste0(stat, ("_mcse"))] <-
    simsum$summ[simsum$summ$stat == stat &
                  simsum$summ$method == method, "mcse"]
  return(summary)
}
