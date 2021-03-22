process_scen_nos <- function(scen_nos,
                             output_dir = "./output/",
                             processed_dir = "./output/processed/"){
  for (i in seq_along(scen_nos)){
    processed_output <- make_output_long(i,
                        output_dir)
    saveRDS(processed_output,
            paste0(processed_dir, "scen_no", i, ".Rds"))
  }
}

make_output_long <- function(scen_no,
                             output_dir = "./output/"){
  file <- paste0(output_dir, "scen_no", scen_no, ".Rds")
  output <- data.frame(readRDS(file = file))
  output <- cbind(id = 1:nrow(output), output)
  effect <- reshape_output(output[, c(1, 2, 6, 10)],
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

reshape_output <- function(output_one_par,
                           v.names){
  out <- stats::reshape(
    output_one_par,
    varying = paste0(c("uncor.", "mecor.", "simex."), v.names),
    direction = "long",
    v.names = v.names,
    idvar = "id",
    timevar = "method",
    times = c("uncor", "mecor", "simex")
  )
  rownames(out) <- 1:nrow(out)
  return(out)
}

summarise_sim <- function(scen_nos,
                          output_dir = "./output/"){
  summary <- init_summary(scen_nos)
  for (i in seq_along(scen_nos)){
    summary <- summarise_one_scen_no(summary, scen_nos[i])
  }
  summary$perc_bias <- (summary$bias / 0.2) * 100
  return(summary)
}

summarise_one_scen_no <- function(summary,
                                  scen_no,
                                  processed_dir = "./output/processed/"){
  file <- paste0(processed_dir, "scen_no", scen_no, ".Rds")
  processed_output <- data.frame(readRDS(file = file))
  simsum <- rsimsum::simsum(data = processed_output,
                            estvarname = "effect",
                            true = 0.2,
                            se = "se",
                            methodvar = "method",
                            ci.limits = c("ci.lower", "ci.upper"),
                            ref = "uncor",
                            x = TRUE)
  # stats contains the params that will be pulled from the simsum object
  stats <- eval_param_rsimsum()[- grep("_mcse", eval_param_rsimsum())]
  methods <- c("uncor", "mecor", "simex")
  for(i in 1:NROW(stats)){
    for (j in seq_along(methods)){
      summary <- fill_stat_and_mcse(summary,
                                    simsum,
                                    scen_no,
                                    stats[i],
                                    methods[j])
    }
  }
  return(summary)
}

# Loop trough all stats and fill the subsequent cells in summary
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

init_summary <- function(scen_nos){
  # parameters that will be evaluated
  eval_param <- eval_param_rsimsum()
  # init dataframe that will hold the results of the sim study
  summary_eval_param <- matrix(ncol = length(eval_param),
                               nrow = length(scen_nos))
  summary_eval_param <- data.frame(summary_eval_param)
  summary_eval_param$scen_no = scen_nos
  colnames(summary_eval_param)[1:length(eval_param)] <- eval_param
  # for all scen_nos, 3 methods are used
  summary <- expand.grid(scen_no = scen_nos,
                         method = c("uncor", "mecor", "simex"))
  data(input) # input parameters of the sim study
  summary <- dplyr::left_join(summary,
                              input,
                              by = "scen_no")
  summary <- dplyr::left_join(summary,
                              summary_eval_param,
                              by = "scen_no")
  return(summary)
}
