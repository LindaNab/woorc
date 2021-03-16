#' Save output of one single replication of simulation study
#'
#' @param output the output to be saved
#' @param file the file where the output needs to be saved
save_output <- function(output,
                        file){
  if (file.exists(file)){
    con <- file(file)
    while (isOpen(con)){
      Sys.sleep(2)
    }
    open(con)
    results_in_file <- readRDS(file)
    new_results <- rbind(results_in_file,
                         output)
    rownames(new_results) <- NULL
    saveRDS(new_results, file = file)
    close(con)
  } else{ #create new file
    saveRDS(output, file = file)}
  message <- paste0(file, " saved!")
  print(message)
}
