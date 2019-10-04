#' Check log column names presence
#'
#' Function that checks if the provided labels correspond to a column name in the log. FOR INTERNAL USE ONLY.
#' @param log The log whose columns names will be checked
#' @param ... The labels to search for in the log columns
#' @return A vector of labels that were not found in the log
#'

check_colnames <- function(log, ...) {

  # Unpack the labels passed in the ellipsis
  labels <- list(...) %>% unlist

  # Initialize the result vector as NULL
  result <- c()

  # Check every provided label if it corresponds to a column name in the log
  for(label in labels){
    if(!is.null(label) & !(label %in% colnames(log))){
      result <- append(result, label)
    }
  }

  # Return the result of the check. if column names were not identified, these labels are returned. Otherwie, the result is returned as NULL
  return(result)
}
