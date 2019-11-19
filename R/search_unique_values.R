#' Search for unique values / distinct combinations
#'
#' Function that lists all distinct combinations of the given columns in the activity log
#' @param activity_log The activity log (renamed/formatted using functions rename_activity_log and convert_timestamp_format)
#' @param ... The names of columns in the activity log for which you want to show the different combinations found in the log. If only one column is provided, this results in a list of unique values in that column.
#' @param filter_condition Condition that is used to extract a subset of the activity log prior to the application of the function
#' @return A data frame containing the unique (distinct) values (combinations) in the requested column(s).
#' @export

detect_unique_values <- function(activity_log, ..., filter_condition = NULL) {

  # Initiate warning variables
  warning.filtercondition <- FALSE

  # Unpack the parameters in the ellipsis
  columns <- list(...) %>% unlist

  # Check if the log has been renamed
  missing_columns <- check_colnames(activity_log, columns)
  if(!is.null(missing_columns)){
    stop("The following column labels were not found in the log:",
         paste(missing_columns, collapse = "\t"), ".\n  ",
         "Please check rename_activity_log.")
  }

  # Apply filter condition when specified
  tryCatch({
    if(!is.null(filter_condition)) {
      activity_log <- activity_log %>% filter(!! rlang::parse_expr(filter_condition))
    }
  }, error = function(e) {
    warning.filtercondition <<- TRUE
  }
  )

  if(warning.filtercondition) {
    warning("The condition '", filter_condition, "'  is invalid. No filtering performed on the dataset.")
  }

  # Take the columns under investigation, compute their distinct values and arrange alphabetically
  params <- list(...) %>% unlist()
  output <- activity_log %>% distinct(!! rlang::parse_expr(params)) %>% arrange(!! rlang::parse_expr(params))

  # Prepare output
  if(!is.null(filter_condition) & !warning.filtercondition) {
    cat("Applied filtering condition:", filter_condition, "\n", "\n")
  }

  cat("*** OUTPUT ***", "\n")
  if(is.null(columns)){
    cat("No column names were passed. Distinct rows are computed for the entire dataset.", "\n\n")
  } else {
    cat("Distinct entries are computed for the following columns:\n  ", paste(columns, collapse = " - "), "\n", sep = "")
  }
  return(output)

}
