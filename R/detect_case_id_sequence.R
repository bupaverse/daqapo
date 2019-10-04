#' Detect gaps in case_id
#'
#' Function detecting gaps in the sequence of case identifiers
#' @param activity_log The activity log (renamed/formatted using functions rename_activity_log and convert_timestamp_format)
#' @param details Boolean indicating wheter details of the results need to be shown
#' @param filter_condition Condition that is used to extract a subset of the activity log prior to the application of the function
#' @return The case IDs that are missing in the sequence.
#' @export

case_id_sequence <- function(activity_log, details = TRUE, filter_condition = NULL) {

  # Initiate warning variables
  warning.filtercondition <- FALSE

  # Check if the required columns are present in the log
  missing_columns <- check_colnames(activity_log, "case_id")
  if(!is.null(missing_columns)){
    stop("The following columns, which are required for the test, were not found in the activity log: ",
         paste(missing_columns, collapse = "\t"), ".", "\n  ",
         "Please check rename_activity_log.")
  }

  # Check if case_id is a numeric column
  if(!class(activity_log$case_id) %in% c("numeric", "integer")){
    stop("The case IDs in the activity log are not numeric. Therefore, it is not possible to perform this test on the dataset.")
  }

  # Apply filter condition when specified
  tryCatch({
    if(!is.null(filter_condition)) {
      activity_log <- activity_log %>% filter_(filter_condition)
    }
  }, error = function(e) {
    warning.filtercondition <<- TRUE
  }
  )

  if(warning.filtercondition) {
    warning("The condition '", filter_condition, "'  is invalid. No filtering performed on the dataset.")
  }

  # Take all the case IDs in order
  case_ids <- activity_log %>% distinct(case_id) %>% arrange(case_id)

  # Detect gaps
  first <- case_ids$case_id[1]
  last <- case_ids$case_id[nrow(case_ids)]

  cases <- data.frame(case = seq(first, last)) %>%
    mutate(present = case %in% case_ids$case_id) %>%
    filter(present == F)

  # Prepare output
  missing_case_ids <- paste(cases$case, collapse = " - ")
  n_missing_case_ids <- nrow(cases)
  n_expected_cases <- seq(first, last) %>% length()

  # Print output
  if(!is.null(filter_condition)) {
    cat("Applied filtering condition", filter_condition, "\n")
  }
  cat("*** OUTPUT ***", "\n")
  cat("It was checked if there are gaps in the sequence of case IDs", "\n")
  cat("From the", n_expected_cases, "expected cases in the activity log, ranging from", first, "to", last, ",",
      n_missing_case_ids, "(", n_missing_case_ids / n_expected_cases * 100, "% ) are missing.\n")

  if(details == TRUE){
    cat("These case numbers are:\n")
    return(missing_case_ids)
  }
}
