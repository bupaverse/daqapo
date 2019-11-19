#' Detect incorrect activity names
#'
#' Function returning the incorrect activity labels in the log as indicated by the user. If details are requested, the entire activity log's rows containing incorrect activities are returned.
#' @param activity_log The activity log (renamed/formatted using functions rename_activity_log and convert_timestamp_format)
#' @param details Boolean indicating wheter details of the results need to be shown
#' @param filter_condition Condition that is used to extract a subset of the activity log prior to the application of the function
#' @return Information on the incorrect activities in the log
#' @export

detect_incorrect_activity_names <- function(activity_log, details = TRUE, filter_condition = NULL){

  # Predefine variables
  activity <- NULL

  # Initiate warning variables
  warning.filtercondition <- FALSE

  # Check if the required columns are present in the log
  missing_columns <- check_colnames(activity_log, c("activity"))
  if(!is.null(missing_columns)){
    stop("The following columns, which are required for the test, were not found in the activity log: ",
         paste(missing_columns, collapse = "\t"), ".", "\n  ",
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

  # Show all unique activity names to the user
  unique_names <- activity_log %>% count(activity) %>% arrange(-n)
  cat("The following activities were detected in the activity log:", "\n")
  print.data.frame(unique_names)

  # Ask the user for input on the wrong names
  wrong_indices <- readline("Please indicate by index and separated by a comma which names are incorrect (Enter N if everything is correct): ")
  if(str_to_upper(wrong_indices) == "N") {
    to_remove = NULL
    n_to_remove <- 0
  } else {
    wrong_indices <- wrong_indices %>% str_split(",") %>% unlist() %>% str_squish() %>% as.integer()

    # Get the activity labels from the entered indices
    to_remove <- c()
    for (index in wrong_indices) {
      to_remove <- append(to_remove, unique_names$activity[index])
      n_to_remove <- length(to_remove)
    }
  }

  # Print output
  if(!is.null(filter_condition) & !warning.filtercondition) {
    cat("Applied filtering condition:", filter_condition, "\n", "\n")
  }

  cat("*** OUTPUT ***", "\n")
  cat(n_to_remove, "out of", nrow(unique_names), "(", n_to_remove / nrow(unique_names) * 100, "% ) activity labels are identified to be incorrect.", "\n")
  if (n_to_remove > 0) {
    if (!details) {
      cat("These activity labels are:\n")
      return(to_remove)
    } else {
      cat("These activity labels are:", "\n", paste(to_remove, collapse = " - "), "\n\n")
      anomalies <- activity_log %>% filter(activity %in% to_remove)
      cat("Given this information,", nrow(anomalies), "of", nrow(activity_log), "(", nrow(anomalies) / nrow(activity_log) * 100, "% ) rows in the activity log are incorrect. These are the following:", "\n")
      return(activity_log %>% filter(activity %in% to_remove))
    }
  } else {
    return(NULL)
  }

}
