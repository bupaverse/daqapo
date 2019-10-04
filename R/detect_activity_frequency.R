#' Check activity frequencies
#'
#' Function that detects activity frequency anomalies per case
#' @param activity_log The activity log (renamed/formatted using functions rename_activity_log and convert_timestamp_format)
#' @param ... Vectors of length 2 containing first the activity label followed by a numeric threshold value for that activity.
#' @param details Boolean indicating wheter details of the results need to be shown
#' @param filter_condition Condition that is used to extract a subset of the activity log prior to the application of the function
#' @return An overview of frequencies of activities
#' @export

activity_frequency <- function(activity_log, ... , details = TRUE, filter_condition = NULL) {

  # Initiate warning variables
  warning.filtercondition <- FALSE

  # Check if the required columns are present in the log
  missing_columns <- check_colnames(activity_log, c("case_id", "activity"))
  if(!is.null(missing_columns)){
    stop("The following columns, which are required for the test, were not found in the activity log: ",
         paste(missing_columns, collapse = "\t"), ".", "\n  ",
         "Please check rename_activity_log.")
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

  # Unpack the parameters in the ellipsis
  params <- list(...)
  activities <- c()
  thresholds <- c()

  for (sublist in params){
    activities <- append(activities, unlist(sublist)[1])
    thresholds <- append(thresholds, unlist(sublist)[2] %>% as.integer())
  }

  # Prepare the filter condition for anomaly detection
  anomaly_filter <- paste0("(activity == '", activities, "' & n > ", thresholds, ")", collapse = " | ")

  n_cases <- activity_log %>% distinct(case_id) %>% nrow

  # Filter out activities that were not passed to the function
  activity_log <- activity_log %>%
    filter(activity %in% activities)

  frequencies <- activity_log %>% count(case_id, activity) %>% arrange(-n)
  # Case level: interesting activities are those that occur >= threshold times
  anomalies <- frequencies %>% filter_(anomaly_filter)


  # Prepare output numbers
  n_anomalies <- anomalies %>% distinct(case_id) %>% nrow()
  n_anomalies_relative <- n_anomalies / n_cases * 100

  # Print output
  if(!is.null(filter_condition)) {
    cat("Applied filtering condition:", filter_condition, "\n")
  }

  cat("*** OUTPUT ***", "\n")
  cat("For", n_anomalies, "cases in the acivity log (", n_anomalies_relative, "%), an anomaly is detected.", "\n")

  if(details == TRUE & nrow(anomalies) > 0){
    cat("The anomalies are spread over the following cases:", "\n")
    return(anomalies)
  }
}
