#' Check activity frequencies
#'
#' Function that detects activity frequency anomalies per case
#' @param activitylog The activity log
#' @param ... Named vectors with name of the activity, and value of the threshold.
#' @param details Boolean indicating wheter details of the results need to be shown
#' @param filter_condition Condition that is used to extract a subset of the activity log prior to the application of the function
#' @return An overview of frequencies of activities
#' @export

detect_activity_frequency_violations <- function(activitylog, ..., details, filter_condition) {
  UseMethod("detect_activity_frequency_violations")
}

# @describeIn detect_activity_frequency_violations Detect activity frequency voilations in activity log
#' @export

detect_activity_frequency_violations.activitylog <- function(activitylog, ... , details = TRUE, filter_condition = NULL) {

  # Initiate warning variables
  warning.filtercondition <- FALSE

  # Apply filter condition when specified
  tryCatch({
    if(!is.null(filter_condition)) {
      activitylog <- activitylog %>% filter(!!rlang::parse_expr(filter_condition))
    }
  }, error = function(e) {
    warning.filtercondition <<- TRUE
  }
  )

  if(warning.filtercondition) {
     warning("The condition '", filter_condition, "'  is invalid. No filtering performed on the dataset.")
    #Make sure we don't pretend as if it is filtered later
    filter_condition <- NULL
  }

  # Unpack the parameters in the ellipsis
  params <- list(...)

  # Prepare the filter condition for anomaly detection
  anomaly_filter <- paste0(glue::glue("({activity_id(activitylog)} == '", names(params), "' & n > ", params, ")", collapse = " | "))

  n_cases <- n_cases(activitylog)

  # Case level: interesting activities are those that occur >= threshold times
  anomalies <- activitylog %>%
    filter_activity(names(params)) %>%
    count(!!case_id_(activitylog), !!activity_id_(activitylog)) %>%
    arrange(-n) %>%
    filter(!!rlang::parse_expr(anomaly_filter))

  # Prepare output numbers
  n_anomalies <- anomalies %>% pull(!!case_id_(activitylog)) %>% unique() %>% length()
  n_anomalies_relative <- n_anomalies / n_cases * 100

  # Print output
  if(!is.null(filter_condition)) {
    message(glue::glue("Applied filtering condition: {filter_condition}"))
  }

  message("*** OUTPUT ***")
  message(glue::glue("For {n_anomalies} cases in the activity log ({n_anomalies_relative}%) an anomaly is detected."))

  if(details == TRUE & nrow(anomalies) > 0){
    message("The anomalies are spread over the following cases:", "\n")
    return(anomalies)
  }
}
