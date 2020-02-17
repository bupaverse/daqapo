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

  # Apply filter condition when specified
  filter_specified <- FALSE
  tryCatch({
    is.null(filter_condition)
  }, error = function(e) {
    filter_specified <<- TRUE
  }
  )

  if(!filter_specified) {
    # geen filter gespecifieerd.

  } else {
    filter_condition_q <- enquo(filter_condition)
    activitylog <- APPLY_FILTER(activitylog, filter_condition_q = filter_condition_q)

  }

  # Unpack the parameters in the ellipsis
  params <- list(...)
  # Prepare the filter condition for anomaly detection
  anomaly_filter <- paste0(glue::glue("({activity_id(activitylog)} == '{names(params)}' & n > {params})"), collapse = " | ")

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

  message("*** OUTPUT ***")
  message(glue::glue("For {n_anomalies} cases in the activity log ({n_anomalies_relative}%) an anomaly is detected."))

  if(details == TRUE & nrow(anomalies) > 0){
    message("The anomalies are spread over the following cases:", "\n")
    return(anomalies)
  }
}
