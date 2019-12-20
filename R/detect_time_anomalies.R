#' Detect time anomalies
#'
#' Function detecting time anomalies, which can refer to activities with negative or zero duration
#' @inheritParams detect_activity_frequency_violations
#' @param anomaly_type Type of anomalies that need to be detected (either "negative", "zero" or "both")
#' @return Information on the presence of time anomalies of the specified anomaly type
#' @export

detect_time_anomalies <- function(activitylog, anomaly_type = c("both", "negative","zero") ,
                                  details = TRUE, filter_condition = NULL){

  # Predefine variables
  type <- NULL
  duration <- NULL
  activity <- NULL

  anomaly_type <- match.arg(anomaly_type)
  # Generate warning if inappropriate anomaly type is selected


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

  # Calculate durations
  activitylog %>%
    mutate(duration = as.double(complete - start, units = "mins")) -> anomalies

  # Determine time anomalies
  if(anomaly_type == "negative"){
    anomalies <- anomalies %>% filter(duration < 0)
  } else if(anomaly_type == "zero"){
    anomalies <- anomalies %>% filter(duration == 0)
  } else{
    anomalies <- anomalies %>% filter(duration <= 0) %>%
      mutate(type = ifelse(duration < 0, "negative duration", "zero duration"))
  }

  # Print output
  message("Selected anomaly type: ", anomaly_type, "\n")

  message("*** OUTPUT ***")
  message("For ", nrow(anomalies), " rows in the activity log (", round(nrow(anomalies) / nrow(activitylog) * 100, 2), "%), an anomaly is detected.")

  if(nrow(anomalies) > 0){
    message("The anomalies are spread over the activities as follows:")
    if(anomaly_type == "both"){
      print(anomalies %>% group_by(!!activity_id_(activitylog), type) %>% summarize(n = n()) %>% arrange(desc(n)))
    } else{
      print(anomalies %>% group_by(!!activity_id_(activitylog)) %>% summarize(n = n()) %>% arrange(desc(n)))
    }

    if(details == TRUE){
      message("Anomalies are found in the following rows:")
      return(anomalies)
    }
  }
}
