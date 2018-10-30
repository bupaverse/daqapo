#' Detect time anomalies
#'
#' Function detecting time anomalies, which can refer to activities with negative or zero duration
#' @param activity_log The activity log (renamed/formatted using functions rename_activity_log and convert_timestamp_format)
#' @param anomaly_type Type of anomalies that need to be detected (either "negative", "zero" or "both")
#' @param details Boolean indicating wheter details of the results need to be shown
#' @param filter_condition Condition that is used to extract a subset of the activity log prior to the application of the function
#' @return Information on the presence of time anomalies of the specified anomaly type
#' @export
time_anomalies <- function(activity_log, anomaly_type = "negative", details = TRUE, filter_condition = NULL){

  # Generate warning if inappropriate anomaly type is selected
  if(!(anomaly_type %in% c("negative", "zero", "both"))){
    warning("This anomaly type is not supported. Anomaly type should be: negative, zero, both. Default level of aggregation selected: negative.")
    anomaly_type <- "negative"
  }

  # Apply filter condition when specified
  if(!is.null(filter_condition)) {
    activity_log <- activity_log %>% filter_(filter_condition)
  }

  # Calculate durations
  activity_log$duration <- as.numeric(difftime(activity_log$complete, activity_log$start, units = "mins"))

  # Determine time anomalies
  if(anomaly_type == "negative"){
    anomalies <- activity_log %>% filter(duration < 0)
  } else if(anomaly_type == "zero"){
    anomalies <- activity_log %>% filter(duration == 0)
  } else{
    anomalies <- activity_log %>% filter(duration <= 0)
    anomalies$type <- ifelse(anomalies$duration < 0, "negative duration", "zero duration")
  }

  # Print output
  if(!is.null(filter_condition)) {
    cat("Applied filtering condition:", filter_condition, "\n")
  }
  cat("Selected anomaly type:", anomaly_type, "\n", "\n")

  cat("*** OUTPUT ***", "\n")
  cat("For", nrow(anomalies), "rows in the activity log (", nrow(anomalies) / nrow(activity_log) * 100, "%), an anomaly is detected.", "\n")

  if(nrow(anomalies) > 0){
    cat("The anomalies are spread over the activities as follows:", "\n")
    if(anomaly_type == "both"){
      print(anomalies %>% group_by(activity, type) %>% summarize(n = n()) %>% arrange(desc(n)))
    } else{
      print(anomalies %>% group_by(activity) %>% summarize(n = n()) %>% arrange(desc(n)))
    }
    cat("\n", "\n")

    if(details == TRUE){
      cat("Anomalies are found in the following rows:", "\n")
      return(anomalies)
    }
  }
}
