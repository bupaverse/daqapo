#' Filter anomalies from the activity log
#'
#' Function that filters detected anomalies from the activity log
#' @param activity_log The activity log (renamed/formatted using functions rename_activity_log and convert_timestamp_format)
#' @param anomaly_log The anomaly log generated from the different DAQAPO tests
#' @return activitylog in which the anomaly rows are filtered out
#' @export

filter_anomalies <- function(activity_log, anomaly_log) {
  # Perform an anti_join, that's it!
  return(anti_join(activity_log, anomaly_log))
}
