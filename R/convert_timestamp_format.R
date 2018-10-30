#' Convert timestamp format
#'
#' Function converting the timestamps in the activity log to the appropriate format, required for the application of quality assessment functions
#' @param activity_log The activity log
#' @param timestamp_format The format of the timestamps in the original dataset (either "yyyy-mm-dd hh:mm:ss" or "dd-mm-yyyy hh:mm:ss")
#' @return Activity log with timestamps in converted timestamp format
#' @export
convert_timestamp_format <- function(renamed_activity_log, timestamp_format = "yyyy-mm-dd hh:mm:ss"){

  if(!("start" %in% colnames(renamed_activity_log) | !("complete" %in% colnames(rename_activity_log)))){
    stop("Column labels are not converted to standardized values. Please check function rename_activity_log.")
  }

  if(timestamp_format == "yyyy-mm-dd hh:mm:ss"){
    renamed_activity_log$start <- ymd_hms(renamed_activity_log$start)
    renamed_activity_log$complete <- ymd_hms(renamed_activity_log$complete)
  } else if(timestamp_format == "dd-mm-yyyy hh:mm:ss"){
    renamed_activity_log$start <- dmy_hms(renamed_activity_log$start)
    renamed_activity_log$complete <- dmy_hms(renamed_activity_log$complete)
  } else{
    stop("Timestamp format not supported. Convert timestamp to POSICXct object manually.")
  }

  return(renamed_activity_log)
}
