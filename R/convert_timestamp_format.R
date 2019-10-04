#' Convert timestamp format
#'
#' Function converting the timestamps in the activity log to the appropriate format, required for the application of quality assessment functions
#' @param renamed_activity_log The activity log (renamed/formatted using functions rename_activity_log and convert_timestamp_format)
#' @param timestamp_format The format of the timestamps in the original dataset (either "yyyy-mm-dd hh:mm:ss" or "dd-mm-yyyy hh:mm:ss" or "yyyy/mm/dd hh:mm:ss" or "dd/mm/yyyy hh:mm:ss" or "yyyy-mm-dd" or "dd-mm-yyyy" or "yyyy/mm/dd" or "dd/mm/yyyy")
#' @return Activity log with timestamps in converted timestamp format
#' @export
convert_timestamp_format <- function(renamed_activity_log, timestamp_format = "yyyy-mm-dd hh:mm:ss"){

  if( !("start" %in% colnames(renamed_activity_log)) | !("complete" %in% colnames(renamed_activity_log)) ) {
    warning("Column labels are not converted to standardized values. Please check function rename_activity_log.")
    return(renamed_activity_log)
  }

  if(str_detect(timestamp_format, "/")){
    timestamp_format <- str_replace_all(timestamp_format, "/", "-")

    renamed_activity_log <- renamed_activity_log %>%
      mutate(
        start = str_replace_all(start, "/", "-"),
        complete = str_replace_all(complete, "/", "-")
      )
  }

  if(timestamp_format == "yyyy-mm-dd hh:mm:ss"){
    renamed_activity_log$start <- ymd_hms(renamed_activity_log$start)
    renamed_activity_log$complete <- ymd_hms(renamed_activity_log$complete)
  } else if(timestamp_format == "dd-mm-yyyy hh:mm:ss"){
    renamed_activity_log$start <- dmy_hms(renamed_activity_log$start)
    renamed_activity_log$complete <- dmy_hms(renamed_activity_log$complete)
  } else if(timestamp_format == "yyyy-mm-dd") {
    renamed_activity_log$start <- ymd(renamed_activity_log$start)
    renamed_activity_log$complete <- ymd(renamed_activity_log$complete)
    warning("The timestamps are too coarse: only dates available.")
  } else if(timestamp_format == "dd-mm-yyyy") {
    renamed_activity_log$start <- dmy(renamed_activity_log$start)
    renamed_activity_log$complete <- dmy(renamed_activity_log$complete)
    warning("The timestamps are too coarse: only dates available.")
  } else {
    warning("Timestamp format not supported. Convert timestamps to POSICXct format manually.")
  }

  return(renamed_activity_log)
}
