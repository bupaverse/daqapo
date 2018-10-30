#' Detect attribute range violations
#'
#' Function detecting violations of the attribute range, i.e. attribute values outside the range of tolerable values
#' @param activity_log The activity log (renamed/formatted using functions rename_activity_log and convert_timestamp_format)
#' @param column Column in the activity log for which the domain range needs to be checked
#' @param domain_range Domain range for the specified column in parameter column
#' @param timestamp_format When the provided domain range represents timestamps, the format of the timestamps needs to be provided (either "yyyy-mm-dd hh:mm:ss" or "dd-mm-yyyy hh:mm:ss")
#' @param details Boolean indicating wheter details of the results need to be shown
#' @param filter_condition Condition that is used to extract a subset of the activity log prior to the application of the function
#' @return Information on the presence of attribute range violations in the specified column
#' @export
attribute_range <- function(activity_log, column, domain_range, timestamp_format = "yyyy-mm-dd hh:mm:ss", details = TRUE, filter_condition = NULL){

  # Apply filter condition when specified
  if(!is.null(filter_condition)) {
    activity_log <- activity_log %>% filter_(filter_condition)
  }

  # Save domain_range for printing purposes (before POSIXct conversion to numeric)
  print_domain_range <- domain_range

  # If column under investigation is a POSIXct-column, convert domain_range to POSIXct-objects
  if(is.POSIXct(activity_log[,column])){

    activity_log$new_ts <- as.numeric(activity_log[,column])

    if(timestamp_format == "yyyy-mm-dd hh:mm:ss"){
      domain_range[1] <- as.numeric(ymd_hms(domain_range[1]))
      domain_range[2] <- as.numeric(ymd_hms(domain_range[2]))
    } else if(timestamp_format == "dd-mm-yyyy hh:mm:ss"){
      domain_range[1] <- as.numeric(dmy_hms(domain_range[1]))
      domain_range[2] <- as.numeric(dmy_hms(domain_range[2]))
    } else{
      stop("Timestamp format not supported. Convert timestamp to one of the following formats: yyyy-mm-dd hh:mm:ss or dd-mm-yyyy hh:mm:ss.")
    }
  }

  # Determine the number of rows for which the attributes domain range is violated
  n_rows <- nrow(activity_log)

  if(is.numeric(activity_log[,column])){
    violated <- activity_log[which(is.na(activity_log[,column]) | activity_log[,column] < domain_range[1] | activity_log[,column] > domain_range[2]),]

  } else if(is.POSIXct(activity_log[,column])){
    violated <- activity_log[which(is.na(activity_log$new_ts) | activity_log$new_ts < domain_range[1] | activity_log$new_ts > domain_range[2]),]
    violated <- violated %>% select(-new_ts)

  } else if(is.character(activity_log[,column])){
    violated <- activity_log[-(which(is.na(activity_log[,column]) | activity_log[,column] %in% domain_range)),]

  } else if(is.factor(activity_log[,column])){
    activity_log[,column] <- as.character(activity_log[,column])
    domain_range <- unique(activity_log[, column])
    violated <- activity_log[-(which(is.na(activity_log[,column]) | activity_log[,column] %in% domain_range)),]
  }

  # Prepare output
  stat_outside <- nrow(violated) / n_rows * 100
  stat_inside <- 100 - stat_outside

  # Print output
  if(!is.null(filter_condition)) {
    cat("Applied filtering condition:", filter_condition, "\n", "\n")
  }

  cat("*** OUTPUT ***", "\n")
  cat("The domain range", print_domain_range, "for column", column, "is checked.", "\n", "\n")
  cat("The values fall within the specified domain range for",
      n_rows - nrow(violated), "(", stat_inside, "%) of the rows in the activity log and outside the domain range for",
      nrow(violated), "(", stat_outside, "%) of these rows.", "\n", "\n")

  if(details == TRUE){
    if(stat_outside > 0){
      cat("The following rows fall outside the specified domain range for column", column, ":", "\n")
      return(violated)
    }
  }
}
