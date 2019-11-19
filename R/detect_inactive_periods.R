#' Detect inactive periods
#'
#' Function detecting inactive periods, i.e. periods of time in which no activity executions/arrivals are recorded in the activity log
#' @param activity_log The activity log (renamed/formatted using functions rename_activity_log and convert_timestamp_format)
#' @param threshold_in_minutes Threshold after which a period without activity executions/arrivals is considered as an inactive period (expressed in minutes)
#' @param timestamp Type of timestamp that needs to be taken into account in the analysis (either "start", "complete" or "both)
#' @param only_consider_first_activity List of activity labels marking the first activity in the process. When specified, an inactive period only occurs when the time between two consecutive arrivals exceeds the specified threshold (arrival is proxied by the activity/activities specified in this argument).
#' @param details Boolean indicating wheter details of the results need to be shown
#' @param filter_condition Condition that is used to extract a subset of the activity log prior to the application of the function
#' @return Information on the presence of inactive periods.
#' @export

detect_inactive_periods <- function(activity_log, threshold_in_minutes, timestamp = "both", only_consider_first_activity = NA, details = TRUE, filter_condition = NULL){

  # Predefine variables
  time_gap <- NULL
  prior_complete <- NULL
  prior_start <- NULL
  case_id <- NULL
  activity <- NULL
  start <- NULL
  complete <- NULL

  # Initiate warning variables
  warning.filtercondition <- FALSE

  # Check if the required columns are present in the log
  missing_columns <- check_colnames(activity_log, c("case_id", "activity", "start", "complete"))
  if(!is.null(missing_columns)){
    stop("The following columns, which are required for the test, were not found in the activity log: ",
         paste(missing_columns, collapse = "\t"), ".", "\n  ",
         "Please check rename_activity_log.")
  }

  # Generate warning if inappropriate timestamp value is provided
  if(!(timestamp %in% c("both", "start", "complete"))){
    warning("This timestamp parameter value is not supported. Timestamp parameter should have value: both, start, complete. Default timestamp parameter value selected: both.")
    timestamp <- "both"
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

  # Select specified start activity for each case when specified
  if(!is.na(only_consider_first_activity)){
    if(timestamp %in% c("both", "start")){
      activity_log <- as.data.frame(activity_log %>% group_by(case_id) %>%
                                      filter(activity %in% only_consider_first_activity) %>% filter(start == min(start)))
    } else{
      activity_log <- as.data.frame(activity_log %>% group_by(case_id) %>%
                                      filter(activity %in% only_consider_first_activity) %>% filter(complete == min(complete)))
    }
  }

  # Sort the activity log
  activity_log <- activity_log %>% arrange(start,complete)

  # Create prior_start and prior_complete column
  #activity_log$start <- as.character(activity_log$start)
  #activity_log$complete <- as.character(activity_log$complete)
  #activity_log$prior_start <- c(NA, activity_log$start[-nrow(activity_log)])
  #activity_log$prior_complete <- c(NA, activity_log$complete[-nrow(activity_log)])

  #activity_log$start <- ymd_hms(activity_log$start)
  #activity_log$complete <- ymd_hms(activity_log$complete)
  #activity_log$prior_start <- ymd_hms(activity_log$prior_start)
  #activity_log$prior_complete <- ymd_hms(activity_log$prior_complete)

  activity_log <- activity_log %>%
    mutate(
      prior_start = lag(start),
      prior_complete = lag(complete)
    )

  # Determine inactive periods
  if(timestamp == "both"){
    if(is.na(only_consider_first_activity)){
      activity_log$time_gap <- as.numeric(difftime(activity_log$start, activity_log$prior_complete, units = "mins"))
    } else{
      activity_log$time_gap <- as.numeric(difftime(activity_log$start, activity_log$prior_start, units = "mins"))
    }
  } else if(timestamp == "start"){
    activity_log$time_gap <- as.numeric(difftime(activity_log$start, activity_log$prior_start, units = "mins"))
  } else{
    activity_log$time_gap <- as.numeric(difftime(activity_log$complete, activity_log$prior_complete, units = "mins"))
  }

  activity_log <- activity_log %>% filter(time_gap >= threshold_in_minutes)

  # Print general output information
  if(!is.null(filter_condition)) {
    cat("Applied filtering condition", filter_condition, "\n")
  }
  cat("Selected timestamp parameter value:", timestamp, "\n", "\n")

  # Print specific output
  cat("*** OUTPUT ***", "\n")
  cat("Specified threshold of", threshold_in_minutes, "minutes is violated", nrow(activity_log), "times.", "\n", "\n")

  if(details == TRUE){
    cat("Threshold is violated in the following periods:", "\n")
    if(timestamp == "both"){
      if(is.na(only_consider_first_activity)){
        activity_log <- activity_log %>% select(period_start = prior_complete, period_end = start, time_gap)
      } else{
        activity_log <- activity_log %>% select(period_start = prior_start, period_end = start, time_gap)
      }
    } else if(timestamp == "start"){
      activity_log <- activity_log %>% select(period_start = prior_start, period_end = start, time_gap)
    } else{
      activity_log <- activity_log %>% select(period_start = prior_complete, period_end = complete, time_gap)
    }
    return(activity_log)
  }
}
