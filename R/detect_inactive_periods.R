#' Detect inactive periods
#'
#' Function detecting inactive periods, i.e. periods of time in which no activity executions/arrivals are recorded in the activity log
#' @inheritParams detect_activity_frequency_violations
#' @param threshold Threshold after which a period without activity executions/arrivals is considered as an inactive period (expressed in minutes)
#' @param timestamp Type of timestamp that needs to be taken into account in the analysis (either "start", "complete" or "both)
#' @param only_first List of activity labels marking the first activity in the process. When specified, an inactive period only occurs when the time between two consecutive arrivals exceeds the specified threshold (arrival is proxied by the activity/activities specified in this argument).
#' @return Information on the presence of inactive periods.
#' @export
#'
detect_inactive_periods <- function(activitylog,
                                    threshold,
                                    timestamp,
                                    only_first,
                                    details,
                                    filter_condition) {
  UseMethod("detect_inactive_periods")
}
#' @export

detect_inactive_periods.activitylog <- function(activitylog,
                                                threshold,
                                    timestamp = c("both", "start","complete"),
                                    only_first = NA,
                                    details = TRUE,
                                    filter_condition = NULL){

  timestamp <- match.arg(timestamp)
  # Predefine variables
  time_gap <- NULL
  prior_complete <- NULL
  prior_start <- NULL
  case_id <- NULL
  activity <- NULL
  start <- NULL
  complete <- NULL

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

  # Select specified start activity for each case when specified
  if(!is.na(only_first)){
    if(timestamp %in% c("both", "start")){
      activitylog <- as.data.frame(activitylog %>% group_by(case_id) %>%
                                      filter(activity %in% only_first) %>% filter(start == min(start)))
    } else{
      activitylog <- as.data.frame(activitylog %>% group_by(case_id) %>%
                                      filter(activity %in% only_first) %>% filter(complete == min(complete)))
    }
  }

  # Sort the activity log
  activitylog <- activitylog %>% arrange(start,complete)

  # Create prior_start and prior_complete column
  #activity_log$start <- as.character(activity_log$start)
  #activity_log$complete <- as.character(activity_log$complete)
  #activity_log$prior_start <- c(NA, activity_log$start[-nrow(activity_log)])
  #activity_log$prior_complete <- c(NA, activity_log$complete[-nrow(activity_log)])

  #activity_log$start <- ymd_hms(activity_log$start)
  #activity_log$complete <- ymd_hms(activity_log$complete)
  #activity_log$prior_start <- ymd_hms(activity_log$prior_start)
  #activity_log$prior_complete <- ymd_hms(activity_log$prior_complete)

  activitylog <- activitylog %>%
    mutate(
      prior_start = lag(start),
      prior_complete = lag(complete)
    )

  # Determine inactive periods
  if(timestamp == "both"){
    if(is.na(only_first)){
      activitylog$time_gap <- as.numeric(difftime(activitylog$start, activitylog$prior_complete, units = "mins"))
    } else{
      activitylog$time_gap <- as.numeric(difftime(activitylog$start, activitylog$prior_start, units = "mins"))
    }
  } else if(timestamp == "start"){
    activity_log$time_gap <- as.numeric(difftime(activitylog$start, activitylog$prior_start, units = "mins"))
  } else{
    activity_log$time_gap <- as.numeric(difftime(activitylog$complete, activitylog$prior_complete, units = "mins"))
  }

  activitylog <- activitylog %>% filter(time_gap >= threshold)

  # Print general output information

  cat("Selected timestamp parameter value:", timestamp, "\n", "\n")

  # Print specific output
  cat("*** OUTPUT ***", "\n")
  cat("Specified threshold of", threshold, "minutes is violated", nrow(activitylog), "times.", "\n", "\n")

  if(details == TRUE){
    cat("Threshold is violated in the following periods:", "\n")
    if(timestamp == "both"){
      if(is.na(only_first)){
        activitylog <- activitylog %>% select(period_start = prior_complete, period_end = start, time_gap)
      } else{
        activitylog <- activitylog %>% select(period_start = prior_start, period_end = start, time_gap)
      }
    } else if(timestamp == "start"){
      activitylog <- activitylog %>% select(period_start = prior_start, period_end = start, time_gap)
    } else{
      activitylog <- activitylog %>% select(period_start = prior_complete, period_end = complete, time_gap)
    }
    return(activitylog)
  }
}
