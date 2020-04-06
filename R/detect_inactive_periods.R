#' Detect inactive periods
#'
#' Function detecting inactive periods, i.e. periods of time in which no activity executions/arrivals are recorded in the activity log
#' @inheritParams detect_activity_frequency_violations
#' @param type Type of inactive periods you want to detect. "arrivals" will look for periods without new cases arriving. "activities" will look for periods where no activities occur.
#' @param threshold Threshold after which a period without activity executions/arrivals is considered as an inactive period (expressed in minutes)
#' @param timestamp Type of timestamp that needs to be taken into account in the analysis (either "start", "complete" or "both)
#' @param start_activities List of activity labels marking the first activity in the process. When specified, an inactive period only occurs when the time between two consecutive arrivals exceeds the specified threshold (arrival is proxied by the activity/activities specified in this argument).
#' @return Information on the presence of inactive periods.
#' @examples
#' data("hospital_actlog")
#' detect_inactive_periods(activitylog = hospital_actlog,threshold = 30)
#' @export
#'
detect_inactive_periods <- function(activitylog,
                                    threshold,
                                    type,
                                    timestamp,
                                    start_activities,
                                    details,
                                    filter_condition) {
  UseMethod("detect_inactive_periods")
}
#' @export

detect_inactive_periods.activitylog <- function(activitylog,
                                                threshold,
                                                type = c("arrivals", "activities"),
                                                timestamp = c("both", "start","complete"),
                                                start_activities = NA,
                                                details = TRUE,
                                                filter_condition = NULL){

  timestamp <- match.arg(timestamp)
  type <- match.arg(type)

  if(type == "activities" & !is.na(start_activities)) {
    warning("Specifying start_activities is only relevant for type = arrivals. Ignoring parameter.")
  }

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

  if(type == "arrivals") {
    detect_inactive_arrivals(activitylog, start_activities, threshold, timestamp) -> log
  } else if(type == "activities") {
    detect_inactive_activities(activitylog, threshold, timestamp) -> log
  }


  log <- log %>% filter(time_gap >= threshold)

  # Print general output information

  message("Selected timestamp parameter value: ", timestamp, "\n")
  message("Selected inactivity type:", type, "\n")

  # Print specific output
  message("*** OUTPUT ***")
  message("Specified threshold of ", threshold, " minutes is violated ", nrow(log), " times.", "\n")

  if(details == TRUE){
    message("Threshold is violated in the following periods: ", "\n")
    if(timestamp == "both"){
      log <- log %>% select(period_start = prior_complete, period_end = start, time_gap)
    } else if(timestamp == "start"){
      log <- log %>% select(period_start = prior_start, period_end = start, time_gap)
    } else if(timestamp == "complete") {
      log <- log %>% select(period_start = prior_complete, period_end = complete, time_gap)
    }
    return(log)
  }
}


detect_inactive_arrivals <- function(activitylog, start_activities, threshold, timestamp) {
  start <- NULL
  prior_start <- NULL
  complete <- NULL
  prior_complete <- NULL
  # filter only start events
  if(!is.na(start_activities)){
    activitylog %>%
      filter(!!activity_id_(activitylog) %in% start_activities) -> log
  } else {
    log <- activitylog
  }

  if(timestamp == "start"){
    log <- log %>%
      group_by(!!case_id_(activitylog)) %>%
      filter(start == min(start)) %>%
      ungroup() %>%
      arrange(start) %>%
      mutate(
        prior_start = lag(start),
      ) %>%
      mutate(time_gap = as.double(start - prior_start, units = "mins"))
  } else if(timestamp == "complete"){
    log <- log %>%
      group_by(!!case_id_(activitylog)) %>%
      filter(complete == min(complete)) %>%
      ungroup() %>%
      arrange(complete) %>%
      mutate(
        prior_complete = lag(complete)
      ) %>%
      mutate(time_gap = as.double(complete - prior_complete, units = "mins"))

  } else if(timestamp == "both") {
    log <- log %>%
      group_by(!!case_id_(activitylog)) %>%
      filter(start == min(start)) %>%
      filter(complete == min(complete)) %>%
      ungroup() %>%
      arrange(start, complete) %>%
      mutate(
        prior_complete = lag(complete)) %>%
      mutate(time_gap = as.double(start - prior_complete, units = "mins"))
  }
  return(log)
}




detect_inactive_activities <- function(activitylog, threshold, timestamp) {
  start <- NULL
  prior_start <- NULL
  complete <- NULL
  prior_complete <- NULL

  if(timestamp == "start"){
    log <- activitylog %>%
      arrange(start) %>%
      mutate(
        prior_start = lag(start),
      ) %>%
      mutate(time_gap = as.double(start - prior_start, units = "mins"))
  } else if(timestamp == "complete"){
    log <- activitylog %>%
      arrange(complete) %>%
      mutate(
        prior_complete = lag(complete)
      ) %>%
      mutate(time_gap = as.double(complete - prior_complete, units = "mins"))
  } else if(timestamp == "both") {
    log <- activitylog %>%
      arrange(start, complete) %>%
      mutate(
        prior_complete = lag(complete)
      ) %>%
      mutate(time_gap = as.double(start - prior_complete, units = "mins"))
  }
  return(log)
}

