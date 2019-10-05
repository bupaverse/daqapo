#' Detect multi-registration
#'
#' Function detecting multi-registration for the same case or by the same resource at the same point in time
#' @param activity_log The activity log (renamed/formatted using functions rename_activity_log and convert_timestamp_format)
#' @param level_of_aggregation Level of aggregation at which multi-registration should be detected (either "resource" or "case")
#' @param timestamp Type of timestamp that needs to be taken into account in the analysis (either "start", "complete" or "both")
#' @param threshold_in_seconds Threshold which is applied to determine whether multi-registration occurs (expressed in seconds) (time gaps smaller than threshold are considered as multi-registration)
#' @param details Boolean indicating wheter details of the results need to be shown
#' @param filter_condition Condition that is used to extract a subset of the activity log prior to the application of the function
#' @return Information on the occurrence of multi-registration at the selected level of aggregation
#' @export
multi_registration <- function(activity_log, level_of_aggregation = "resource", timestamp = "complete", threshold_in_seconds, details = TRUE, filter_condition = NULL){

  # Predefine variables
  less_than_th <- NULL
  also_include <- NULL
  prior_start <- NULL
  prior_resource <- NULL
  time_gap <- NULL
  next_less_than_th <- NULL
  next_resource <- NULL
  prior_complete <- NULL
  prior_case <- NULL
  next_case <- NULL

  # Initiate warning variables
  warning.filtercondition <- FALSE

  # Check if the required columns are present in the log
  missing_columns <- check_colnames(activity_log, c("case_id", "resource", "start", "complete"))
  if(!is.null(missing_columns)){
    stop("The following columns, which are required for the test, were not found in the activity log: ",
         paste(missing_columns, collapse = "\t"), ".", "\n  ",
         "Please check rename_activity_log.")
  }

  # Generate warning if inappropriate level of aggregation is selected
  if(!(level_of_aggregation %in% c("resource", "case"))){
    warning("This level of aggregation is not supported. Level of aggregation should have value: resource, case. Default level of aggregation selected: resource.")
    level_of_aggregation <- "resource"
  }

  # Generate warning if inappropriate timestamp value is provided
  if(!(timestamp %in% c("start", "complete", "both"))){
    warning("This timestamp parameter value is not supported. Timestamp parameter should have value: start, complete, both. Default timestamp value selected: complete.")
    timestamp <- "complete"
  }

  # Apply filter condition when specified
  tryCatch({
    if(!is.null(filter_condition)) {
      activity_log <- activity_log %>% filter_(filter_condition)
    }
  }, error = function(e) {
    warning.filtercondition <<- TRUE
  }
  )

  if(warning.filtercondition) {
    warning("The condition '", filter_condition, "'  is invalid. No filtering performed on the dataset.")
  }

  # Sort the activity log and create appropriate prior-columns
  if(level_of_aggregation == "resource"){
    if(timestamp == "start"){
      activity_log <- activity_log %>% arrange(resource, start,complete) %>%
        mutate(
          prior_start = lag(start),
          prior_resource = lag(resource)
        )
      # activity_log$start <- as.character(activity_log$start)
      # activity_log$prior_start <- c(NA, activity_log$start[-nrow(activity_log)])
      # activity_log$prior_resource <- c(NA, activity_log$resource[-nrow(activity_log)])
      # activity_log$start <- ymd_hms(activity_log$start)
      # activity_log$prior_start <- ymd_hms(activity_log$prior_start)
    } else if(timestamp == "complete"){
      activity_log <- activity_log %>% arrange(resource, complete, start) %>%
        mutate(
          prior_complete = lag(complete),
          prior_resource = lag(resource)
        )
      # activity_log$complete <- as.character(activity_log$complete)
      # activity_log$prior_complete <- c(NA, activity_log$complete[-nrow(activity_log)])
      # activity_log$prior_resource <- c(NA, activity_log$resource[-nrow(activity_log)])
      # activity_log$complete <- ymd_hms(activity_log$complete)
      # activity_log$prior_complete <- ymd_hms(activity_log$prior_complete)
    } else{
      activity_log <- activity_log %>% arrange(resource, start, complete) %>%
        mutate(
          prior_start = lag(start),
          prior_complete = lag(complete),
          prior_resource = lag(resource)
        )
      # activity_log$start <- as.character(activity_log$start)
      # activity_log$complete <- as.character(activity_log$complete)
      # activity_log$prior_start <- c(NA, activity_log$start[-nrow(activity_log)])
      # activity_log$prior_complete <- c(NA, activity_log$complete[-nrow(activity_log)])
      # activity_log$prior_resource <- c(NA, activity_log$resource[-nrow(activity_log)])
      # activity_log$start <- ymd_hms(activity_log$start)
      # activity_log$prior_start <- ymd_hms(activity_log$prior_start)
      # activity_log$complete <- ymd_hms(activity_log$complete)
      # activity_log$prior_complete <- ymd_hms(activity_log$prior_complete)
    }
  } else{
    if(timestamp == "start"){
      activity_log <- activity_log %>% arrange(case_id, start,complete) %>%
        mutate(
          prior_start = lag(start),
          prior_case = lag(case_id)
        )
      # activity_log$start <- as.character(activity_log$start)
      # activity_log$prior_start <- c(NA, activity_log$start[-nrow(activity_log)])
      # activity_log$prior_case <- c(NA, activity_log$case_id[-nrow(activity_log)])
      # activity_log$start <- ymd_hms(activity_log$start)
      # activity_log$prior_start <- ymd_hms(activity_log$prior_start)
    } else if(timestamp == "complete"){
      activity_log <- activity_log %>% arrange(case_id, complete, start) %>%
        mutate(
          prior_complete = lag(complete),
          prior_case = lag(case_id)
        )
      # activity_log$complete <- as.character(activity_log$complete)
      # activity_log$prior_complete <- c(NA, activity_log$complete[-nrow(activity_log)])
      # activity_log$prior_case <- c(NA, activity_log$case_id[-nrow(activity_log)])
      # activity_log$complete <- ymd_hms(activity_log$complete)
      # activity_log$prior_complete <- ymd_hms(activity_log$prior_complete)
    } else{
      activity_log <- activity_log %>% arrange(case_id, start,complete) %>%
        mutate(
          prior_start = lag(start),
          prior_complete = lag(complete),
          prior_case = lag(case_id)
        )
      # activity_log$start <- as.character(activity_log$start)
      # activity_log$complete <- as.character(activity_log$complete)
      # activity_log$prior_start <- c(NA, activity_log$start[-nrow(activity_log)])
      # activity_log$prior_complete <- c(NA, activity_log$complete[-nrow(activity_log)])
      # activity_log$prior_case <- c(NA, activity_log$case_id[-nrow(activity_log)])
      # activity_log$start <- ymd_hms(activity_log$start)
      # activity_log$prior_start <- ymd_hms(activity_log$prior_start)
      # activity_log$complete <- ymd_hms(activity_log$complete)
      # activity_log$prior_complete <- ymd_hms(activity_log$prior_complete)
    }
  }

  # Determine whether multi-registration is present
  if(level_of_aggregation == "resource"){
    if(timestamp == "start"){
      activity_log$time_gap <- as.numeric(difftime(activity_log$start, activity_log$prior_start, units = "secs"))
    } else if(timestamp == "complete"){
      activity_log$time_gap <- as.numeric(difftime(activity_log$complete, activity_log$prior_complete, units = "secs"))
    } else{
      activity_log$time_gap <- as.numeric(difftime(activity_log$start, activity_log$prior_complete, units = "secs"))
    }

    # Determine lines that qualify as multi-registration
    multi_reg <- activity_log
    multi_reg$less_than_th <- multi_reg$resource == multi_reg$prior_resource & multi_reg$time_gap <= threshold_in_seconds
    # Following lines ensure that the first line in case of multi-registration is also recorded
    multi_reg$next_less_than_th <- c(multi_reg$less_than_th[-1], NA)
    multi_reg$next_resource <- c(multi_reg$resource[-1], NA)
    multi_reg$also_include <- multi_reg$resource == multi_reg$next_resource & multi_reg$next_less_than_th == TRUE

    if(timestamp == "start"){
      multi_reg <- multi_reg %>% filter(less_than_th == TRUE | also_include == TRUE) %>%
        select(-c(prior_start, prior_resource, time_gap, less_than_th, next_less_than_th, next_resource, also_include))
    } else if(timestamp == "complete"){
      multi_reg <- multi_reg %>% filter(less_than_th == TRUE | also_include == TRUE) %>%
        select(-c(prior_complete, prior_resource, time_gap, less_than_th, next_less_than_th, next_resource, also_include))
    } else{
      multi_reg <- multi_reg %>% filter(less_than_th == TRUE | also_include == TRUE) %>%
        select(-c(prior_start, prior_complete, prior_resource, time_gap, less_than_th, next_less_than_th, next_resource, also_include))
    }

  } else{
    if(timestamp == "start"){
      activity_log$time_gap <- as.numeric(difftime(activity_log$start, activity_log$prior_start, units = "secs"))
    } else if(timestamp == "complete"){
      activity_log$time_gap <- as.numeric(difftime(activity_log$complete, activity_log$prior_complete, units = "secs"))
    } else{
      activity_log$time_gap <- as.numeric(difftime(activity_log$start, activity_log$prior_complete, units = "secs"))
    }

    # Determine lines that qualify as multi-registration
    multi_reg <- activity_log
    multi_reg$less_than_th <- multi_reg$case_id == multi_reg$prior_case & multi_reg$time_gap <= threshold_in_seconds
    # Following lines ensure that the first line in case of multi-registration is also recorded
    multi_reg$next_less_than_th <- c(multi_reg$less_than_th[-1], NA)
    multi_reg$next_case <- c(multi_reg$case_id[-1], NA)
    multi_reg$also_include <- multi_reg$case_id == multi_reg$next_case & multi_reg$next_less_than_th == TRUE

    if(timestamp == "start"){
      multi_reg <- multi_reg %>% filter(less_than_th == TRUE | also_include == TRUE) %>%
        select(-c(prior_start, prior_case, time_gap, less_than_th, next_less_than_th, next_case, also_include))
    } else if(timestamp == "complete"){
      multi_reg <- multi_reg %>% filter(less_than_th == TRUE | also_include == TRUE) %>%
        select(-c(prior_complete, prior_case, time_gap, less_than_th, next_less_than_th, next_case, also_include))
    } else{
      multi_reg <- multi_reg %>% filter(less_than_th == TRUE | also_include == TRUE) %>%
        select(-c(prior_start,prior_complete, prior_case, time_gap, less_than_th, next_less_than_th, next_case, also_include))
    }
  }

  # Prepare_output
  pct_resource <- length(unique(multi_reg$resource)) / length(unique(activity_log$resource)) * 100
  pct_case <- length(unique(multi_reg$case_id)) / length(unique(activity_log$case_id)) * 100

  # Print output
  if(!is.null(filter_condition)) {
    cat("Applied filtering condition:", filter_condition, "\n")
  }
  cat("Selected level of aggregation:", level_of_aggregation, "\n")
  cat("Selected timestamp parameter value:", timestamp, "\n", "\n")

  cat("*** OUTPUT ***", "\n")

  if(level_of_aggregation == "resource"){
    cat("Multi-registration is detected for", length(unique(multi_reg$resource)), "of the", length(unique(activity_log$resource)), "resources (", pct_resource, "%) These resources are:", "\n")
    print(unique(multi_reg$resource))
    cat("\n")
  } else{
    cat("Multi-registration is detected for", length(unique(multi_reg$case_id)), "of the", length(unique(activity_log$case_id)), "cases (", pct_case, "%) of the cases. These cases are:", "\n")
    print(unique(multi_reg$case_id))
    cat("\n")
  }

  if(details == TRUE){
    if(level_of_aggregation == "resource" & pct_resource > 0){
      cat("For the following rows in the activity log, multi-registration is detected:", "\n")
      return(multi_reg)
    } else if(level_of_aggregation == "case" & pct_case > 0){
      cat("For the following rows in the activity log, multi-registration is detected:", "\n")
      return(multi_reg)
    }
  }
}
