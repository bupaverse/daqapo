#' Detect activity duration outliers
#'
#' Function detecting duration outliers for a particular activity
#' @param activity_log The activity log (renamed/formatted using functions rename_activity_log and convert_timestamp_format)
#' @param activity_considered Activity name for which duration outliers should be detected
#' @param bound_sd Number of standard deviations from the mean duration which is used to define an outlier in the absence of lower_bound and upper_bound (default value of 3 is used)
#' @param lower_bound Lower bound for activity duration used during outlier detection (expressed in minutes)
#' @param upper_bound Upper bound for activity duration used during outlier detection (expressed in minutes)
#' @param details Boolean indicating wheter details of the results need to be shown
#' @param filter_condition Condition that is used to extract a subset of the activity log prior to the application of the function
#' @return Information on the presence of activity duration outliers
#' @export

detect_duration_outliers <- function(activity_log, activity_considered, bound_sd = 3, lower_bound = NA, upper_bound = NA, details = TRUE, filter_condition = NULL){

  # Predefine variables
  duration <- NULL
  activity <- NULL

  # Initiate warning variables
  warning.filtercondition <- FALSE

  # Check if the required columns are present in the log
  missing_columns <- check_colnames(activity_log, c("activity", "start", "complete"))
  if(!is.null(missing_columns)){
    stop("The following columns, which are required for the test, were not found in the activity log: ",
         paste(missing_columns, collapse = "\t"), ".", "\n  ",
         "Please check rename_activity_log.")
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

  # Select activity under consideration
  activity_log <- activity_log %>% filter(activity == activity_considered)

  # If there are no rows present in the log at this point, there is no need to keep going
  if(nrow(activity_log) == 0) {
    warning("The activity '", activity_considered, "' was not found in the log. Therefore, no outliers exist for that activity.")
    return(activity_log)
  }

  # Calculate durations
  activity_log$duration <- as.numeric(difftime(activity_log$complete, activity_log$start, units = "mins"))

  # Determine whether warning for negative durations is required
  if(nrow(activity_log %>% filter(duration < 0)) > 0){
    warning("Negative durations detected. Check function time_anomalies for more details.")
  }

  # Determine upper and lower bounds in case they are not specified
  if(is.na(lower_bound) & is.na(upper_bound)){
    lower_bound <- mean(activity_log$duration, na.rm = T) - bound_sd * sd(activity_log$duration, na.rm = T)
    if(lower_bound < 0){ # Correction in case a negative value is obtained
      lower_bound <- 0
    }
    upper_bound <- mean(activity_log$duration, na.rm = T) + bound_sd * sd(activity_log$duration, na.rm = T)
  }

  # Outlier determination
  outliers <- activity_log %>% filter(duration < lower_bound | duration > upper_bound)

  # Print output
  if(!is.null(filter_condition)) {
    cat("Applied filtering condition:", filter_condition, "\n", "\n")
  }

  cat("*** OUTPUT ***", "\n")
  cat("Outliers are detected for activity", activity_considered, "with lower bound", lower_bound, "and upper bound", upper_bound, ".", "\n", "\n")
  cat("A total of", nrow(outliers),"is detected (", nrow(outliers) / nrow(activity_log) * 100, "% of the activity executions)", "\n")

  if(details == TRUE){
    if(nrow(outliers) > 0){
      cat("For the following rows, outliers are detected:", "\n")
      return(outliers)
    }
  }
}
