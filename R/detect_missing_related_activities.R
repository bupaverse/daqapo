#' Detect missing related activities
#'
#' Function detecting missing related activity registration, i.e. detecting activities that should be registered for a case because another activity is registered for that case
#' @param activity_log The activity log (renamed/formatted using functions rename_activity_log and convert_timestamp_format)
#' @param activity1 Activity name of the activity that acts as a an antecedent (if activity1 occurs, then activity2 should also occur)
#' @param activity2 Activity name of the activity that acts as a an consequent (if activity1 occurs, then activity2 should also occur)
#' @param details Boolean indicating wheter details of the results need to be shown
#' @param filter_condition Condition that is used to extract a subset of the activity log prior to the application of the function
#' @return Information on the presence/absence of related activities.
#' @export

detect_related_activities <- function(activity_log, activity1, activity2, details = TRUE, filter_condition = NULL){

  # Predefine variables
  activity <- NULL

  # Initiate warning variables
  warning.filtercondition <- FALSE

  # Check if the required columns are present in the log
  missing_columns <- check_colnames(activity_log, c("case_id", "activity"))
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

  # Determine cases for which activity1 is recorded
  ac1_cases <- (activity_log %>% filter(activity == activity1))$case_id

  # Determine cases in which activity1 is present, but activity2 isn't
  ac2_cases <- (activity_log %>% filter(activity == activity2))$case_id
  only_ac1 <- setdiff(ac1_cases, ac2_cases)

  # Prepare output
  stat_false <- length(only_ac1) / length(ac1_cases) * 100
  stat_true <- 100 - stat_false

  # Print output
  if(!is.null(filter_condition)) {
    cat("Applied filtering condition:", filter_condition, "\n", "\n")
  }

  cat("*** OUTPUT ***", "\n")
  cat("The following statement was checked: if", activity1, "is recorded for a case, then", activity2, "should also be recorded.", "\n")
  cat("This statement holds for",
      length(ac1_cases) - length(only_ac1), "(", stat_true, "%) of the cases in which", activity1, "was recorded and does not hold for",
      length(only_ac1), "(", stat_false, "%) of the cases in which", activity1, "was recorded.", "\n", "\n")

  if(details == TRUE){
    if(stat_false > 0){
      cat("For the following cases, only", activity1, "is recorded:", "\n")
      return(only_ac1)
    }
  }
}
