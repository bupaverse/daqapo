#' Detect conditional activity presence violations
#'
#' Function detecting violations of conditional activity presence (i.e. an activity/activities that should be present when (a) particular condition(s) hold(s))
#' @param condition_vector Vector of condition(s) which serve as an antecedent (if the condition(s) in condition_vector hold, then the activity/activities in activity_vector should be recorded)
#' @param activity_vector Vector of activity/activities which serve as a consequent (if the condition(s) in condition_vector hold, then the activity/activities in activity_vector should be recorded)
#' @inheritParams detect_activity_frequency_violations
#' @return Information on the degree to which the specified conditional activity presence is respected/violated.
#' @export

detect_conditional_activity_presence <- function(activity_log, condition_vector, activity_vector, details = TRUE, filter_condition = NULL){

  # Predefine variables
  case_id <- NULL
  activity <- NULL

  # Initiate warning variables
  warning.filtercondition <- FALSE
  error.conditionfilter <- FALSE

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

  # Concatenate condition_vector
  condition_vector <- paste(condition_vector, collapse = " & ")

  # Determine cases in activity log for which conditions in condition_vector holds
  tryCatch({
    cases_cond_satisfied <- unique((activity_log %>% filter(!! rlang::parse_expr(condition_vector)))$case_id)
  }, error = function(e) {
    error.conditionfilter <<- TRUE
  })

  if(error.conditionfilter) {
    stop("The condition vector (", condition_vector, ") is not valid. Check the syntax and column names.")
  }

  # Determine whether activities in activity_vector are recorded for cases in cases_cond_satisfied
  activity_log <- activity_log %>% filter(case_id %in% cases_cond_satisfied, activity %in% activity_vector) %>%
    group_by(case_id) %>% arrange(activity) %>% summarize(n = n())

  # Prepare output
  if(length(activity_vector) == 1){
    violated <- setdiff(cases_cond_satisfied, activity_log$case_id)
  } else{
    activities_recorded <- activity_log %>% filter(n == length(activity_vector))
    violated <- setdiff(cases_cond_satisfied, activities_recorded$case_id)
  }

  stat_false <- length(violated) / length(cases_cond_satisfied) * 100
  stat_true <- 100 - stat_false

  # Print output
  if(!is.null(filter_condition)) {
    cat("Applied filtering condition:", filter_condition, "\n", "\n")
  }

  cat("*** OUTPUT ***", "\n")
  cat("The following statement was checked: if condition(s)", condition_vector, "hold(s), then activity/activities", activity_vector, "should be recorded", "\n", "\n")
  cat("The condition(s) hold(s) for", length(cases_cond_satisfied), "cases. From these cases:", "\n")
  cat("- the specified activity/activities is/are recorded for", length(cases_cond_satisfied) - length(violated), "cases (", stat_true, "%)", "\n")
  cat("- the specified activity/activities is/are not recorded for", length(violated), "cases (", stat_false, "%)", "\n", "\n")

  if(details == TRUE){
    if(stat_false > 0){
      cat("For the following cases, the condition(s) hold(s) but", activity_vector, "is/are not recorded:", "\n")
      return(violated)
    }
  }
}
