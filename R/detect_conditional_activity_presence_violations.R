#' Detect conditional activity presence violations
#'
#' Function detecting violations of conditional activity presence (i.e. an activity/activities that should be present when (a) particular condition(s) hold(s))
#' @param condition Condition which serve as an antecedent (if the condition in condition holds, then the activit(y)(ies) in activities should be present.)
#' @param activities Vector of activity/activities which serve as a consequent (if the condition(s) in condition_vector hold, then the activity/activities in activity_vector should be recorded)
#' @inheritParams detect_activity_frequency_violations
#' @return Information on the degree to which the specified conditional activity presence is respected/violated.
#'
#' @export


detect_conditional_activity_presence <- function(activitylog, condition, activities, details, filter_condition ){
 UseMethod("detect_conditional_activity_presence")
}
#' @export
detect_conditional_activity_presence.activitylog <- function(activitylog, condition, activities, details = TRUE, filter_condition = NULL){

  # Predefine variables
  case_id <- NULL
  activity <- NULL

  # Initiate warning variables
  warning.filtercondition <- FALSE
  error.conditionfilter <- FALSE

  if(any(!(activities %in% activity_labels(activitylog)))) {
    warning(glue("The following activities do not occur in the data, and are dropped: {str_c(activities[[!(activities %in% activity_labels(activitylog)]]), collapse = ', ')}"))

    activities <- activities[[activities %in% activity_labels(activitylog)]]
  }

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

  # Concatenate condition_vector
  condition <- enquo(condition)

  # Determine cases in activity log for which conditions in condition_vector holds
  tryCatch({
    cases_cond_satisfied <- activitylog %>% filter(!!(condition)) %>% case_labels()
  }, error = function(e) {
    error.conditionfilter <<- TRUE
  })

  if(error.conditionfilter) {
    stop("The condition vector (", expr_text(condition), ") is not valid. Check the syntax and column names.")
  }

  # Determine whether activities in activities are recorded for cases in cases_cond_satisfied
  activitylog_summary <- activitylog %>%
    filter_case(cases_cond_satisfied) %>%
    filter_activity(activities) %>%
    group_by(!!case_id_(activitylog)) %>% summarize(n = n())

  all_activities_recorded <- activitylog_summary %>% filter(n == length(activities))
  violated <- setdiff(cases_cond_satisfied, all_activities_recorded[[case_id(activitylog)]])

  stat_false <- length(violated) / length(cases_cond_satisfied) * 100
  stat_true <- 100 - stat_false

  message("*** OUTPUT ***")
  message("The following statement was checked: if condition(s) ", expr_text(condition), " hold(s), then activity/activities ", str_c(activities, collapse = ", "), " should be recorded", "\n")
  message("The condition(s) hold(s) for ", length(cases_cond_satisfied), " cases. From these cases:")
  message("- the specified activity/activities is/are recorded for ", length(cases_cond_satisfied) - length(violated), " case(s) (", stat_true, "%)")
  message("- the specified activity/activities is/are not recorded for ", length(violated), " case(s) (", stat_false, "%)", "\n")

  if(details == TRUE){
    if(stat_false > 0){
      message("For the following cases, the condition(s) hold(s) but (some) activit(y)(ies) is/are not recorded:", "\n")
      return(violated)
    }
  }
}
