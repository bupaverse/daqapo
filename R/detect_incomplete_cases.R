#' Detect incomplete cases
#'
#' Function detecting incomplete cases in terms of the activities that need to be recorded for a case. The function only checks the presence of activities, not the completeness of the rows describing the activity executions.
#' @inheritParams detect_activity_frequency_violations
#' @param activities A vector of activity names which should be present for a case
#' @return Information on the presence/absence of incomplete cases.
#' @examples
#' \dontrun{
#' data("hospital_actlog")
#' detect_incomplete_cases(activitylog = hospital_actlog,
#'      activities = c("Registration","Triage","Clinical exam","Treatment","Treatment evaluation"))
#' }
#' @export
#'
#'
detect_incomplete_cases <- function(activitylog, activities, details, filter_condition) {
  UseMethod("detect_incomplete_cases")
}
#' @export
#'
detect_incomplete_cases.activitylog <- function(activitylog, activities, details = TRUE, filter_condition = NULL){

  # Predefine variables
  activity_list <- NULL
  case_id <- NULL
  activity <- NULL
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

  # Filter out activities which are not part of activity_vector
  # activity_log <- activity_log %>% filter(activity %in% activity_vector)

  # Determine activities executed for each case
  #    OBSOLETE - this duplicates repeated activities in the activity_list. For a case to be deemed complete, repetition is not an issue
  #    activity_log <- activity_log %>% group_by(case_id) %>% arrange(activity) %>% summarize(activity_list = paste(activity, collapse = " - "))


  # Determine deviations between required activities and recorded activities
  # req_act_list <- paste(sort(activity_vector), collapse = " - ")
  # incomplete <- activity_log %>% filter(!(activity_list == req_act_list))

  activitylog %>%
    filter_activity_presence(activities = activities, method = "all", reverse = T) -> incomplete

  # Prepare output
  stat_false <- round(nrow(incomplete) / nrow(activitylog) * 100, 2)
  stat_true <- 100 - stat_false

  # Print output

  message("*** OUTPUT ***")
  message("It was checked whether the activities ", paste(sort(activities), collapse = ", "), " are present for cases.")
  message("These activities are present for ",
      n_cases(activitylog) - n_cases(incomplete), " (", stat_true, "%) of the cases and are not present for ",
      n_cases(incomplete), " (", stat_false, "%) of the cases.")
  message("Note: this function only checks the presence of activities for a particular case, not the completeness of these entries in the activity log or the order of activities.", "\n")

  if(details == TRUE){
    if(stat_false > 0){
      message("For cases for which the aforementioned activities are not all present, the following activities are recorded (ordered by decreasing frequeny of occurrence):", "\n")
      incomplete_summary <- incomplete %>% group_by(!!activity_id_(activitylog)) %>% summarize(n = n(), case_ids = paste(!!case_id_(activitylog), collapse = " - ")) %>% arrange(desc(n))
      return(incomplete_summary)
    }
  }
}
