#' Detect incomplete cases
#'
#' Function detecting incomplete cases in terms of the activities that need to be recorded for a case. The function only checks the presence of activities, not the completeness of the rows describing the activity executions.
#' @param activity_log The activity log (renamed/formatted using functions rename_activity_log and convert_timestamp_format)
#' @param activity_vector A vector of activity names which should be present for a case
#' @param details Boolean indicating wheter details of the results need to be shown
#' @param filter_condition Condition that is used to extract a subset of the activity log prior to the application of the function
#' @return Information on the presence/absence of incomplete cases.
#' @export
incomplete_cases <- function(activity_log, activity_vector, details = TRUE, filter_condition = NULL){

  # Apply filter condition when specified
  if(!is.null(filter_condition)) {
    activity_log <- activity_log %>% filter_(filter_condition)
  }

  # Filter out activities which are not part of activity_vector
  activity_log <- activity_log %>% filter(activity %in% activity_vector)

  # Determine activities executed for each case
  activity_log <- activity_log %>% group_by(case_id) %>% arrange(activity) %>% summarize(activity_list = paste(activity, collapse = " - "))

  # Determine deviations between required activities and recorded activities
  req_act_list <- paste(sort(activity_vector), collapse = " - ")
  incomplete <- activity_log %>% filter(!(activity_list == req_act_list))

  # Prepare output
  stat_false <- nrow(incomplete) / nrow(activity_log) * 100
  stat_true <- 100 - stat_false

  # Print output
  if(!is.null(filter_condition)) {
    cat("Applied filtering condition", filter_condition, "\n", "\n")
  }

  cat("*** OUTPUT ***", "\n")
  cat("It was checked whether the activities", req_act_list, "are present for cases.", "\n")
  cat("These activities are present for",
      nrow(activity_log) - nrow(incomplete), "(", stat_true, "%) of the cases and are not present for",
      nrow(incomplete), "(", stat_false, "%) of the cases.", "\n", "\n")
  cat("Note: this function only checks the presence of activities for a particular case, not the completness of these entries in the activity log or the order of activities.", "\n", "\n")

  if(details == TRUE){
    if(stat_false > 0){
      cat("For cases for which the aforementioned activities are not all present, the following activities are recorded (ordered by decreasing frequeny of occurrence):", "\n")
      incomplete_summary <- incomplete %>% group_by(activity_list) %>% summarize(n = n(), case_ids = paste(case_id, collapse = " - ")) %>% arrange(desc(n))
      return(incomplete_summary)
    }
  }
}
