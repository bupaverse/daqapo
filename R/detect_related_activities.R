#' Detect missing related activities
#'
#' Function detecting missing related activity registration, i.e. detecting activities that should be registered for a case because another activity is registered for that case
#' @inheritParams detect_activity_frequency_violations
#' @param antecedent Activity name of the activity that acts as a an antecedent (if antecedent occurs, then consequent should also occur)
#' @param consequent Activity name of the activity that acts as a an consequent (if antecedent occurs, then consequent should also occur)
#' @return Information on the presence/absence of related activities.
#' @examples
#' data("hospital_actlog")
#' detect_related_activities(activitylog = hospital_actlog,
#'      antecedent = "Treatment evaluation",
#'      consequent = "Treatment")
#' @export

detect_related_activities <- function(activitylog, antecedent, consequent, details, filter_condition) {
  UseMethod("detect_related_activities")
}

#' @export

detect_related_activities.activitylog <- function(activitylog, antecedent, consequent, details = TRUE, filter_condition = NULL){

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

  # Determine cases for which antecedent is recorded
  ac1_cases <- activitylog %>% filter_activity_presence(activities = antecedent) %>% case_labels()

  # Determine cases in which antecedent is present, but consequent isn't
  ac2_cases <- activitylog %>% filter_activity_presence(activities = consequent) %>% case_labels()

  only_ac1 <- setdiff(ac1_cases, ac2_cases)

  # Prepare output
  stat_false <- round(length(only_ac1) / length(ac1_cases) * 100, 2)
  stat_true <- 100 - stat_false

  # Print output

  message("*** OUTPUT ***")
  message("The following statement was checked: if ", antecedent, " is recorded for a case, then ", consequent, " should also be recorded.")
  message("This statement holds for ",
      length(ac1_cases) - length(only_ac1), " (", stat_true, "%) of the cases in which ", antecedent, " was recorded and does not hold for ",
      length(only_ac1), " (", stat_false, "%) of the cases in which ", antecedent, " was recorded.")

  if(details == TRUE){
    if(stat_false > 0){
      message("For the following cases, only ", antecedent, " is recorded:")
      return(only_ac1)
    }
  }
}
