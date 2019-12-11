#' Detect dependency violations between attributes
#'
#' Function detecting violations of dependencies between attributes (i.e. condition(s) that should hold when (an)other condition(s) hold(s))
#' @inheritParams detect_activity_frequency_violations
#' @param antecedent (Vector of) condition(s) which serve as an antecedent (if the condition(s) in antecedent hold, then the condition(s) in consequent should also hold)
#' @param consequent (Vector of) condition(s) which serve as a consequent (if the condition(s) in antecedent hold, then the condition(s) in consequent should also hold)
#' @return Information on the degree to which the specified dependencies are respected/violated.
#' @export
detect_attribute_dependencies <- function(activitylog, antecedent, consequent, details = TRUE, filter_condition = NULL, ...){

  # Initiate warning variables
  filter_specified <- FALSE
  error.cond1 <- FALSE
  error.cond2 <- FALSE


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
  # Quote antecedent and consequent
  antecedent <- enquo(antecedent)
  consequent <- enquo(consequent)

  # Check rows in activity log for which conditions in condition_vector1 holds
  tryCatch({
    activity_log_cond1 <- activitylog %>% filter(!!(antecedent))
  }, error = function(e) {
    error.cond1 <<- TRUE
  })

  if(error.cond1) {
    stop("The first condition vector (", expr_text(antecedent), ") is not valid. Check the syntax and column names.")
  }

  # Check rows for which both condition_vector1 and condition_vector2 holds
  tryCatch({
    activity_log_cond12 <- activity_log_cond1 %>% filter(!!(consequent))
  }, error = function(e) {
    error.cond2 <<- TRUE
  })

  if(error.cond2) {
    stop("The second condition vector (", expr_text(consequent), ") is not valid. Check the syntax and column names.")
  }

  # Prepare output
  stat_true <- nrow(activity_log_cond12) / nrow(activity_log_cond1) * 100
  stat_false <- 100 - stat_true

  # Print output
  message("*** OUTPUT ***")
  message("The following statement was checked: if condition(s) ", expr_text(antecedent), " hold(s), then ", expr_text(consequent), " should also hold.")
  message("This statement holds for ",
      nrow(activity_log_cond12), " (", round(stat_true, 2), "%) of the rows in the activity log for which the first condition(s) hold and does not hold for ",
      nrow(activity_log_cond1) - nrow(activity_log_cond12), " (", round(stat_false, 2), "%) of these rows.", "\n")

  if(details == TRUE){
    if(stat_false > 0){
      message("For the following rows, the first condition(s) hold(s), but the second condition does not:")
      return(dplyr::setdiff(activity_log_cond1,activity_log_cond12))
    }
  }
}
