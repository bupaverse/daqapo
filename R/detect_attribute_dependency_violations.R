#' Detect dependency violations between attributes
#'
#' Function detecting violations of dependencies between attributes (i.e. condition(s) that should hold when (an)other condition(s) hold(s))
#' @param activity_log The activity log (renamed/formatted using functions rename_activity_log and convert_timestamp_format)
#' @param antecedent (Vector of) condition(s) which serve as an antecedent (if the condition(s) in antecedent hold, then the condition(s) in consequent should also hold)
#' @param consequence (Vector of) condition(s) which serve as a consequent (if the condition(s) in antecedent hold, then the condition(s) in consequent should also hold)
#' @param details Boolean indicating wheter details of the results need to be shown
#' @param filter_condition Condition that is used to extract a subset of the activity log prior to the application of the function
#' @return Information on the degree to which the specified dependencies are respected/violated.
#' @export
detect_attribute_dependencies <- function(activity_log, antecedent, consequent, details = TRUE, filter_condition = NULL, ...){

  # Initiate warning variables
  filter_specified <- FALSE
  error.cond1 <- FALSE
  error.cond2 <- FALSE

  tryCatch({
    is.null(filter_condition)
  }, error = function(e) {
    filter_specified <<- TRUE
  }
  )
  # Apply filter condition when specified
  if(!filter_specified) {
    # geen filter gespecifieerd.

  } else {
    filter_condition_q <- enquo(filter_condition)
    activity_log <- APPLY_FILTER(activity_log, filter_condition_q = filter_condition_q)

  }
  # Quote antecedent and consequent
  antecedent <- enquo(antecedent)
  consequent <- enquo(consequent)

  # Check rows in activity log for which conditions in condition_vector1 holds
  tryCatch({
    activity_log_cond1 <- activity_log %>% filter(!!(antecedent))
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
  cat("*** OUTPUT ***", "\n")
  cat("The following statement was checked: if condition(s)", expr_text(antecedent), "hold(s), then", expr_text(consequent), "should also hold", "\n", "\n")
  cat("This statement holds for",
      nrow(activity_log_cond12), "(", stat_true, "%) of the rows in the activity log for which the first condition(s) hold and does not hold for",
      nrow(activity_log_cond1) - nrow(activity_log_cond12), "(", stat_false, "%) of these rows.", "\n", "\n")

  if(details == TRUE){
    if(stat_false > 0){
      cat("For the following rows, the first condition(s) hold(s), but the second condition does not:", "\n")
      return(dplyr::setdiff(activity_log_cond1,activity_log_cond12))
    }
  }
}
