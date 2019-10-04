#' Detect dependency violations between attributes
#'
#' Function detecting violations of dependencies between attributes (i.e. condition(s) that should hold when (an)other condition(s) hold(s))
#' @param activity_log The activity log (renamed/formatted using functions rename_activity_log and convert_timestamp_format)
#' @param condition_vector1 Vector of condition(s) which serve as an antecedent (if the condition(s) in condition_vector1 hold, then the condition(s) in condition_vector2 should also hold)
#' @param condition_vector2 Vector of condition(s) which serve as a consequent (if the condition(s) in condition_vector1 hold, then the condition(s) in condition_vector2 should also hold)
#' @param details Boolean indicating wheter details of the results need to be shown
#' @param filter_condition Condition that is used to extract a subset of the activity log prior to the application of the function
#' @return Information on the degree to which the specified dependencies are respected/violated.
#' @export
attribute_dependency <- function(activity_log, condition_vector1, condition_vector2, details = TRUE, filter_condition = NULL){

  # Initiate warning variables
  warning.filtercondition <- FALSE
  error.cond1 <- FALSE
  error.cond2 <- FALSE

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

  # Concatenate condition_vector1 and condition_vector2
  condition_vector1 <- paste(condition_vector1, collapse = " & ")
  condition_vector2 <- paste(condition_vector2, collapse = " & ")

  # Check rows in activity log for which conditions in condition_vector1 holds
  tryCatch({
    activity_log_cond1 <- activity_log %>% filter_(condition_vector1)
  }, error = function(e) {
    error.cond1 <<- TRUE
  })

  if(error.cond1) {
    stop("The first condition vector (", condition_vector1, ") is not valid. Check the syntax and column names.")
  }

  # Check rows for which both condition_vector1 and condition_vector2 holds
  tryCatch({
    activity_log_cond12 <- activity_log_cond1 %>% filter_(condition_vector2)
  }, error = function(e) {
    error.cond2 <<- TRUE
  })

  if(error.cond2) {
    stop("The second condition vector (", condition_vector2, ") is not valid. Check the syntax and column names.")
  }

  # Prepare output
  stat_true <- nrow(activity_log_cond12) / nrow(activity_log_cond1) * 100
  stat_false <- 100 - stat_true

  # Print output
  if(!is.null(filter_condition)) {
    cat("Applied filtering condition:", filter_condition, "\n", "\n")
  }

  cat("*** OUTPUT ***", "\n")
  cat("The following statement was checked: if condition(s)", condition_vector1, "hold(s), then", condition_vector2, "should also hold", "\n", "\n")
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
