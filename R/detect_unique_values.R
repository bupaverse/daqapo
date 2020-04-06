#' Search for unique values / distinct combinations
#'
#' Function that lists all distinct combinations of the given columns in the activity log
#' @inheritParams detect_activity_frequency_violations
#' @param column_labels The names of columns in the activity log for which you want to show the different combinations found in the log. If only one column is provided, this results in a list of unique values in that column.
#' @return A data frame containing the unique (distinct) values (combinations) in the requested column(s).
#' @examples
#' data("hospital_actlog")
#' detect_unique_values(activitylog = hospital_actlog,
#'      column_labels = "activity")
#' detect_unique_values(activitylog = hospital_actlog,
#'      column_labels = c("activity", "originator"))
#' @export

detect_unique_values <- function(activitylog, column_labels, filter_condition = NULL) {

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


  if(class(column_labels) != "character") {
    stop("column_labels must be a character vector")
  }

  if(!all(column_labels %in% names(activitylog))) {
    warning(glue("Some provided column labels don't exist and will be ignored: {str_c(column_labels[!(column_labels %in% names(activitylog))], collapse = ', ')}"))
    column_labels <- column_labels[(column_labels %in% names(activitylog))]
  }

  # Take the columns under investigation, compute their distinct values and arrange alphabetically

  output <- activitylog %>%
    select_at(.vars = column_labels) %>%
    distinct()


  if(length(column_labels) < 1) {
    stop("column_labels must be a vector of one or more valid column labels of type factor or character.")
  }

  # Prepare output

  message("*** OUTPUT ***")
  message("Distinct entries are computed for the following columns: \n", paste(column_labels, collapse = " - "))
  return(output)

}
