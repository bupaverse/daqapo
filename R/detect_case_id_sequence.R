#' Detect gaps in case_id
#'
#' Function detecting gaps in the sequence of case identifiers
#' @inheritParams detect_activity_frequency_violations
#' @return data.frame providing an overview of the case identifiers which are expected, but which are not present in the activity log
#' @examples
#' \donttest{
#' data("hospital_actlog")
#' detect_case_id_sequence_gaps(activitylog = hospital_actlog)
#' }
#' @importFrom tibble tibble
#' @export

detect_case_id_sequence_gaps <- function(activitylog, details, filter_condition) {
  UseMethod("detect_case_id_sequence_gaps")
}

#' @export

detect_case_id_sequence_gaps.activitylog <- function(activitylog, details = TRUE, filter_condition = NULL) {

  # Predefine variables
  case <- NULL
  present <- NULL
  case_id <- NULL

  # Initiate warning variables
  warning.asnumeric <- FALSE

  # Check if case_id is a numeric column
  if(!class(activitylog[[case_id(activitylog)]]) %in% c("numeric", "integer")){
    tryCatch({
      activitylog <- mutate_at(.tbl = activitylog, .vars = case_id(activitylog), .funs = as.numeric) %>%
        re_map(mapping(activitylog))
    }, warning = function(e) {
      warning.asnumeric <<- TRUE
    }
    )
    if(warning.asnumeric) {
      stop("The case IDs in the activity log cannot be converted to numeric. Therefore, it is not possible to perform this test on the dataset.")
    }

  }

  filter_specified <- FALSE
  # Apply filter condition when specified
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
    activitylog <- APPLY_FILTER(activitylog, filter_condition_q = filter_condition_q)
  }
  # Detect gaps
  first <- min(activitylog[[case_id(activitylog)]])
  last <- max(activitylog[[case_id(activitylog)]])
  cases <- sort(unique(activitylog[[case_id(activitylog)]]))

  tibble(cases) %>%
    mutate(diff = lead(cases) - cases) %>%
    filter(diff > 1) %>%
    mutate(from = cases + 1, to = cases + diff - 1, n_missing = diff - 1) %>%
    select(-diff, -cases) -> cases

  # Prepare output
  n_missing_case_ids <- sum(cases$n_missing)
  n_expected_cases <- last - first + 1

  # Print output

  message("*** OUTPUT ***", "\n")
  message("It was checked whether there are gaps in the sequence of case IDs", "\n")
  message(glue("From the {n_expected_cases} expected cases in the activity log, ranging from {first} to {last}, {n_missing_case_ids} ({round(100*n_missing_case_ids/n_expected_cases, 2)}%) are missing."))

  if(details == TRUE & n_missing_case_ids > 0){
    message("These missing case numbers are:\n")
    return(cases)
  }
}
