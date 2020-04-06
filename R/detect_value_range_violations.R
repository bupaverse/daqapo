#' Detect value range violations
#'
#' Function detecting violations of the value range, i.e. values outside the range of tolerable values
#' @inheritParams detect_activity_frequency_violations
#' @param ... Define domain range using domain_numeric, domain_categorical and/or domain_time for each column
#' @return Information on the presence of attribute range violations in the specified column
#' @examples
#' \dontrun{
#' data("hospital_actlog")
#' detect_value_range_violations(activitylog = hospital_actlog,
#'      triagecode = domain_numeric(from = 0, to = 5))
#' }
#' @seealso \code{\link{domain_categorical}},\code{\link{domain_time}},\code{\link{domain_numeric}}
#' @importFrom glue glue
#' @export
#'
detect_value_range_violations <- function(activitylog, ..., details, filter_condition) {
  UseMethod("detect_value_range_violations")
}
#' @export
detect_value_range_violations.activitylog <- function(activitylog, ... , details = TRUE, filter_condition = NULL){


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


  params <- list(...)

  classes <- map(params, ~class(.x)[1]) %>% unlist()
  if(any(classes != "value_range")) {
    stop("Domains should be defined with domain_ functions.")
  }

  columns <- names(params)
  if(any(!(columns %in% names(activitylog)))) {
    warning(glue::glue("The following columns are not found and ignored: {str_c(columns[!(columns %in% names(activitylog))], collapse = ', ')}. Did you spelled them wrong?"))
    columns <- columns[(columns %in% names(activitylog))]
  }

  violated <- vector(mode = "list", length = length(params))

  message("*** OUTPUT ***")
  for(i in seq_along(params)) {
    type <- params[[i]]$type
    column <- names(params)[i]
    FUN <- switch(type,
                  numeric = check_domain_numeric,
                  categorical = check_domain_character,
                  time = check_domain_time)

    violated[[i]] <- FUN(activitylog, column, params[[i]])
  message("")
  }

  violated <- bind_rows(violated)
  if(details == TRUE){
    if(nrow(violated) > 0){
      message("The following rows fall outside the specified domain range for indicated column:")
      return(violated)
    }
  }
}

check_domain_time <- function(activitylog, column, domain_range) {
  column_checked <- NULL
  activitylog %>%
    filter(is.na(!!sym(column)) | !!sym(column) < domain_range$from | !!sym(column) > domain_range$to) -> violated

  # Prepare output
  stat_outside <- round(nrow(violated) / nrow(activitylog) * 100, 2)
  stat_inside <- round(100 - stat_outside, 2)

  message("The domain range for column ", column, " is checked.")
  message(glue("Values allowed between {domain_range$from} and {domain_range$to}"))
  message("The values fall within the specified domain range for ",
      nrow(activitylog) - nrow(violated), " (", stat_inside, "%) of the rows in the activity log and outside the domain range for ",
      nrow(violated), " (", stat_outside, "%) of these rows.")
  violated %>%
    mutate(column_checked = column) %>%
    select(column_checked, everything())

}

check_domain_numeric <- function(activitylog, column, domain_range) {
  column_checked <- NULL

  activitylog %>%
    filter(is.na(!!sym(column)) | !between(!!sym(column), domain_range$from, domain_range$to)) -> violated

  stat_outside <- round(nrow(violated) / nrow(activitylog) * 100, 2)
  stat_inside <- round(100 - stat_outside, 2)

  message("The domain range for column ", column, " is checked.")
  message(glue("Values allowed between {domain_range$from} and {domain_range$to}"))
  message("The values fall within the specified domain range for ",
          nrow(activitylog) - nrow(violated), " (", stat_inside, "%) of the rows in the activity log and outside the domain range for ",
          nrow(violated), " (", stat_outside, "%) of these rows.")

  violated %>%
    mutate(column_checked = column) %>%
    select(column_checked, everything())
}

check_domain_character <- function(activitylog, column, domain_range) {
  column_checked <- NULL

  activitylog %>%
    filter(is.na(!!sym(column)) | !(!!sym(column) %in% domain_range$allowed)) -> violated

  stat_outside <- round(nrow(violated) / nrow(activitylog) * 100, 2)
  stat_inside <- round(100 - stat_outside, 2)

  message("The domain range for column ", column, " is checked.")
  message(glue("Values allowed: {str_c(domain_range$allowed, collapse = ', ')}"))
  message("The values fall within the specified domain range for ",
          nrow(activitylog) - nrow(violated), " (", stat_inside, "%) of the rows in the activity log and outside the domain range for ",
          nrow(violated), " (", stat_outside, "%) of these rows.")
  violated %>%
    mutate(column_checked = column) %>%
    select(column_checked, everything())
}
