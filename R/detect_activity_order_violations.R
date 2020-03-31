#' Detect activity order violations
#'
#' Function detecting violations in activity order. Having additional or less activity types than those specified in activity_order is no violation, but the activity types present should occur in the specified order, and only once.
#' @inheritParams detect_activity_frequency_violations
#' @param activity_order Vector expressing the activity order that needs to be checked (using activity names)
#' @param timestamp Type of timestamp that needs to be taken into account in the analysis (either "start", "complete" or "both)
#' @return Information on the degree to which the specified activity order is respected/violated.
#' @export
#'

detect_activity_order_violations <- function(activitylog,
                                             activity_order,
                                             timestamp, details,
                                             filter_condition) {
  UseMethod("detect_activity_order_violations")
}

#' @describeIn detect_activity_order_violations Detect activity order_violations in activity log.
#' @export

detect_activity_order_violations.activitylog <- function(activitylog,
                                                         activity_order,
                                                         timestamp = c("both", "start","complete"),
                                                         details = TRUE,
                                                         filter_condition = NULL){


  timestamp <- match.arg(timestamp)
  # Predefine variables
  nr <- NULL
  overlapping <- NULL
  activity_list <- NULL
  case_id <- NULL
  activity <- NULL
  start <- NULL
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


  if(timestamp == "both") {
    overlaps <- detect_overlaps(activitylog, details = F, level_of_aggregation = "case")
    if(nrow(overlaps) > 0) {
      warning("Some activity instances within the same case overlap. Use detect_overlaps to investigate further.")
    }
  }

  incomplete <- suppressMessages(detect_incomplete_cases(activitylog, activities = activity_order))
  if(nrow(incomplete) > 0) {
    warning("Not all specified activities occur in each case. Use detect_incomplete_cases to investigate further.")
  }


  n_cases <- n_cases(activitylog)

  # Add number to ordered activities (used for ordering when multiple activities have the same timestamp for a particular case)

  tibble(!!activity_id_(activitylog) := activity_order) %>%
    mutate(nr = 1:n()) -> orders

  activitylog %>%
    filter(!!activity_id_(activitylog) %in% activity_order) -> filtered_activitylog

  filtered_activitylog %>%
    inner_join(orders, by = activity_id(activitylog)) -> activitylog_order


  # Determine activity order for each case
  if(timestamp == "both"){
    # Sort the activity log
    activitylog_order <- activitylog_order %>%
      group_by(!!case_id_(activitylog)) %>%
      arrange(start,complete, nr) %>%
      mutate(actual_nr = 1:n())
  } else if(timestamp == "start") {
    activitylog_order <- activitylog_order %>%
      group_by(!!case_id_(activitylog)) %>%
      arrange(start, nr) %>%
      mutate(actual_nr = 1:n())
  }  else{
    # Sort the activity log
      activitylog_order <- activitylog_order %>%
        group_by(!!case_id_(activitylog)) %>%
        arrange(complete, nr) %>%
        mutate(actual_nr = 1:n())
  }
  # Perform comparison

  activitylog_order %>%
    group_by(!!case_id_(activitylog)) %>%
    #incorrect if order is different, or
    mutate(nr = dense_rank(nr)) %>%
    summarize(incorrect = any(actual_nr != nr)) %>%
    filter(incorrect) -> incorrect_order

  activitylog_order %>%
    ungroup() %>%
    filter(!!case_id_(activitylog) %in% incorrect_order[[1]]) %>%
    group_by(!!case_id_(activitylog)) %>%
    arrange(actual_nr) %>%
    summarize(activity_list = paste(!!activity_id_(activitylog), collapse = " - ")) -> incorrect_order

  # Prepare output
  # stat_false <- nrow(incorrect_order) / nrow(activitylog) * 100
  stat_false <- round(nrow(incorrect_order) / n_cases * 100, 2)
  stat_true <- 100 - stat_false

  req_activity_order <- paste(activity_order, collapse = " - ")
  # Print output

  message("Selected timestamp parameter value: ", timestamp, "\n", "\n")

  message("*** OUTPUT ***")
  message("It was checked whether the activity order ", req_activity_order, " is respected.")
  message("This activity order is respected for ",
      # nrow(activity_log) - nrow(incorrect_order), "(", stat_true, "%) of the cases and not for",
      n_cases - nrow(incorrect_order), " (", stat_true, "%) of the cases and not for",
      nrow(incorrect_order), " (", stat_false, "%) of the cases.")

  if(details == TRUE){
    if(stat_false > 0){
      message("For cases for which the aformentioned activity order is not respected, the following order is detected (ordered by decreasing frequeny of occurrence):", "\n")
      incorrect_order_summary <- incorrect_order %>% group_by(activity_list) %>% summarize(n = n(), case_ids = paste(!!case_id_(activitylog), collapse = " - ")) %>% arrange(desc(n))
      return(incorrect_order_summary)
    }
  }
}
