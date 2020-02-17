#' Detect activity order violations
#'
#' Function detecting violations in activity order
#' @inheritParams detect_activity_frequency_violations
#' @param activity_order Vector expressing the activity order that needs to be checked (using activity names)
#' @param timestamp Type of timestamp that needs to be taken into account in the analysis (either "start", "complete" or "both)
#' @return Information on the degree to which the specified activity order is respected/violated.
#' @export
#'

detect_activity_order_violations <- function(activitylog,
                                             activity_order,
                                             timestamp, details, filter_condition) {
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

  n_cases <- n_cases(activitylog)

  # Filter activities included in activity_order
  activitylog <- activitylog %>% filter_activity(activities = activity_order)

  # Add number to ordered activities (used for ordering when multiple activities have the same timestamp for a particular case)
  activity_order <- as.data.frame(activity_order)
  activity_order$nr <- seq(1, nrow(activity_order))
  colnames(activity_order)[1] <- activity_id(activitylog)
  activitylog <- merge(activitylog,activity_order, by  = activity_id(activitylog))

  # Detect time overlap between consecutive activities in case both timestamps are used
  if(timestamp == "both" & details == TRUE){
    # Sort the activity log
    activitylog <- activitylog %>% arrange(case_id, start,complete, nr)
    # Determine whether activities are overlapping
    #activity_log$start <- as.character(activity_log$start)
    #activity_log$complete <- as.character(activity_log$complete)
    #activity_log$prior_start <- c(NA, activity_log$start[-nrow(activity_log)])
    #activity_log$prior_complete <- c(NA, activity_log$complete[-nrow(activity_log)])
    #activity_log$prior_case <- c(NA, activity_log$case_id[-nrow(activity_log)])
    #activity_log$prior_activity <- c(NA, activity_log$activity[-nrow(activity_log)])
    #activity_log$start <- ymd_hms(activity_log$start)
    #activity_log$complete <- ymd_hms(activity_log$complete)
    #activity_log$prior_start <- ymd_hms(activity_log$prior_start)
    #activity_log$prior_complete <- ymd_hms(activity_log$prior_complete)

    activitylog <- activitylog %>%
      mutate(
        prior_start = lag(start),
        prior_complete = lag(complete),
        prior_case = lag(case_id),
        prior_activity = lag(activity)
      )

    activitylog$time_overlap <- activitylog$case_id == activitylog$prior_case & activitylog$prior_complete > activitylog$start
    activitylog$time_overlap[1] <- FALSE
    activitylog$overlapping <- NA

    index <- is.na(activitylog$time_overlap)  # NA values are generated when there are missing timestamps. In that case, no overlaps can be detected.
    activitylog$time_overlap[index] <- FALSE
    remove(index)

    index <- activitylog$time_overlap == TRUE
    activitylog$overlapping[index] <- paste(activitylog$prior_activity[index], "and", activitylog$activity[index])
    remove(index)

    time_overlaps <- activitylog %>% filter(!is.na(overlapping)) %>% group_by(overlapping) %>% summarize(n = n()) %>% arrange(desc(n))
  }

  # Determine activity order for each case
  if(timestamp %in% c("both", "start")){
    # Sort the activity log
    activitylog <- activitylog %>% arrange(case_id, start,complete, nr)
    # Check whether activity order is satisfied
    activitylog <- activitylog %>% group_by(case_id) %>% summarize(activity_list = paste(activity, collapse = " - "))
  } else{
    # Sort the activity log
    activitylog <- activitylog %>% arrange(case_id, complete, start, nr)
    # Check whether activity order is satisfied
    activitylog <- activitylog %>% group_by(case_id) %>% summarize(activity_list = paste(activity, collapse = " - "))
  }

  # Perform comparison
  req_activity_order <- paste(activity_order$activity, collapse = " - ")
  incorrect_order <- activitylog %>% filter(!(activity_list == req_activity_order))

  # Prepare output
  # stat_false <- nrow(incorrect_order) / nrow(activitylog) * 100
  stat_false <- nrow(incorrect_order) / n_cases * 100
  stat_true <- 100 - stat_false

  # Print output

  cat("Selected timestamp parameter value:", timestamp, "\n", "\n")

  cat("*** OUTPUT ***", "\n")
  cat("It was checked whether the activity order", req_activity_order, "is respected.", "\n")
  cat("This activity order is respected for",
      # nrow(activity_log) - nrow(incorrect_order), "(", stat_true, "%) of the cases and not for",
      n_cases - nrow(incorrect_order), "(", stat_true, "%) of the cases and not for",
      nrow(incorrect_order), "(", stat_false, "%) of the cases.", "\n", "\n")

  cat("Note: cases for which not all activities are recorded, will be considered as cases for which an incorrect order is registered.", "\n", "\n")


  if(details == TRUE){
    if(timestamp == "both"){
      cat("Time overlap is detected between the following consecutive activities (ordered by decreasing frequency of occurrence):", "\n")
      print(time_overlaps)
    }
    if(stat_false > 0){
      cat("\n")
      cat("For cases for which the aformentioned activity order is not respected, the following order is detected (ordered by decreasing frequeny of occurrence):", "\n")
      incorrect_order_summary <- incorrect_order %>% group_by(activity_list) %>% summarize(n = n(), case_ids = paste(case_id, collapse = " - ")) %>% arrange(desc(n))
      return(incorrect_order_summary)
    }
  }
}
