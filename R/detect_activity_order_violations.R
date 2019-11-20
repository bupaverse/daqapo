#' Detect activity order violations
#'
#' Function detecting violations in activity order
#' @param activity_log The activity log (renamed/formatted using functions rename_activity_log and convert_timestamp_format)
#' @param activity_order Vector expressing the activity order that needs to be checked (using activity names)
#' @param timestamp Type of timestamp that needs to be taken into account in the analysis (either "start", "complete" or "both)
#' @param details Boolean indicating wheter details of the results need to be shown
#' @param filter_condition Condition that is used to extract a subset of the activity log prior to the application of the function
#' @return Information on the degree to which the specified activity order is respected/violated.
#' @export
detect_activity_order_violations <- function(activity_log, activity_order, timestamp = "both", details = TRUE, filter_condition = NULL){

  # Predefine variables
  nr <- NULL
  overlapping <- NULL
  activity_list <- NULL
  case_id <- NULL
  activity <- NULL
  start <- NULL
  complete <- NULL

  # Initiate warning variables
  warning.filtercondition <- FALSE

  # Check if the required columns are present in the log
  missing_columns <- check_colnames(activity_log, c("case_id", "activity", "start", "complete"))
  if(!is.null(missing_columns)){
    stop("The following columns, which are required for the test, were not found in the activity log: ",
         paste(missing_columns, collapse = "\t"), ".", "\n  ",
         "Please check rename_activity_log.")
  }

  # Generate warning if inappropriate timestamp value is provided
  if(!(timestamp %in% c("both", "start", "complete"))){
    warning("This timestamp parameter value is not supported. Timestamp parameter should have value: both, start, complete. Default level of aggregation selected: both.")
    timestamp <- "both"
  }

  # Apply filter condition when specified
  tryCatch({
    if(!is.null(filter_condition)) {
      activity_log <- activity_log %>% filter(!! rlang::parse_expr(filter_condition))
    }
  }, error = function(e) {
    warning.filtercondition <<- TRUE
  }
  )

  if(warning.filtercondition) {
    warning("The condition '", filter_condition, "'  is invalid. No filtering performed on the dataset.")
  }

  n_cases <- activity_log %>% distinct(case_id) %>% nrow()

  # Filter activities included in activity_order
  activity_log <- activity_log %>% filter(activity %in% activity_order)

  # Add number to ordered activities (used for ordering when multiple activities have the same timestamp for a particular case)
  activity_order <- as.data.frame(activity_order)
  activity_order$nr <- seq(1, nrow(activity_order))
  colnames(activity_order)[1] <- "activity"
  activity_log <- merge(activity_log,activity_order, by  = "activity")

  # Detect time overlap between consecutive activities in case both timestamps are used
  if(timestamp == "both" & details == TRUE){
    # Sort the activity log
    activity_log <- activity_log %>% arrange(case_id, start,complete, nr)
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

    activity_log <- activity_log %>%
      mutate(
        prior_start = lag(start),
        prior_complete = lag(complete),
        prior_case = lag(case_id),
        prior_activity = lag(activity)
      )

    activity_log$time_overlap <- activity_log$case_id == activity_log$prior_case & activity_log$prior_complete > activity_log$start
    activity_log$time_overlap[1] <- FALSE
    activity_log$overlapping <- NA

    index <- is.na(activity_log$time_overlap)  # NA values are generated when there are missing timestamps. In that case, no overlaps can be detected.
    activity_log$time_overlap[index] <- FALSE
    remove(index)

    index <- activity_log$time_overlap == TRUE
    activity_log$overlapping[index] <- paste(activity_log$prior_activity[index], "and", activity_log$activity[index])
    remove(index)

    time_overlaps <- activity_log %>% filter(!is.na(overlapping)) %>% group_by(overlapping) %>% summarize(n = n()) %>% arrange(desc(n))
  }

  # Determine activity order for each case
  if(timestamp %in% c("both", "start")){
    # Sort the activity log
    activity_log <- activity_log %>% arrange(case_id, start,complete, nr)
    # Check whether activity order is satisfied
    activity_log <- activity_log %>% group_by(case_id) %>% summarize(activity_list = paste(activity, collapse = " - "))
  } else{
    # Sort the activity log
    activity_log <- activity_log %>% arrange(case_id, complete, start, nr)
    # Check whether activity order is satisfied
    activity_log <- activity_log %>% group_by(case_id) %>% summarize(activity_list = paste(activity, collapse = " - "))
  }

  # Perform comparison
  req_activity_order <- paste(activity_order$activity, collapse = " - ")
  incorrect_order <- activity_log %>% filter(!(activity_list == req_activity_order))

  # Prepare output
  # stat_false <- nrow(incorrect_order) / nrow(activity_log) * 100
  stat_false <- nrow(incorrect_order) / n_cases * 100
  stat_true <- 100 - stat_false

  # Print output
  if(!is.null(filter_condition)) {
    cat("Applied filtering condition", filter_condition, "\n")
  }
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
