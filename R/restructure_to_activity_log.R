#' Restructure an event log dataset to activity log format
#'
#' Function which takes an event log as input and transforms it into an activity log. If event_matching_label, the name of the column that contains information which start and complete events belong together, is NULL, a basic heuristic of "for every start event per case per activity, take the first available complete event." will be applied.
#' @param event_log The data frame containing the event log data
#' @param case_id_label The name of the column in the original dataset reflecting the case identifier
#' @param activity_label The name of the column in the original dataset reflecting the activity name
#' @param resource_label The name of the column in the original dataset reflecting the resource identifier
#' @param event_lifecycle_label The name of the column in the original dataset reflecting the activity lifecycle state
#' @param timestamps_label The name of the column in the original dataset reflecting the activity times
#' @param case_attributes A vector containing the name(s) of the column(s) in the original dataset reflecting case attributes
#' @param event_matching_label The name of the column in the original dataset reflecting the relation between start and end events per case per activity. If NULL, a basic heuristic will be applied that links every first available start event with the first available complete event
#' @param timestamp_format The format of the timestamps in the original dataset (either "yyyy-mm-dd hh:mm:ss" or "dd-mm-yyyy hh:mm:ss")
#' @return A data frame in the format of an activity log.
#' @export
#'

restructure_to_activity_log <- function(event_log, case_id_label, activity_label, resource_label, event_lifecycle_label,
                                        timestamps_label, case_attributes = NULL, event_matching_label = NULL, timestamp_format = "yyyy-mm-dd hh:mm:ss") {

  # Predefine variables
  str <- NULL

  # Check if the provided column labels are correct:
  missing_columns <- check_colnames(event_log, case_id_label, activity_label, resource_label, timestamps_label, case_attributes, event_lifecycle_label, event_matching_label)
  if(!is.null(missing_columns)){
    stop("The following column labels were not found in the log:",
         paste(missing_columns, collapse = "\t"), ".", "\n  ",
         "Ensure all labels are passed correctly to transform to an activity log.")
  }

  # Select only the required columns
  if(is.null(case_attributes) & is.null(event_matching_label)){
    event_log <- event_log %>% select_(case_id_label, activity_label, resource_label, event_lifecycle_label,
                                       timestamps_label)
  }
  else if(is.null(case_attributes) & !is.null(event_matching_label)){
    event_log <- event_log %>% select_(case_id_label, activity_label, resource_label, event_lifecycle_label,
                                       timestamps_label, event_matching_label)
  }
  else if(!is.null(case_attributes) & is.null(event_matching_label)){
    to_select <- c(case_id_label, activity_label, resource_label, event_lifecycle_label, timestamps_label, case_attributes)
    event_log <- event_log %>% select(one_of(to_select))
  }
  else {
    to_select <- c(case_id_label, activity_label, resource_label, event_lifecycle_label, timestamps_label, case_attributes, event_matching_label)
    event_log <- event_log %>% select(one_of(to_select))
  }

  # Do some preparations:
  # Lowercase lifecycles and only use 'start' and 'complete'
  event_log[[event_lifecycle_label]] <- str_to_lower(event_log[[event_lifecycle_label]])

  other_events <- event_log %>%
    filter_(paste0(event_lifecycle_label, " != 'start' & ", event_lifecycle_label, " != 'complete'")) %>%
    nrow() %>% as.numeric()
  if(other_events > 0) {
    warning("Other event types than 'start' and 'complete' detected. They are filtered out of the event log.")
  }
  rm(other_events)

  event_log <- event_log %>%
    filter_(paste0(event_lifecycle_label, " == 'start' | ", event_lifecycle_label, " == 'complete'")) %>%
    arrange_(timestamps_label)


  # If there is no event matching column, generate one.
  # This function will only be called if there are duplicate key entries which this column needs to fix anyways.
  if(is.null(event_matching_label)){

    # Start with checking which events need to be matched via duplicate key entries check:
    n_duplicates <- event_log %>% count_(c(case_id_label, activity_label, resource_label, event_lifecycle_label)) %>% filter(n>1)

    if(nrow(n_duplicates) > 0){
      ####
      # IF DUPLICATE KEYS ARE FOUND
      ####

      # Add the combination of case, activity and resource as a string for easier matching (the lifecycle is not included: if either start or complete is present as a duplicate,
      # we also need to number the other event to ensure the dataset is still spread as intended)
      n_duplicates$str <- paste(n_duplicates[[case_id_label]], n_duplicates[[activity_label]],
                                n_duplicates[[resource_label]], sep = " - ")
      duplicates <- n_duplicates$str

      # Add the combination strings to the event log as well
      event_log$str <- paste(event_log[[case_id_label]], event_log[[activity_label]],
                             event_log[[resource_label]], sep = " - ")

      # Group and create the row numbers where this is required, then drop the strings for readability
      event_log <- event_log %>%
        group_by_(case_id_label, activity_label, event_lifecycle_label) %>%
        mutate(
          event_matching = if_else(str %in% duplicates, row_number(), NULL)
        ) %>%
        select(-str) %>%
        ungroup()
      event_matching_label <- "event_matching"

      warning("No event_matching_label was provided. Automatically created links between events: \n  ",
              "For every start event per case per activity, take the first available complete event.")
    }

  }

  ### Restructure
  event_log <- event_log %>% spread_(event_lifecycle_label, timestamps_label)
  if (!is.null(event_matching_label)){
    event_log <- event_log %>%
    select_(paste0("-", event_matching_label))
  }

  ### Check if both start and complete exist in the log. If either is missing, set them to NA
  if(!("start" %in% colnames(event_log))){
    warning("No start events were found. Creating and initialising 'start' to NA.")
    event_log$start <- NA
  }
  if(!("complete" %in% colnames(event_log))){
    warning("No complete events were found. Creating and initialising 'complete' to NA.")
    event_log$complete <- NA
  }

  ### Rename and order
  event_log <- event_log %>%
    rename_activity_log(case_id_label, activity_label, resource_label, "start", "complete", case_attributes = case_attributes) %>%
    select(case_id, activity, resource, start, complete, everything()) %>%
    arrange(start, complete) %>%
    convert_timestamp_format(timestamp_format)



  return(event_log)
}
