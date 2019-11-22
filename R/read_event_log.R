#' Read an event log dataset
#'
#' Function which reads event log datasets. Depending on the specification of the function's parameters, it also replaces particular cell values by NA.
#' @param file_name The file name of the input dataset. Can be csv, xes or eventlog format
#' @param case_id_label The name of the column in the original dataset reflecting the case identifier
#' @param activity_label The name of the column in the original dataset reflecting the activity name
#' @param resource_label The name of the column in the original dataset reflecting the resource identifier
#' @param event_lifecycle_label The name of the column in the original dataset reflecting the activity lifecycle state
#' @param timestamps_label The name of the column in the original dataset reflecting the activity times
#' @param case_attributes A vector containing the name(s) of the column(s) in the original dataset reflecting case attributes
#' @param timestamp_format The format of the timestamps in the original dataset (either "yyyy-mm-dd hh:mm:ss" or "dd-mm-yyyy hh:mm:ss")
#' @param event_matching_label The name of the column in the original dataset reflecting the relation between start and end events per case per activity
#' @param sep The character that separates columns in csv file formats
#' @param empty_cells_are_NA Boolean indicating whether empty cells should be considered as NA
#' @param zero_values_are_NA Boolean indicating whether zero values (i.e. 0) should be considered as NA
#' @return An activity log with proper NA settings, data restructuring and type conversions.
#' @export
#'

read_event_log <- function(file_name,
                           case_id_label = NULL,
                           activity_label = NULL,
                           resource_label = NULL,
                           event_lifecycle_label = NULL,
                           timestamps_label = NULL,
                           case_attributes = NULL,
                           timestamp_format = NULL,
                           event_matching_label = NULL,
                           sep = ",",
                           empty_cells_are_NA = TRUE,
                           zero_values_are_NA = FALSE){

  # Predefine variables
  str_sub <- NULL
  start <- NULL
  complete <- NULL
  case_id <- NULL
  activity <- NULL
  resource <- NULL

  # Check type of file_name input
  if(class(file_name)[1] == "character"){
    extension <- str_sub(file_name, -3)

    # In case of a csv file
    if(extension == "csv"){
      if(empty_cells_are_NA == TRUE & zero_values_are_NA == TRUE){
        raw_event_log <- read.csv(file_name, header = TRUE, sep = sep, stringsAsFactors = FALSE, na.strings = c("NA", "", " ", "\t", 0))
      } else if(empty_cells_are_NA == TRUE){
        raw_event_log <- read.csv(file_name, header = TRUE, sep = sep, stringsAsFactors = FALSE, na.strings = c("NA", "", " ", "\t"))
      } else{
        raw_event_log <- read.csv(file_name, header = TRUE, sep = sep, stringsAsFactors = FALSE, na.strings = "NA")
      }
    } else if (extension == "xes"){
      raw_event_log <- read_xes(xesfile = file_name) #%>% as.data.frame()
    } else {
      stop("The input file must be either in .csv, .xes or eventlog format.")
    }
  } else if("eventlog" %in% class(file_name) | "data.frame" %in% class(file_name)){
    raw_event_log <- file_name %>% as.data.frame()
  } else {
    stop("The input file must be either in .csv, .xes or eventlog format.")
  }

  if(is.null(raw_event_log)){
    stop("Error loading the event log. Please check if the input is in the correct csv, xes or eventlog format.")
  }

  # READING DATA IS COMPLETE

  # Check if the provided column labels are correct:
  missing_columns <- check_colnames(raw_event_log, case_id_label, activity_label, resource_label, timestamps_label, case_attributes, event_lifecycle_label)
  if(!is.null(missing_columns)){
    stop("The following column labels were not found in the log:",
         paste(missing_columns, collapse = "\t"), ".\n  ",
         "Ensure all labels are passed correctly to read the event log.")
  }

  # Set some vars to keep track of errors found:
  warning.resourceinconsistencies <- FALSE
  warning.duplicatekeys <- FALSE



  # If column specifications are provided, we can already restructure and rename to our activity log format
  if(!(is.null(case_id_label) | is.null(activity_label) | is.null(resource_label) | is.null(timestamps_label))){

    # Do we need to restructure based on event lifecycle states?
    if(!is.null(event_lifecycle_label)){

      # Select all columns that are required to reach the activity log format
      to_select <- c(case_id_label, activity_label, resource_label, event_lifecycle_label, timestamps_label, event_matching_label, case_attributes)
      raw_event_log <- raw_event_log %>% select(one_of(to_select))

      ####
      # Restructure and restrict to 'start' and 'complete' timestamps
      ####

      ### Make sure the lifecycle states are in lowercaps to avoid errors later on
      raw_event_log[[event_lifecycle_label]] <- str_to_lower(raw_event_log[[event_lifecycle_label]])

      ### See if there are resource inconsistencies
      suppressWarnings(
        inconsistencies <- resource_inconsistencies(
          raw_event_log, case_id_label, activity_label, resource_label, event_lifecycle_label, timestamps_label, event_matching_label
        )
      )

      if("data.frame" %in% class(inconsistencies)) {
        if (nrow(inconsistencies) > 0 ) {
          warning.resourceinconsistencies <- TRUE
          warning("Resource inconsistencies were found in the event log.", "\n  ", "Please check resource_inconsistencies.", "\n")
        }
      }

      ### Check for duplicate key entries on lifecycle level (i.e. activity duplication)
      n_duplicates <- raw_event_log %>% count_(c(case_id_label, activity_label, resource_label, event_lifecycle_label,
                                                 event_matching_label)) %>% filter(n>1)
      # n_duplicates <- raw_event_log %>% count(!! rlang::parse_expr(case_id_label), !! rlang::parse_expr(activity_label),
      #                                          !! rlang::parse_expr(resource_label), !! rlang::parse_expr(event_lifecycle_label),
      #                                          !! rlang::parse_expr(event_matching_label)) %>% filter(n>1)
      if(n_duplicates %>% nrow() > 0) {
        warning.duplicatekeys <- TRUE
        warning("Duplicate identifiers are detected in the following case, activity, resource and lifecycle combinations:\n",
                paste(capture.output(print(n_duplicates)), collapse = "\n"),
                "\n  ",
                "Please check restructure_to_activity_log.", "\n")
      }

      if (warning.resourceinconsistencies | warning.duplicatekeys) {
        # warning("The data was not transformed to activity log format.\n")
        return(raw_event_log)
      }

      ### Filter on lifecycle states 'start' and 'complete'
      other_events <- raw_event_log %>%
        filter(!! rlang::parse_expr(paste0(event_lifecycle_label, " != 'start' & ", event_lifecycle_label, " != 'complete'"))) %>%
        nrow() %>% as.numeric()
      if(other_events > 0) {
        warning("Other event types than 'start' and 'complete' detected. They are filtered out of the event log.")
      }
      rm(other_events)

      raw_event_log <- raw_event_log %>%
        filter(!! rlang::parse_expr(paste0(event_lifecycle_label, " == 'start' | ", event_lifecycle_label, " == 'complete'")))

      ### Go from long to wide format, based on the lifecycles
      raw_event_log <- raw_event_log %>% spread(event_lifecycle_label, timestamps_label)

      ### Check if the columns start and complete exist. If not, initiate them to NA
      if(!("start" %in% colnames(raw_event_log))){
        warning("No start events were found. Creating and initialising 'start' to NA.")
        raw_event_log$start <- NA
      }
      if(!("complete" %in% colnames(raw_event_log))){
        warning("No complete events were found. Creating and initialising 'complete' to NA.")
        raw_event_log$complete <- NA
      }

      ### Reorder and rename the columns, and arrange the log
      raw_event_log <- raw_event_log %>%
        select(case_id_label, activity_label, resource_label, "start", "complete", everything()) %>%
        rename_activity_log(case_id_label, activity_label, resource_label, "start", "complete", case_attributes) %>%
        convert_timestamp_format(timestamp_format) %>%
        arrange(start, complete)

      # Return the result
      return(raw_event_log)
    }

    # There are no lifecycles, so assume all timestamps are for the complete lifecycle state
    else {
      warning("No event_lifecycle_state passed. It is assumed that all timestamps are for complete events.")
      # Select all columns that are required to reach the activity log format
      if(!is.null(case_attributes)){
        raw_event_log <- raw_event_log %>% select(case_id_label, activity_label, resource_label, timestamps_label, case_attributes)
      } else {
        raw_event_log <- raw_event_log %>% select(case_id_label, activity_label, resource_label, timestamps_label)
      }

      # Apply formatting
      raw_event_log$start <- NA
      raw_event_log <- raw_event_log %>%
        rename_activity_log(case_id_label, activity_label, resource_label, "Start", timestamps_label, case_attributes) %>%
        convert_timestamp_format(timestamp_format) %>%
        select(case_id, activity, resource, start, complete, everything()) %>%   # everything() selects case attributes if they are present
        arrange(start, complete)

      # Return the result
      return(raw_event_log)
    }
  }

  # If not, give a warning message that the data structure should be checked.
  else {
    warning("Column labels are not provided. Please check the data structure and rename_activity_log before running any analyses.")
  }

  # Return the unedited log
  return(raw_event_log)
}



