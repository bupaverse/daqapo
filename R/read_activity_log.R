#' Read activity log dataset from csv
#'
#' Function which reads input csv dataset. Depending on the specification of the function's parameters, it also replaces particular cell values by NA.
#' @param file_name The file name of the input csv dataset
#' @param case_id_label The name of the column in the original dataset reflecting the case identifier
#' @param activity_label The name of the column in the original dataset reflecting the activity name
#' @param resource_label The name of the column in the original dataset reflecting the resource identifier
#' @param start_label The name of the column in the original dataset reflecting the activity start times
#' @param complete_label The name of the column in the original dataset reflecting the activity completion times
#' @param case_attributes A vector containing the name(s) of the column(s) in the original dataset reflecting case attributes
#' @param timestamp_format The format of the timestamps in the original dataset (either "yyyy-mm-dd hh:mm:ss" or "dd-mm-yyyy hh:mm:ss")
#' @param sep The character that separates columns in csv file formats
#' @param empty_cells_are_NA Boolean indicating whether empty cells should be considered as NA
#' @param zero_values_are_NA Boolean indicating whether zero values (i.e. 0) should be considered as NA
#' @return Input csv dataset with appropriate formatting of NA values
#' @export
read_activity_log <- function(file_name, case_id_label = NULL, activity_label = NULL, resource_label = NULL, start_label = NULL, complete_label = NULL,
                              case_attributes = NULL, timestamp_format = NULL, sep = ",", empty_cells_are_NA = TRUE, zero_values_are_NA = FALSE){

  # Predefine variables
  start <- NULL
  complete <- NULL

  if(class(file_name)[1] == "character"){
    if(empty_cells_are_NA == TRUE & zero_values_are_NA == TRUE){
      raw_activity_log <- read.csv(file_name, header = TRUE, sep = sep, stringsAsFactors = FALSE, na.strings = c("NA", "", " ", "\t", 0))
    } else if(empty_cells_are_NA == TRUE){
      raw_activity_log <- read.csv(file_name, header = TRUE, sep = sep, stringsAsFactors = FALSE, na.strings = c("NA", "", " ", "\t"))
    } else{
      raw_activity_log <- read.csv(file_name, header = TRUE, sep = sep, stringsAsFactors = FALSE, na.strings = "NA")
    }
  } else if("data.frame" %in% class(file_name)){
    raw_activity_log <- file_name # %>% as.data.frame()
  } else {
    stop("The input file must be either a .csv file or a data frame object.")
  }

  # Can we rename the activity log?
  renamed <- FALSE

  if(!(is.null(case_id_label) | is.null(activity_label) | is.null(resource_label) | is.null(start_label) | is.null(complete_label))){

    tryCatch(
      {
        raw_activity_log <- rename_activity_log(raw_activity_log, case_id_label, activity_label, resource_label, start_label, complete_label, case_attributes)
        renamed <- TRUE
      },
      warning = function(w) {
        warning(w)
      }
    )

    # # Are the provided labels correct?
    # missing_columns <- check_colnames(raw_activity_log, c(case_id_label, activity_label, resource_label, start_label, complete_label))
    # if(!is.null(missing_columns)){
    #   message <- paste(missing_columns, collapse = "\t")
    #   warning(paste("The following column labels were not found in the log:", message))
    #   return(raw_activity_log)
    # }
    #
    # # They are correct, so rename the log so our tests recognize the columns
    # raw_activity_log <- rename_activity_log(raw_activity_log, case_id_label, activity_label, resource_label, start_label, complete_label, case_attributes)
    # renamed <- TRUE

  } else {
    warning("Column labels are not provided. Please check rename_activity_log before running any analyses.")
  }

  # Can we convert timestamps for start/complete to POSIXct?
  if(!is.null(timestamp_format)){

    # Only convert if the log has been renamed to proper column labels
    if(renamed == TRUE){
      raw_activity_log <- raw_activity_log %>% convert_timestamp_format(timestamp_format) %>% arrange(start, complete)
    } else {
      warning("The activity log columns were not yet renamed. Please check rename_activity_log, then convert_timestamp_format before running any analyses.")
    }
  } else {
    warning("The timestamp format was not provided. Please check convert_timestamp_format before running any analyses.")
  }

  return(raw_activity_log)
}

