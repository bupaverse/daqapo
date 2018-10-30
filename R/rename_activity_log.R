#' Rename columns in an activity log
#'
#' Function renaming the columns in the activity log to the appropriate names, required for the application of quality assessment functions
#' @param activity_log The activity log
#' @param case_id_label The name of the column in the original dataset reflecting the case identifier
#' @param activity_label The name of the column in the original dataset reflecting the activity name
#' @param resource_label The name of the column in the original dataset reflecting the resource identifier
#' @param start_time_label The name of the column in the original dataset reflecting the activity start times
#' @param completion_time_label The name of the column in the original dataset reflecting the activity completion times
#' @param case_attributes A vector containing the name(s) of the column(s) in the original dataset reflecting case attributes
#' @return Activity log with renamed columns
#' @export
rename_activity_log <- function(activity_log, case_id_label, activity_label, resource_label, start_time_label, completion_time_label,
                                case_attributes = NULL) {
  colnames(activity_log)[match(case_id_label, colnames(activity_log))] <- "case_id"
  colnames(activity_log)[match(activity_label, colnames(activity_log))] <- "activity"
  colnames(activity_log)[match(resource_label, colnames(activity_log))] <- "resource"
  colnames(activity_log)[match(start_time_label, colnames(activity_log))] <- "start"
  colnames(activity_log)[match(completion_time_label, colnames(activity_log))] <- "complete"

  if(!is.null(case_attributes)){
    for(i in 1:length(case_attributes)){
      colnames(activity_log)[match(case_attributes[i], colnames(activity_log))] <- paste("cattr_", case_attributes[i], sep = "")
    }
  }

  return(activity_log)
}
