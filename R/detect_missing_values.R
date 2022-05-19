#' Detect missing values
#'
#' Function detecting missing values at different levels of aggregation
#' \itemize{
#' \item overview: presents an overview of the absolute and relative number of missing values for each column
#' \item column: presents an overview of the absolute and relative number of missing values for a particular column
#' \item activity: presents an overview of the absolute and relative number of missing values for each column, aggregated by activity
#' }
#'
#' @inheritParams detect_activity_frequency_violations
#' @param level_of_aggregation Level of aggregation at which missing values are identified (either "overview", "column" or "activity)
#' @param column Column name of the column that needs to be analyzed when the level of aggregation is "column"
#' @return activitylog containing the rows of the original activity log which contain a missing value
#' @examples
#' \donttest{
#' data("hospital_actlog")
#' detect_missing_values(activitylog = hospital_actlog)
#' detect_missing_values(activitylog = hospital_actlog, level_of_aggregation = "activity")
#' detect_missing_values(activitylog = hospital_actlog, level_of_aggregation = "column",
#'      column = "triagecode")
#' }
#' @export

detect_missing_values <- function(activitylog, level_of_aggregation, column, details, filter_condition) {
  UseMethod("detect_missing_values")
}

#' @export

detect_missing_values.activitylog <- function(activitylog,
                                  level_of_aggregation = c("overview", "column","activity"),
                                  column = NULL, details = TRUE, filter_condition = NULL){

  level_of_aggregation <- match.arg(level_of_aggregation)

  # Predefine variables
  complete.cases <- NULL
  activity <- NULL

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

  # Print general output information
  message("Selected level of aggregation:", level_of_aggregation)

  # Provide appropriate output depending on the level of aggregation
  if(level_of_aggregation == "overview"){

    if(!is.null(column)) {
      warning("Ignoring provided column argument at overview level.")
    }

    message("*** OUTPUT ***")
    message("Absolute number of missing values per column:")
    print(as.data.frame(colSums(is.na(activitylog)), optional = TRUE))
    message("Relative number of missing values per column (expressed as percentage):")
    print(as.data.frame(colMeans(is.na(activitylog)) * 100, optional = TRUE))

    if(details == TRUE){
      # Provide overview of incomplete rows
      message("Overview of activity log rows which are incomplete:")
      activitylog <- activitylog[!complete.cases(activitylog),]
      return(activitylog)
    }
  } else if(level_of_aggregation == "column"){
    # Check if provided column name is valid
    if(is.null(column) || !(column %in% colnames(activitylog))){
      stop("Provided column name (argument column) is not present in the activity log.")
    } else{
      message("*** OUTPUT ***")
      message("Absolute number of missing values in column", column, ":", sum(is.na(activitylog[,column])))
      message("Relative number of missing values in column", column, "(expressed as percentage):",
          sum(is.na(activitylog[,column]))/nrow(activitylog) * 100 ,"\n")

      if(details == TRUE){
        # Provide overview of rows where the column under consideration is NA
        message("\n")
        message("Overview of activity log rows in which", column, "is missing:")
        activitylog <- activitylog[is.na(activitylog[,column]),]
        return(activitylog)
      }
    }
  } else if(level_of_aggregation == "activity"){

    if(!is.null(column)) {
      warning("Ignoring provided column argument at overview level.")
    }

    message("*** OUTPUT ***")
    message("Absolute number of missing values per column (per activity):")
    print(activitylog %>% group_by(!!activity_id_(activitylog)) %>% summarise_all(list(~sum(is.na(.)))))
    message("Relative number of missing values per column (per activity, expressed as percentage):")
    print(activitylog %>% group_by(!!activity_id_(activitylog)) %>% summarise_all(list(~sum(is.na(.)) / length(.))))

    if(details == TRUE){
      # Provide overview of incomplete rows
      message("Overview of activity log rows which are incomplete:")
      activitylog <- activitylog[!complete.cases(activitylog),]
      return(activitylog)
    }
  }

}
