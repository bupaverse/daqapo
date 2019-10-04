#' Detect missing values
#'
#' Function detecting missing values at different levels of aggregation
#' \itemize{
#' \item overview: presents an overview of the absolute and relative number of missing values for each column
#' \item column: presents an overview of the absolute and relative number of missing values for a particular column
#' \item activity: presents an overview of the absolute and relative number of missing values for each column, aggregated by activity
#' }
#' @param activity_log The activity log (renamed/formatted using functions rename_activity_log and convert_timestamp_format)
#' @param level_of_aggregation Level of aggregation at which missing values are identified (either "overview", "column" or "activity)
#' @param colname Column name of the column that needs to be analyzed when the level of aggregation is "column"
#' @param details Boolean indicating wheter details of the results need to be shown
#' @param filter_condition Condition that is used to extract a subset of the activity log prior to the application of the function
#' @return Information on the absolute and relative number of missing values at the requested level of aggregation
#' @export

missing_values <- function(activity_log, level_of_aggregation = "overview", colname = NULL, details = TRUE, filter_condition = NULL){

  # Initiate warning variables
  warning.filtercondition <- FALSE

  # Generate warning if inappropriate level of aggregation is requested
  if(!(level_of_aggregation %in% c("overview", "column", "activity"))){
    warning("This level of aggregation is not supported. Level of aggregation should be: overview, column, activity. Default level of aggregation selected: overview.")
    level_of_aggregation <- "overview"
  }

  # Apply filter condition when specified
  tryCatch({
    if(!is.null(filter_condition)) {
      activity_log <- activity_log %>% filter_(filter_condition)
    }
  }, error = function(e) {
    warning.filtercondition <<- TRUE
  }
  )

  if(warning.filtercondition) {
    warning("The condition '", filter_condition, "'  is invalid. No filtering performed on the dataset.")
  }

  # Print general output information
  if(!is.null(filter_condition) & !warning.filtercondition) {
    cat("Applied filtering condition", filter_condition, "\n")
  }
  cat("Selected level of aggregation:", level_of_aggregation, "\n", "\n")

  # Provide appropriate output depending on the level of aggregation
  if(level_of_aggregation == "overview"){
    cat("*** OUTPUT ***", "\n")
    cat("Absolute number of missing values per column:", "\n")
    print(as.data.frame(colSums(is.na(activity_log)), optional = TRUE))
    cat("\n")
    cat("Relative number of missing values per column (expressed as percentage):", "\n")
    print(as.data.frame(colMeans(is.na(activity_log)) * 100, optional = TRUE))

    if(details == TRUE){
      # Provide overview of incomplete rows
      cat("\n")
      cat("Overview of activity log rows which are incomplete:", "\n")
      activity_log <- activity_log[!complete.cases(activity_log),]
      return(activity_log)
    }
  } else if(level_of_aggregation == "column"){
    # Check if provided column name is valid
    if(is.null(colname) || !(colname %in% colnames(activity_log))){
      stop("Provided column name (argument colname) is not present in the activity log.")
    } else{
      cat("*** OUTPUT ***", "\n")
      cat("Absolute number of missing values in column", colname, ":", sum(is.na(activity_log[,colname])) ,"\n")
      cat("Relative number of missing values in column", colname, "(expressed as percentage):",
          sum(is.na(activity_log[,colname]))/nrow(activity_log) * 100 ,"\n")

      if(details == TRUE){
        # Provide overview of rows where the column under consideration is NA
        cat("\n")
        cat("Overview of activity log rows in which", colname, "is missing:", "\n")
        activity_log <- activity_log[is.na(activity_log[,colname]),]
        return(activity_log)
      }
    }
  } else if(level_of_aggregation == "activity"){

    # Check if the required columns are present in the log
    missing_columns <- check_colnames(activity_log, "activity")
    if(!is.null(missing_columns)){
      stop("The following columns, which are required for the test, were not found in the activity log: ", paste(missing_columns, collapse = "\t"), ".", "\n", "Please check rename_activity_log.")
    }

    cat("*** OUTPUT ***", "\n")
    cat("Absolute number of missing values per column (per activity):", "\n")
    print(activity_log %>% group_by(activity) %>% summarise_all(funs(sum(is.na(.)))))
    cat("\n")
    cat("Relative number of missing values per column (per activity, expressed as percentage):", "\n")
    print(activity_log %>% group_by(activity) %>% summarise_all(funs(sum(is.na(.)) / length(.))))

    if(details == TRUE){
      # Provide overview of incomplete rows
      cat("\n")
      cat("Overview of activity log rows which are incomplete:", "\n")
      activity_log <- activity_log[!complete.cases(activity_log),]
      return(activity_log)
    }
  }

}
