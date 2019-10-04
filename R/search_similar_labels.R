#' Search for similar labels in a column
#'
#' Function that tries to detect spelling mistakes in a given activity log column
#' @param activity_log The activity log (renamed/formatted using functions rename_activity_log and convert_timestamp_format)
#' @param column_label The name of the column in which to search for spelling mistakes
#' @param max_edit_distance The maximum number of insertions, deletions and substitutions that are allowed to be executed in order for two strings to be considered similar.
#' @param show_NA A boolean indicating if labels that do not show similarities with others should be shown in the output
#' @param ignore_capitals A boolean indicating if capitalization should be included or excluded when calculating the edit distance between two strings
#' @return A data frame containing, for each label in the column, an overview of similar labels.
#' @export

similar_labels <- function(activity_log, column_label, max_edit_distance = 3, show_NA = FALSE, ignore_capitals = FALSE) {

  # Check if the log has been renamed
  missing_columns <- check_colnames(activity_log, column_label)
  if(!is.null(missing_columns)){
    stop(paste("The following column labels were not found in the log:", paste(missing_columns, collapse = "\t"), ".\n  Please check rename_activity_log."))
  }

  # First extract all unique values of the requested column
  labels <- activity_log %>% count_(column_label) %>% arrange(-n)

  # Is this a character vector? If not, stop.
  if(class(labels[[column_label]]) != "character"){
    stop("The column you provided (", column_label, ") is not of the 'character' class.")
  }

  # If so, start work on the dataframe containing all similar labels
  # Define the function that will gather all similar labels. x represents the vector holding all labels to compute similarities for

  gather_similar_labels <- function(x){
    # Prepare an output variable in which similar labels are stored
    out <- character(length = length(x))

    for(i in seq_along(x)){
      # Results holds all similar labels for this specific iteration
      results <- c()

      # NA cannot have similarities, so skip it altogether
      if ( !is.na(x[i]) & !is.null(x[i]) ) {

        # x will be the current label. Iterate over all others to compute similarties
        for(label in labels[[column_label]]){
          # Set the variables to compare depending on ignore_capitals
          if(!ignore_capitals) {
            compare_x <- x[i]
            compare_label <- label
          } else {
            compare_x <- str_to_lower(x[i])
            compare_label <- str_to_lower(label)
          }

          # Make the comparison
          if( (compare_x != compare_label) & (ain(compare_x, compare_label, maxDist = max_edit_distance) == TRUE) ) {
            # If the comparison is found to be positive, add it to the results
            results <- results %>% append(label)
          }
        }
      }

      # Results holds labels for this iteration. Add it to the general output
      if(!is.null(results)){
        out[i] <- paste(results, collapse = " - ")
      } else {
        out[i] <- NA
      }

    }

    return(out)
  }

  # Start building the data frame to store the similarities in
  similarities <- labels # %>% select(-n)

  similarities$similar_to <- gather_similar_labels(similarities[[column_label]])

  if(!show_NA){
    similarities <- similarities %>% filter(!is.na(similar_to))
  }

  return(similarities)

}
