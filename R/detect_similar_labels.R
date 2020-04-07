#' Search for similar labels in a column
#'
#' Function that tries to detect spelling mistakes in a given activity log column
#' @inheritParams detect_activity_frequency_violations
#' @param column_labels The name of the column(s) in which to search for spelling mistakes
#' @param max_edit_distance The maximum number of insertions, deletions and substitutions that are allowed to be executed in order for two strings to be considered similar.
#' @param show_NA A boolean indicating if labels that do not show similarities with others should be shown in the output
#' @param ignore_capitals A boolean indicating if capitalization should be included or excluded when calculating the edit distance between two strings
#' @return tbl_df providing an overview of similar labels for the indicated column
#' @examples
#' \donttest{
#' data("hospital_actlog")
#' detect_similar_labels(activitylog = hospital_actlog,
#'      column_labels = "activity",
#'      max_edit_distance = 3)
#' }
#' @export


detect_similar_labels <- function(activitylog, column_labels, max_edit_distance, show_NA,ignore_capitals, filter_condition) {
  UseMethod("detect_similar_labels")
}

#' @export

detect_similar_labels <- function(activitylog, column_labels, max_edit_distance = 3, show_NA = FALSE, ignore_capitals = FALSE, filter_condition = NULL) {

  # Predefine variables
  similar_to <- NULL
  data <- NULL

  if(class(column_labels) != "character") {
    stop("column_labels must be a character vector")
  }

  if(!all(column_labels %in% names(activitylog))) {
    warning(glue("Some provided column labels don't exist and will be ignored: {str_c(column_labels[!(column_labels %in% names(activitylog))], collapse = ', ')}"))
    column_labels <- column_labels[(column_labels %in% names(activitylog))]
  }


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


  activitylog %>%
    select_at(.vars = column_labels) %>%
    map(class) -> classes


  if(!all(classes %in% c("character","factor"))) {
    warning(glue("Not all provided columns are of type character or factor and will be ignored: {str_c(classes[!(classes %in% c('character','factor'))] %>% names(), collapse = ',')}"))
    column_labels <- classes[classes %in% c("character","factor")] %>% names
  }

  if(length(column_labels) < 1) {
    stop("column_labels must be a vector of one or more valid column labels of type factor or character.")
  }


  similarities <- tibble(column_labels) %>%
    mutate(data = map(column_labels, gather_similar_labels,
                         activitylog = activitylog,
                         ignore_capitals = ignore_capitals,
                         max_edit_distance = max_edit_distance)) %>%
    unnest(data)

  if(!show_NA){
    similarities <- similarities %>% filter(!is.na(similar_to))
  }

  return(similarities)

}

gather_similar_labels <- function(variable, activitylog, ignore_capitals = ignore_capitals, max_edit_distance = max_edit_distance) {

  if(class(activitylog[[variable]]) == "character") {
    labels <- unique(activitylog[[variable]])

  } else if (class(activitylog[[variable]]) == "factor") {
    labels <- levels(activitylog[[variable]])
  }

  similars <- detect_similar_labels_COLUMN(labels = labels, max_edit_distance = max_edit_distance, ignore_capitals = ignore_capitals)

  tibble(labels) %>%
    mutate(similar_to = similars) %>%
    return()
}

detect_similar_labels_COLUMN <- function(labels, max_edit_distance, ignore_capitals) {
  x <- labels
  out <- character(length = length(labels))

  for(i in seq_along(labels)){
    # Results holds all similar labels for this specific iteration
    results <- c()

    # NA cannot have similarities, so skip it altogether
    if ( !is.na(x[i]) & !is.null(x[i]) ) {

      # x will be the current label. Iterate over all others to compute similarties
      for(label in labels){
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




















