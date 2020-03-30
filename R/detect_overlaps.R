



#' Detect overlapping acitivity instances
#' @inheritParams detect_activity_frequency_violations

#' @param level_of_aggregation Look for overlapping activity instances within a case or within a resource.
#' @return
#' @export
#' @importFrom purrr map2
#' @importFrom purrr map2_lgl
#' @importFrom purrr map_dbl
#' @importFrom lubridate interval
#' @importFrom lubridate int_overlaps
#' @importFrom lubridate intersect
#' @importFrom rlang set_names
#' @examples
detect_overlaps <- function(activitylog,
                            details = FALSE,
                            level_of_aggregation = c("case", "resource"),
                            filter_conditon)  {
  UseMethod("detect_overlaps")
}

#' @export

detect_overlaps.activitylog <- function(activitylog,
                                        details = FALSE,

                                        level_of_aggregation = c("case", "resource"),
                                        filter_condition = NULL) {


  level_of_aggregation <- match.arg(level_of_aggregation)


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

  if(level_of_aggregation == "case") {
    detect_overlaps_case(activitylog, details)
  } else {
    detect_overlaps_resource(activitylog, details)
  }
}

detect_overlaps_case <- function(activitylog, details) {

  activitylog %>%
    rename(ACTIVITY_CLASSIFIER := !!activity_id_(activitylog),
           CASE_CLASSIFIER := !!case_id_(activitylog)) %>%
    select(ACTIVITY_CLASSIFIER, CASE_CLASSIFIER, start, complete) %>%
    mutate(INTERVAL = map2(start, complete, interval)) -> intervals

  intervals %>%
    inner_join(intervals, by = "CASE_CLASSIFIER") %>%
    filter(start.x < start.y) %>%
    mutate(overlaps = map2_lgl(INTERVAL.x, INTERVAL.y, int_overlaps)) %>%
    filter(overlaps) %>%
    mutate(time_of_overlap_mins = map2(INTERVAL.x, INTERVAL.y, lubridate::intersect)) %>%
    mutate(time_of_overlap_mins = map_dbl(time_of_overlap_mins, as.double, units = "mins")) %>%
    filter(time_of_overlap_mins > 0) %>%
    select(-overlaps) %>%
    select(-start.x:-INTERVAL.x, -start.y:-INTERVAL.y) %>%
    set_names(c("activity_a",case_id(activitylog),"activity_b","time_of_overlap_mins")) %>%
    select(!!case_id_(activitylog), everything())  -> raw


  if(details) {
    raw
  } else {
      raw %>%
      group_by(activity_a, activity_b) %>%
      summarize(n = n(), avg_overlap_mins = mean(time_of_overlap_mins))

  }
}

detect_overlaps_resource <- function(activitylog, details) {

  activitylog %>%
    rename(ACTIVITY_CLASSIFIER := !!activity_id_(activitylog),
           RESOURCE_CLASSIFIER := !!resource_id_(activitylog),
           CASE_CLASSIFIER := !!case_id_(activitylog)) %>%
    select(ACTIVITY_CLASSIFIER, RESOURCE_CLASSIFIER, CASE_CLASSIFIER, start, complete) %>%
    mutate(INTERVAL = map2(start, complete, interval)) -> intervals

  intervals %>%
    inner_join(intervals, by = "RESOURCE_CLASSIFIER") %>%
    filter(start.x < start.y) %>%
    mutate(overlaps = map2_lgl(INTERVAL.x, INTERVAL.y, int_overlaps)) %>%
    filter(overlaps) %>%
    mutate(time_of_overlap_mins = map2(INTERVAL.x, INTERVAL.y, lubridate::intersect)) %>%
    mutate(time_of_overlap_mins = map_dbl(time_of_overlap_mins, as.double, units = "mins")) %>%
    filter(time_of_overlap_mins > 0) %>%
    select(-overlaps) %>%
    select(-start.x:-INTERVAL.x, -start.y:-INTERVAL.y) %>%
    set_names(c("activity_a",resource_id(activitylog),"case_a", "activity_b", "case_b", "time_of_overlap_mins")) %>%
    select(!!resource_id_(activitylog), everything()) -> raw


  if(details) {
    raw
  } else {
    raw %>%
      group_by(activity_a, activity_b) %>%
      summarize(n = n(), avg_overlap_mins = mean(time_of_overlap_mins))
  }
}
