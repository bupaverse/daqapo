

#' Events to activities
#'
#' @param eventlog The event log to be converted. An object of class
#' \code{eventlog}.
#'
#'
#' @importFrom bupaR lifecycle_labels
#' @importFrom edeaR filter_lifecycle
#'
#' @export
#'
#'

events_to_activities <- function(eventlog) {
  UseMethod("events_to_activities")
}

#' @describeIn events_to_activities Create activity log from event log
#' @export

events_to_activities.eventlog <- function(eventlog) {


  eventlog <- standardize_lifecycle(eventlog)

  #remove events which are not start or complete
  # if(any(!(lifecycle_labels(eventlog) %in% c("start", "complete")))) {
  #   warning("Other event types than 'start' and 'complete' detected. They are filtered out of the event log.")
  #   eventlog <- filter_lifecycle(eventlog, lifecycle = c("start","complete"))
  # }

  eventlog %>%
    select(-.order, force_df = TRUE) %>%
    spread(!!lifecycle_id_(eventlog), !!timestamp_(eventlog)) -> activitylog


  ### Check if the columns start and complete exist. If not, initiate them to NA
  if(!("start" %in% colnames(activitylog))){
    warning("No start events were found. Creating and initialising 'start' to NA.")
    activitylog$start <- as.Date(NA)
  }
  if(!("complete" %in% colnames(activitylog))){
    warning("No complete events were found. Creating and initialising 'complete' to NA.")
    activitylog$complete <- as.Date(NA)
  }

  activitylog(as.data.frame(activitylog),
              case_id = case_id(eventlog),
              activity_id = activity_id(eventlog),
              resource_id = resource_id(eventlog),
              lifecycle_ids = as.vector(c("start","complete", str_subset(lifecycle_labels(eventlog), c("(start)|(complete)"), negate = T)))
              )


}
