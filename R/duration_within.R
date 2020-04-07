#' Define bounds for activity duration
#'
#' Funtion to define bounds on the duration of an activity during detection of duration outliers.
#'
#' @param bound_sd Number of standard deviations from the mean duration which is used to define an outlier in the absence of lower_bound and upper_bound (default value of 3 is used)
#' @param lower_bound Lower bound for activity duration used during outlier detection (expressed in minutes). This means disregarding the sd and bound_sd for lower bound
#' @param upper_bound Upper bound for activity duration used during outlier detection (expressed in minutes). This means disregarding the sd and bound_sd for upper bound
#' @seealso \code{\link{detect_duration_outliers}}
#' @return No return value, called for side effects
#' @export
#'
duration_within <- function(bound_sd = 3, lower_bound = NA, upper_bound = NA) {
  list(bound_sd = bound_sd,
       lower_bound = lower_bound,
       upper_bound = upper_bound)
}
