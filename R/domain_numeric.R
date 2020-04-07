#' Define allowable range of values
#'
#' @param from Minimum of allowed range
#' @param to Maximum of allowed range
#' @seealso \code{\link{detect_value_range_violations}}
#' @return No return value, called for side effects
#' @export
#'


domain_numeric <- function(from, to) {
  list(type = "numeric",
       from = from,
       to = to) -> range
  class(range) <- c("value_range", class(range))
  range
}
