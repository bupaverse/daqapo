#' Define allowable range of values
#'
#' @param allowed Allowed values of categorical column (character or factor)
#' @seealso \code{\link{detect_value_range_violations}}
#' @export
#'


domain_categorical <- function(allowed) {
  list(type = "categorical",
       allowed = allowed) -> range
  class(range) <- c("value_range", class(range))
  range
}
