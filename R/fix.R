#' Fix problems
#'
#' @param detected_problems Output of a detect_ function. Currently supported: detect_resource_inconsistencies.
#' @param ... Additionals parameters, depending on type of anomalies to fix.
#' @return No return value, called for side effects
#'
#' @export
fix <- function(detected_problems, ...) {
  UseMethod("fix")
}

#' @export

fix.detected_problems <- function(detected_problems, ...) {
  type <- attr(detected_problems, "type")

  FUN <- switch(type,
                "resource_inconsistencies" = fix_resource_inconsistencies)
  eventlog <- attr(detected_problems, "eventlog")

  FUN(eventlog, detected_problems = detected_problems, ...)

}
