


#' Fix problems
#'
#' @param detected_problems Output of a detect_ function. Currently supported: detect_resource_inconsistencies.
#'
#'
#' @export
fix <- function(detected_problems) {
  UseMethod("fix")
}

#' @export

fix.detected_problems <- function(detected_problems) {
  type <- attr(detected_problems, "type")

  FUN <- switch(type,
                "resource_inconsistencies" = fix_resource_inconsistencies)

  FUN(attr(detected_problems, "eventlog"), detected_problems = detected_problems, ...)

}
