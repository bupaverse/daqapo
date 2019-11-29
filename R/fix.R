


#' Fix problems
#'
#' @param detected_problems
#'
#'
#' @export
fix <- function(detected_problems, ...) {
  UseMethod("fix")
}

#' @describeIn fix Fix detected problems
#' @export

fix.detected_problems <- function(detected_problems, ...) {
  type <- attr(detected_problems, "type")

  FUN <- switch(type,
                "resource_inconsistencies" = fix_resource_inconsistencies)

  FUN(attr(detected_problems, "eventlog"), detected_problems = detected_problems, ...)

}
