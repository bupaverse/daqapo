#' Define allowable time range
#'
#' @param from Start time interval
#' @param to End time interval
#' @param format Format of to and from (either ymd_hms, dmy_hms, ymd_hm, ymd, dmy, dmy, ...). Both from and to should have the same format.
#'
#' @seealso \code{\link{detect_value_range_violations}}
#' @return No return value, called for side effects

#' @export
#'
domain_time <- function(from, to, format = ymd_hms) {

  if(!(deparse(substitute(format))  %in% c("ymd_hms", "ymd_hm", "ymd_h","ymd",
                                           "dmy_hms", "dmy_hm", "dmy_h", "dmy",
                                           "mdy_hms", "mdy_hm", "mdy_h", "mdy")) | length(format) > 1) {
    stop("format should be one of the following: ymd_hms, ymd_hm, ymd_h, ymd, dmy_hms, dmy_hm, dmy_h, dmy, mdy_hms, mdy_hm, mdy_h, md")
  }

  list(type = "time",
       from = format(from),
       to = format(to)) -> range
  class(range) <- c("value_range", class(range))
  range

}
