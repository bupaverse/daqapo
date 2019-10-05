#' @title DaQAPO - Data Quality Assessment for Process-oriented Data
#'
#' @description This package is designed to perform data quality assessment on process-oriented data.
#'
#' @docType package
#' @name DaQAPO
#'
#' @import dplyr
#' @importFrom lubridate ymd_hms
#' @importFrom lubridate dmy_hms
#' @importFrom lubridate ymd
#' @importFrom lubridate dmy
#' @importFrom lubridate is.POSIXct
#' @importFrom stringdist ain
#' @importFrom stringr str_to_lower
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_split
#' @importFrom stringr str_squish
#' @importFrom stringr str_to_upper
#' @importFrom stats sd
#' @importFrom tidyr spread
#' @importFrom tidyr spread_
#' @importFrom utils read.csv
#' @importFrom utils capture.output
#' @importFrom xesreadR read_xes
#'

utils::globalVariables(c(".","case_id","activity","start","complete","resource"))
NULL
