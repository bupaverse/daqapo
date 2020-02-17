#' @title daqapo - Data Quality Assessment for Process-oriented Data
#'
#' @description This package is designed to perform data quality assessment on process-oriented data.
#'
#' @docType package
#' @name daqapo
#'


#' @importFrom lubridate is.POSIXct
#' @importFrom stringdist ain
#' @importFrom stringr str_length
#' @importFrom stringr str_to_lower
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_split
#' @importFrom stringr str_squish
#' @importFrom stringr str_to_upper
#' @importFrom stringr str_subset
#' @importFrom stringr str_c
#' @importFrom stats sd
#' @importFrom stats na.omit
#' @importFrom tidyr spread
#' @importFrom tidyr gather
#' @importFrom utils read.csv
#' @importFrom utils capture.output
#' @importFrom xesreadR read_xes
#' @import bupaR
#' @import edeaR
#' @import dplyr
#' @importFrom rlang expr_text
#' @importFrom rlang enquo
#' @importFrom rlang :=
#'

utils::globalVariables(c("."))
NULL
