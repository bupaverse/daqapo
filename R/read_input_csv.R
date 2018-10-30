#' Read input csv dataset
#'
#' Function which reads input csv dataset. Depending on the specification of the function's parameters, it also replaces particular cell values by NA.
#' @param file_name The file name of the input csv dataset
#' @param empty_cells_are_NA Boolean indicating whether empty cells should be considered as NA
#' @param zero_values_are_NA Boolean indicating whether zero values (i.e. 0) should be considered as NA
#' @return Input csv dataset with appropriate formatting of NA values
#' @export
read_input_csv <- function(file_name, empty_cells_are_NA = TRUE, zero_values_are_NA = FALSE){

  if(empty_cells_are_NA == TRUE & zero_values_are_NA == TRUE){
    raw_activity_log <- read.csv(file_name, header = TRUE, sep = ",", stringsAsFactors = FALSE, na.strings = c("NA", "", " ", "\t", 0))
  } else if(empty_cells_are_NA == TRUE){
    raw_activity_log <- read.csv(file_name, header = TRUE, sep = ",", stringsAsFactors = FALSE, na.strings = c("NA", "", " ", "\t"))
  } else{
    raw_activity_log <- read.csv(file_name, header = TRUE, sep = ",", stringsAsFactors = FALSE, na.strings = "NA")
  }

  return(raw_activity_log)
}

