##########################################################################################pack <- "monitor"

# function that converts the timestamps, creates numerics when characters, and
# identifies the different WTs and returns a list with WTs as
# dataframes with the same variables as the dataframe they came from.
##########################################################################################

#' Identifies the wind turbines if several unit IDs appear.
#'
#' Identifies the wind turbines if there are several unit IDs, and returns the same data in a list containing dataframes for each unit ID, and the time stamps are converted to POXIXct class.
#'
#' @param dataframe A data frame containing the data from condition monitoring.
#' @param id The name as a character of the column with unit IDs.
#'
#' @return The data in \code{dataframe} sorted by \code{id} into a list containing dataframes, one for each wind turbines, and the time stamps are converted to POSIXct class.
#' @export
#'
#' @examples
#' identify_WTs_func(data, id = "UnitID")


identify_WTs_func = function(dataframe, id = names(dataframe[1])){
  dataframe[["CMSTimeStamp"]] = as.POSIXct(dataframe[["CMSTimeStamp"]], tz = "GMT")

  out = split(dataframe,dataframe[[id]])
  n = length(out)

  names(out) = paste("WT", 1:n, sep = "")
  cat("Number of WTs = ", n, "\n")
  return(out)
}
