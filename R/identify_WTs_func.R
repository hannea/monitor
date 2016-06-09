##########################################################################################
# function that converts the timestamps, creates numerics when characters, and
# identifies the different WTs and returns a list with WTs as
# dataframes with the same variables as the dataframe they came from.
##########################################################################################

#' Identifies the wind turbines if several unit IDs appear.
#'
#' Identifies the wind turbines if there are several unit IDs, and returns the same data in
#' a list containing data frames for each unit ID, and the time stamps are converted to POSIXct class.
#'
#' @param dataframe A data frame containing the data from condition monitoring. The data frame
#' should contain a column with unit IDs.
#' @param id The name as a character of the column with unit IDs.
#'
#' @return
#' \item{out}{The data in \code{dataframe} sorted by \code{id} into a list containing data frames,
#' one for each wind turbine. Tthe number of wind turbines in \code{dataframe} are printet.}
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
