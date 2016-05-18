##########################################################################################
# Interpolation function that takes a single case.
##########################################################################################

#' Interpolation function using 2 minutes intervals.
#'
#' The interpolation of a single wind turbine with an interval length of 2 minutes. If there
#' are intervals larger than 10 minuttes missing data, no interpolation in done, and NAs are added.
#'
#' @param dataframe A dataframe containing a single wind turbine case.
#' @param var The variable to be interpolated in \code{dataframe}.
#'
#' @return A new dataframe similar to the existing where the \code{var} has been interpolated
#' using 2 minutes intervals.
#' @export
#'
#' @examples interpolate_func_2mins(data, var = "GeneratorSpeed")
#'

interpolate_func_2mins = function(dataframe, var = "PowerActual"){
  start = trunc(min(dataframe[["CMSTimeStamp"]]), "hours") # rounding down to hour or use round()
  end   = max(dataframe[["CMSTimeStamp"]])
  interval = seq(start, end, by = "2 min")

  NA_vector = rep(NA,length(interval))
  dataframe_intervals = data.frame(interval,NA_vector)

  out  = NULL
  out[[1]] = interval

  times_missing = NULL
  n = length(dataframe)

  # No loop, since only one case
  timeStamps = dataframe[["CMSTimeStamp"]]
  diff_greater_than = diff(timeStamps) > as.difftime(10, units = "mins") # gaps with more than 10 min
  # diff is the number of last in interval diff+1 is a big skip
  #print(sum(diff_greater_than)) # 314 if 10 min. length(diff_greater_than_5) = 197559
  variable = dataframe[[var]]

  # loop all j intervals between missing meassurements
  start_index = 1
  time_int = c()
  out_int = c()
  for (j in 1:length(diff_greater_than)) {
    if (diff_greater_than[j] == TRUE) { # these two lines in one
      if (sum(!is.na(variable[start_index:j])) < 30) {# (j < start_index + 9) { # skip intervals with few points - less than 10
        start_index = j + 1 #start_index + 9
      }
      else {

        startStamp = dataframe[["CMSTimeStamp"]][start_index]
        endStamp   = dataframe[["CMSTimeStamp"]][j]

        start_sub = which.min(abs(interval - trunc(startStamp,"mins")))
        end_sub = which.min(abs(interval - trunc(endStamp,"mins")))

        spline_sub = approx(timeStamps[start_index:j], variable[start_index:j], #spline()
                            xout = interval[start_sub:end_sub])

        time_int = c(time_int,.POSIXct(spline_sub$x, tz = "GMT")) # spline removes POSIXct class
        out_int = c(out_int, spline_sub$y)

        start_index = j + 1 # add one to jump across the interval without meassures
      }
    }
  }

  times_missing = .POSIXct(time_int, tz = "GMT") # all intervals in WT combined
  dataframe_missing =  data.frame(times = times_missing, values = out_int)

  out[[2]] = with(dataframe_missing, values[ match(interval, times)]) # fill in NAs on missing points

  names(out)[1] = "time_stamps"
  names(out)[2] = paste(var,"_interpolated", sep = "")

  return(data.frame(out))
}
