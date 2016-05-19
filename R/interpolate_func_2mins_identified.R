##########################################################################################
# Function that takes an "identified" Vestas file as input (generated with the function
# identify_WTs_func) and uses the approx (spline) function to interpolate.
# Gaps with more than 10 mins are skipped and replaced with NAs, and so are small intervals
# with 10 or less points.
# The interpolatin is at exactly 2 mins spaces.
##########################################################################################

#' Interpolation of identified data frame using 2 minute intervals.
#'
#' The interpolation of several wind turbines with an interval length of 2 minutes. If there
#' are intervals larger than 10 minuttes missing data, no interpolation in done, and NAs are added.
#'
#' @param dataframes A list of data frames as a result of the identified function with several
#' wind turbines.
#' @param var The variable to be interpolated in \code{dataframes}.
#'
#' @return A data frame with time stamps and where the \code{var} has been interpolated
#' using 2 minute intervals.
#' @export
#'
#' @examples interpolate_func_2mins(data, var = "GeneratorSpeed")

interpolate_func_2mins_identified = function(dataframes, var = "PowerActual"){
  start = trunc(min(dataframes[[1]][["CMSTimeStamp"]]), "hours") # rounding down to hour or use round()
  end   = max(dataframes[[1]][["CMSTimeStamp"]])
  interval = seq(start, end, by = "2 min")

  NA_vector = rep(NA,length(interval))
  dataframe_intervals = data.frame(interval,NA_vector)

  out  = NULL
  out[[1]] = interval

  times_missing = NULL
  n = length(dataframes)

  # loop over the n WTs
  for(i in 1:n){
    timeStamps = dataframes[[i]][["CMSTimeStamp"]]
    diff_greater_than = diff(timeStamps) > as.difftime(10, units = "mins") # gaps with more than 10 min
    # dif is the number of last in interval diff+1 is a big skip
    print(sum(diff_greater_than)) # 314 if 10 min. length(diff_greater_than_5) = 197559
    variable = dataframes[[i]][[var]]

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

          startStamp = dataframes[[i]][["CMSTimeStamp"]][start_index]
          endStamp   = dataframes[[i]][["CMSTimeStamp"]][j]

          start_sub = which.min(abs(interval - trunc(startStamp,"mins")))
          end_sub = which.min(abs(interval - trunc(endStamp,"mins")))

          spline_sub = approx(timeStamps[start_index:j], variable[start_index:j],
                              xout = interval[start_sub:end_sub])

          time_int = c(time_int,.POSIXct(spline_sub$x, tz = "GMT")) # spline removes POSIXct class
          out_int = c(out_int, spline_sub$y)

          start_index = j + 1 # add one to jump across the interval without meassures
        }
      }
    }

    # apply(abs(outer(time_var_spline$x,time_secs,"-")),1,min)>4*120 # way to big!!!
    times_missing[[i]] = .POSIXct(time_int, tz = "GMT") # all intervals in WTi combined
    dataframe_missing =  data.frame(times = times_missing[[i]], values = out_int)

    # time_new = merge(dataframe_missing, dataframe_intervals)

    out[[i+1]] = with(dataframe_missing, values[ match(interval, times)]) # fill in NAs on missing points

  }
  names(out)[1] = "time_stamps"
  names(out)[2:(n+1)] = paste(var,"_spline", 1:n, sep = "")
  return(data.frame(out))
}
