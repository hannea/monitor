##########################################################################################
# Function that counts alarm and alarm rate. It uses splines fitted to the training data.
# Input: takes a training and test set of load and vibrations, vector of knots,
# and the minimum load included in the caculations.
# So far it is still neccessary to define the knots manually.
# output: count of alarms and alarm rate, and a plot of log of the vibrations
# against load, splines fitted the training data and 3 standard diviations
# above and  below the spline and alarms in the test set is plotted.
##########################################################################################

#' Count and plot alarms using a spline, assuming constant variance.
#'
#' The function uses a spline to fit to \code{vib_training} against \code{load_training}.
#' An alarm is generated when the test data fall outside a band of three sigmas.
#' The function generates a plot by default, showing the training data falling outside the 3 sigma band.
#'
#' @param load_training A vector containing the training data of the load variables such as power or generator speed.
#' @param vib_training A vector containing the training data of the vibration signal.
#' @param knots A vector containing the locations of the knots.
#' @param load_test A vector with the test data of the load variable.
#' @param vib_test A vector with the test data of the vibration signal.
#' @param show_figure A logical vector indicating if a plot should be made.
#' @param load_min The minimum load to be used.
#'
#' @return A list of the count of the number of alarms, alarm rate, and residuals. Furthermore a plot is
#' generated by default set by \code{show_figure}.
#' \item{count}{A count of the number of alarms in the test set.}
#' \item{alarm_rate}{The alarm rate in the test set.}
#' \item{residuals}{The residuals from the test set.}
#' @export
#'
#' @examples count_alarms_constant_var_faster(ltrain, vtrain, knots = c(20,25,27),
#'                                  ltest, vtest, load_min = 17)

count_alarms_constant_var_faster = function(load_training, vib_training, knots,
                                            load_test, vib_test, show_figure = TRUE,
                                            load_min = 0){
  ## knots need to be specified
  if (!is.vector(knots)) {
    stop("knots must be defined as a vector")}

  ii = load_training >= load_min
  jj = load_test >= load_min
  load_training = load_training[ii]
  vib_training  = vib_training[ii]
  load_test = load_test[jj]
  vib_test  = vib_test[jj]

  require(splines)
  fit = lm(vib_training ~ ns(load_training, knots = knots))
  sum = summary(fit)
  sigma = sum$sigma

  ## test data
  pred_test = predict(fit, data.frame(load_training = load_test, vib_test))
  which_outside = vib_test < pred_test - 3*sigma |
    vib_test > pred_test + 3*sigma
  count = sum(which_outside, na.rm = TRUE)
  res = vib_test - pred_test
  # print(length(load_test)) # 12467
  # print(length(res)) # 12467

  #######################
  ## plotting features ##
  #######################
  if (show_figure) {
    par(mar = c(4,4,2,1))
    plot(load_training, vib_training, main = "Constant Variance", pch = 20, col = rgb(0,0,0,0.02),
         xlab = "load", ylab = "vibration")
    abline(v = knots, col = "darkgray", lty = 3)

    # plot spline to training data
    load_vec = seq(min(load_training[!is.na(load_training)]),
                   max(load_training[!is.na(load_training)]), length.out = 100)
    pred = predict(fit, data.frame(load_training = load_vec), se.fit = T)

    # plot 3 sigma band
    lines(load_vec, predict(fit, data.frame(load_training=load_vec)), lwd=2, col="red")
    lines(load_vec , pred$fit + 3* sigma ,lty = "dashed", col = "red")
    lines(load_vec , pred$fit - 3* sigma ,lty = "dashed", col = "red")
    points(load_test[which_outside], vib_test[which_outside], col = "green", pch = 20) # not plotting when NA
    legend("topleft", c("spline","3 sigma band", "test set outside band"), lwd = c(1,1,1),
           col = c("red","red","green"), lty = c(1,2,0), pch = c(NA,NA,20), cex = 0.8)
  }
  ###################
  ## output values ##
  ###################
  alarm_rate = count/length(load_training)

  out = list("count" = count, "alarm_rate" = alarm_rate, "residuals" = res)
  return(out)
}
