##########################################################################################
# Function that counts alarm and alarm rate. It uses splines fitted to the means
# of quantiles and is therefore sensitive to the knots, due to the decreased sample size.
# Input: takes a training and test set of load and vibrations, vector of knots,
# number of bins and the minimum load included in the calculations.
# So far it is still neccessary to define the knots manually.
# output: count of alarms and alarm rate, and a plot of log of the vibrations
# against load, splines fitted to the means and 3 standard diviations above and
# below the means and alarms in the test set is plotted.
##########################################################################################

#' Count and plot alarms using a spline with heterogeneous variance.
#'
#' @param load_training A vector containing the training data of the load variables such as power or generator speed.
#' @param vib_training A vector containing the Vibration signal.
#' @param knots A vector containing the locations of the knots.
#' @param load_test A vector with the test data.
#' @param vib_test A vector with the test data.
#' @param show_figure A logical vector indicating if a plot should be made.
#' @param show_summary A logical vector stating whether the quantile summaries should be shown or not.
#' @param bins The number of bins used.
#' @param load_min The minimum load used.
#'
#' @return  A count of the number of alarms, alarm rate, and residuals. Further a plot is
#' generated if \code{show_figure = TRUE}.
#' @export
#'
#' @examples count_alarms_hetero_var_faster(ltrain, vtrain, knots = c(20,25,27), ltest, vtest, load_min = 17)


count_alarms_hetero_faster = function(load_training, vib_training, knots, load_test, vib_test,
                               show_figure = TRUE, show_summary = FALSE, bins = 160,
                               load_min = 0){

  ## subset the data to above load_min
  ii = load_training >= load_min
  jj = load_test >= load_min
  load_training = load_training[ii]
  vib_training  = vib_training[ii]
  load_test = load_test[jj]
  vib_test  = vib_test[jj]

  quantile_load = unique(quantile(load_training, probs = (0:bins)/bins, na.rm = TRUE)) # 89
  # many not unique quantiles only 89 out of 160
  factor_load = cut(load_training, breaks = quantile_load)          # factor 11493
  sd_vib  = tapply(vib_training, factor_load, sd, na.rm = TRUE)                         # 88
  mean_vib = tapply(vib_training, factor_load, mean ,na.rm = TRUE)                      # 88
  quantile_mean = (quantile_load[-1] + quantile_load[1:length(quantile_load) - 1])/2   # 88
  # midpoints of quantile breaks

  upper3sigma = mean_vib + 3 * sd_vib
  lower3sigma = mean_vib - 3 * sd_vib

  require(splines)
  fit_mean_vib    = lm(mean_vib ~ ns(quantile_mean,knots = knots))
  fit_upper3sigma = lm(upper3sigma ~ ns(quantile_mean,knots = knots))
  fit_lower3sigma = lm(lower3sigma ~ ns(quantile_mean,knots = knots))

  pred_test = predict(fit_mean_vib, data.frame(quantile_mean = load_test))

  ## count number in the test set which is outside the 3 sigma band
  predict_temp_upper = predict(fit_upper3sigma, data.frame(quantile_mean = load_test))
  predict_temp_lower = predict(fit_lower3sigma, data.frame(quantile_mean = load_test))

  which_outside = vib_test < predict_temp_lower | vib_test > predict_temp_upper

  ## output values
  count = sum(which_outside, na.rm = TRUE)
  alarm_rate = count/length(load_test)
  res = vib_test - pred_test

  ######################
  ## ploting features ##
  ######################
  if (show_figure) {
    load_vec = seq(min(load_training[!is.na(load_training)]),
                   max(load_training[!is.na(load_training)]), length.out = 100)
    predict_mean_vib_spline = predict(fit_mean_vib, data.frame(quantile_mean = load_vec))
    predict_upperspline = predict(fit_upper3sigma, data.frame(quantile_mean = load_vec))
    predict_lowerspline = predict(fit_lower3sigma, data.frame(quantile_mean = load_vec))

    par(mar = c(4,4,2,1))
    plot(load_training,vib_training, main = "Heteroscedastic case with quantiles",
         pch = 20, col = rgb(0,0,0,0.02), xlab = "Load", ylab = "Vibrations")
    legend("topleft", c("mean of quantiles", "spline from mean of quantiles",
                            "3 sigma band", "test set outside band"), lwd = c(1,1,1,1),
           col = c("blue","red","red","green"), lty = c(0,1,2,0), pch = c(20,NA,NA,20), cex = 0.8)
    abline(v = knots, col = "darkgray", lty = 3)
    points(quantile_mean, mean_vib, col = "blue", pch = 20)
    ## use several points to show spline instead of just the means and upper/lower3sigma
    lines(load_vec,predict_mean_vib_spline,col = "red",lwd = 2) # 100 equally spaced predicted
    lines(load_vec,predict_upperspline,col = "red",lty = 2) # same for upper
    lines(load_vec,predict_lowerspline,col = "red",lty = 2) # same for lower
    points(load_test[which_outside], vib_test[which_outside], col = "green", pch = 20)
  }

  if (show_summary & show_figure) {
    ## quantile summaries, mean, 3 sigma band
    points(quantile_mean, upper3sigma, col = "red") # upper 3 sigma from quantiles
    points(quantile_mean, lower3sigma, col = "red") # lower 3 sigma from quantiles
    points(quantile_mean, mean_vib, col = "red")#, type = "o")
  }

  out = list("count" = count, "alarm_rate" = alarm_rate, "residuals" = res)
  return(out)
}

