##########################################################################################
# Function that implements the Kalman filter, when the covariances and the initial state
# of the state process are known.
##########################################################################################

#' Kalman filter applied to multivariate random walk.
#'
#' Function that applies the Kalman filter to four observations assuming the
#' state is a univariate random walk. The observation and state equations
#' are \eqn{\bm{y}_t = Fx_t + \bm{v}_t} and \eqn{x_t = x_{t-1} + w_t}, respectively.
#'
#' @param ts A matrix containing the data from a multivariate time series.
#' @param R The covariance of the measurement noise.
#' @param Q The covariance of the state noise.
#' @param m0 Initial state.
#' @param C0 Initial covariance of the state process.
#'
#' @return Returns a list of the state values and covariances.
#' \item{state_values}{The state values.}
#' \item{state_cov}{The state covariances.}
#' @export
#'
# ' @examples Kalman_filter_random_walk(data)

Kalman_filter_random_walk = function(ts, F=c(1,1,1,1), R = 0.1*diag(4), Q = 0.1, m0 = 0, C0 = 1){ # C0 different from zero or no noise
  # initial step
  x_t_correct = m0
  P_t_correct = C0

  x_vec = x_t_correct
  P_vec = P_t_correct

  for (i in 1:length(ts[1,])) {
    # Time Update (prediction)
    x_t_predict = x_t_correct # G=1 since random walk
    P_t_predict = P_t_correct + Q # 1x1

    # Prediction error
    e = t(t(ts[,i] - t(t(F)) %*% x_t_predict)) # 4x1

    # Measurement Update (correction)
    K_temp      = P_t_predict %*% t(F) %*% solve(t(t(F)) %*% P_t_predict %*% (t(F)) + R) # 1x4
    x_t_correct = t(x_t_predict) + K_temp %*% e # 1x1
    P_t_correct = P_t_predict - K_temp %*% t(t(F)) %*% P_t_predict

    x_vec = c(x_vec, x_t_correct)
    P_vec = c(P_vec, P_t_correct)
  }
  return(list("state_values" = x_vec, "state_cov" = P_vec))
}
