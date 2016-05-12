##########################################################################################
# Function that implements the Kalman filter on a univariate ARMA on dlm form. The
# covariances and the initial state of the state process are known.
# It is stationary, and there is no noise in the mesurement noise is zero.
##########################################################################################


#' Kalman filter applied to a univariate stationary zero-mean ARMA process.
#'
#' The dynamic linear model, is on the form, where \eqn{y_t = F\bm{x}_t} is the observation equation and \eqn{\bm{x}_t = G\bm{x}_{t-1} + H w_t} is the state equation, and \eqn{Q = \mbox{Var}(H w_t)}.
#'
#' @param ts A univariate time series with zero mean
#' @param F The coefficient matrix in the observation equation, as shown above.
#' @param G The matrix in the state equation as shown above.
#' @param Q The variance matrix of the state equation.
#' @param m0 The initial value of \eqn{\bm{x}_t}.
#' @param C0 The initial value of the state variance.
#'
#' @return The innovations, the standardized residual process and the predicted values.
#' @export
#'
#' @examples kalman_filter_arma(ts = data, F=F, G=G, Q=Q, m0=m0, C0=C0)
#'

kalman_filter_arma = function(ts, F, G, Q, m0, C0){

  # all inputs should be matrices
  if(!is.matrix(F) | !is.matrix(G) | !is.matrix(Q) | !is.matrix(C0))
    {stop("F, G, Q, and C0 must all be matrices")}

  # initial step
  x_t_correct = m0 # px1
  P_t_correct = C0 # pxp

  y_pred = NULL
  e = NULL
  sd = NULL

  # to save time store the transposed
  tF = t(F)
  tG = t(G)

  for (i in 1:length(ts)) {
    # Prediction
    x_t_predict = G %*% x_t_correct
    P_t_predict = G %*% P_t_correct %*% tG + Q

    y_t_predict    = F %*% x_t_predict
    var_y_t_predict = F %*% P_t_predict %*% tF # R is zero

    # Prediction error (innovations)
    e_temp = ts[[i]] - y_t_predict        # 1x1, is NA if ts[[i]] is

    if (is.na(ts[[i]])) {

      # no correction
      x_t_correct = x_t_predict
      P_t_correct = P_t_predict

      sd_temp  = NA

    } else {

      # Correction
      K_temp      = P_t_predict %*% tF %*% solve.default(F %*% P_t_predict %*% tF) # 3x1
      x_t_correct = x_t_predict + K_temp %*% e_temp                        # 3x1
      P_t_correct = P_t_predict - K_temp %*% F %*% P_t_predict             # 3x3

      sd_temp  = e_temp / sqrt(var_y_t_predict)
    }

    # output
    y_pred[i] = y_t_predict
    e[i] = e_temp
    sd[i] = sd_temp
  }
  return(list("innovations" = e, "sd" = sd, "y_predicted" = y_pred))
}
