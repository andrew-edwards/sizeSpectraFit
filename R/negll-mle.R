##' Calculate negative log-likelihood for the bounded power-law
##'   distribution
##'
##' Calculate the negative log-likelihood of the parameters `b`, `x_min` and
##'   `x_max` given data `x` for the PLB model. Returns the negative
##'   log-likelihood. Will be called by `nlm()` or similar. `x_min` and `x_max`
##'   are just estimated as the min and max of the data, not numerically using likelihood.
##' @param b value of `b` for which to calculate the negative log-likelihood
##' @param x vector of values of data (e.g. masses of individual fish)
##' @param n `length(x)`, have as an input to avoid repeatedly calculating it when
##'   function is called multiple times in an optimization routine
##' @param x_min minimum value of `x` to avoid repeatedly calculating
##' @param x_max maximum value of `x` to avoid repeatedly calculating
##' @param sum_log_x `sum(log(x))` to avoid repeatedly calculating
##' @return numeric negative log-likelihood of the parameters given the data
##' @export
##' @author Andrew Edwards
neg_ll_mle = function(b,
                      x,
                      n,
                      x_min,
                      x_max,
                      sum_log_x){
  if(x_min <= 0 | x_min >= x_max) stop("Parameters out of bounds in negLL.PLB")
  if(b != -1){
    negll = -n * log( ( b + 1) / (x_max^(b + 1) - x_min^(b + 1)) ) -
      b * sum_log_x
  } else {
    negll = n * log( log(x_max) - log(x_min) ) + sum_log_x
  }
  return(negll)
}
