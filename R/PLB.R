##' Probability density and distribution functions, and random number
##' generation, for the bounded power-law distribution.
##'
##' For the bounded power-law distribution (PLB) functions in the standard R
##' format (like [dnorm()] etc.) for density, distribution, and random number generation.
##' These are:
##'  * probability density function, [dPLB()]
##'  * cumulative distribution function P(X <= x), [pPLB()]
##'  * random generation of values [rPLB()],
##' as described in Edwards et al. (2017, Methods in Ecology and
##' Evolution, 8:57-67). Random generation uses the inverse method (e.g. p1215 of Edwards
##' 2008, Journal of Animal Ecology, 77:1212-1222). [qPLB()] is from inverting the
##' cumulative distribution function. Unbounded distributions
##' are also included for completeness (except for qPL) but are likely not needed.
##'
##' @param x vector of values to compute the density and distribution functions.
##' @param n number of random numbers to be generated (if `length(n) > 1` then
##' generate `length(n)` values)
##' @param b exponent of the distribution (must be <-1 for [PL()])
##' @param p vector of probabilities for `qPLB()`
##' @param xmin minimum bound of the distribution, `xmin > 0`
##' @param xmax maximum bound for bounded distribution, `xmax > xmin`
##' @return [dPLB()] returns vector of probability density values
##' corresponding to `x`. [pPLB()] returns vector of cumulative
##' distribution values P(X <= x) corresponding to `x`. [rPLB()] returns
##' a vector (of length `n`) of independent random draws from the distribution.
##' [qPLB()] returns vector of values of `x` for which `P(X <= x) = p` (each
##' element corresponding to the element of `p`). So
##' `pPLB(qPLB(seq(0, 1, by = 0.1)))` gives `0, 0.1, ..., 1`.
##' @name PLB
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' rPLB(10)
##' rPLB(10, b = -2.5)
##' dPLB(1:10, xmin = 2)
##' pPLB(1:10, xmin = 2)
##' qPLB(c(0.05, 0.5, 0.95))
##' }
NULL

##' @rdname PLB
##' @export
dPL <- function(x = 1,
                b = -2,
                xmin = 1){
  if(b >= -1 | xmin <= 0) stop("Parameters out of bounds in dPL")
  C <- - (b+1) / xmin^(b+1)
  y <- 0 * x     # so have zeros where x < xmin
  y[x >= xmin] <- C * x[x >= xmin]^b
  return(y)
}

##' @rdname PLB
##' @export
pPL <- function(x = 10,
                b = -2,
                xmin = 1){
  if(b >= -1 | xmin <= 0) stop("Parameters out of bounds in qPL")
  y <- 0 * x     # so have zeros where x < xmin
  y[x >= xmin] <- 1 - (x[x >= xmin]/xmin)^(b+1)
  return(y)
}

##' @rdname PLB
##' @export
rPL <- function(n = 1,
                b = -2,
                xmin = 1){
  if(b >= -1 | xmin <= 0) stop("Parameters out of bounds in rPL")
  u <- runif(n)
  y <- xmin * ( 1 - u ) ^ (1/(b+1))
  return(y)
}

##' @rdname PLB
##' @export
dPLB <- function(x = 1,
                 b = -2,
                 xmin = 1,
                 xmax = 100){
  if(xmin <= 0 | xmin >= xmax) stop("Parameters out of bounds in dPLB")
  if(b != -1){
    C <- (b+1) / ( xmax^(b+1) - xmin^(b+1) )
  } else {
    C <- 1/ ( log(xmax) - log(xmin) )
  }
  y <- 0 * x     # so have zeros where x < xmin or x > xmax
  y[x >= xmin & x <= xmax] <- C * x[x >= xmin & x <= xmax]^b
  return(y)
}

##' @rdname PLB
##' @export
pPLB <- function(x = 10,
                 b = -2,
                 xmin = 1,
                 xmax = 100){
  if(xmin <= 0 | xmin >= xmax) stop("Parameters out of bounds in pPLB")
  y <- 0 * x        # so have zeros where x < xmin
  y[x > xmax] <- 1
  if(b != -1){
    xmintobplus1 <- xmin^(b+1)
    denom <- xmax^(b+1) - xmintobplus1
    y[x >= xmin & x <= xmax] <-
      ( x[x >= xmin & x <= xmax]^(b + 1) -
        xmintobplus1 ) / denom
  } else {
    logxmin <- log(xmin)
    denom <- log(xmax) - logxmin
    y[x >= xmin & x <= xmax] =
      ( log( x[x >= xmin & x <= xmax] ) - logxmin ) / denom
  }
  return(y)
}

##' @rdname PLB
##' @export
rPLB <- function(n = 1,
                 b = -2,
                 xmin = 1,
                 xmax = 100){
  if(xmin <= 0 | xmin >= xmax) stop("Parameters out of bounds in rPLB")
  u <- runif(n)
  if(b != -1){
    y <- ( u*xmax^(b+1) +  (1-u) * xmin^(b+1) ) ^ (1/(b+1))
  } else {
    y <- xmax^u * xmin^(1-u)
  }
  return(y)
}

##' @rdname PLB
##' @export
qPLB <- function(p = 0.1,
                 b = -2,
                 xmin = 1,
                 xmax = 100){
  if(xmin <= 0 | xmin >= xmax | min(p) < 0 | max(p) > 1) stop("Parameters out of bounds in qPLB")
  if(b != -1){
    x = (p * xmax^(b+1) + (1 - p) * xmin^(b+1))^(1/(b+1))
  } else {
    x = xmax^p * xmin^(1-p)
  }
  return(x)
}
