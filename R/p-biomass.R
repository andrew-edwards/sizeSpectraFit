##' Biomass distribution function from MEE equations A.4 and A.8
##'
##' Total biomass between `xmin` and `x`, assuming a bounded power-law
##' distribution of body masses between `xmin` and `xmax` and a given value of
##' exponent `b`, and a total of `n` individuals.
##'
##' Given by MEE equations A.4 and A.8. Can then be called by [p_biomass_bins()]
##' to give total biomass (and normalised biomass) in each bin.
##'
##' ss need to adapt style:
##' @param x vector of values for which to calculate the total biomass between
##' `xmin` and the value
##' @param b estimated exponent of the PLB distribution
##' @param xmin minimum bound of the distribution, `xmin > 0`
##' @param xmax maximum bound for bounded distribution, `xmax > xmin`
##' @param n
##' @return return vector of total biomass between `xmin` and each value of `x`
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' p_biomass(x = c(1, 5, 10, 20, 50, 100),
##'           b = -2,
##'           xmin = 1,
##'           xmax = 100,
##'           n = 1000)
##' }
p_biomass <- function(x,
                      b,
                      xmin,
                      xmax,
                      n){

  if(xmin <= 0 | xmin >= xmax | min(x) < xmin | max(x) > xmax | n <= 0){
    stop("Parameters out of bounds in p_biomass")
  }

  if(b != -1){
    C <- (b+1) / ( xmax^(b+1) - xmin^(b+1) )
  } else {
    C <- 1/ ( log(xmax) - log(xmin) )
  }

  if(b != -2){
    biomass <- n * C * (x^(b+2) - xmin^(b+2)) / (b + 2)
  } else {
    biomass <- n * C * (log(x) - log(xmin))
  }

  return(biomass)
}
