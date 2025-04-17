##' Given histogram object of counts and bins, determine the mode and return min of that bin as xmin
##'
##' @param h histogram object, e.g. from running `make_hist()` on a vector
##' @return x_min to use to fit ISD, and to plot the histogram to show the full
##'   data and the mode.
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##'  TODO
##' }
determine_xmin <- function(h){
  stopifnot("h needs to be a histogram object" =
              class(h) == "histogram")

  max_ind <- which.max(h$counts)    # returns the first one if they are ties

  # if(max_ind == length(h$counts)){
  #   stop("The maximum count is in the final bin, so a descending PLB is not appropriate.")
  # }     # Also the fitting code will likely break.

  x_min <- h$breaks[max_ind]

  x_min
}
