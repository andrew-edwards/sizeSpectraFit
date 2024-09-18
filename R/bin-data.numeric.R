##' Construct bins that either double in size or are of equal width, and encompass
##'  the data  SEE ISSUE for what to do next.  TODO THIS WILL become the generic
##'  class thing.
##'
##' Construct bins that start from `floor(min(x))` or `min(x)` and either double
##'    in size or are of equal width, and encompass the data. Adapted from
##'    [sizeSpectra::binData()].
##'
##' @param x vector of individual values (e.g. body masses). Only `x` or
##'   `counts` can be specified.
##' @param counts dataframe (or array) with first column being an x value
##'  (e.g. body mass), and second column being the counts of the
##'  number of individuals for that value. Only `x` (the vector) or `counts` can
##'   be specified.
##' @param bin_width type of bins to use:
##'   * `"2k"` will result in `bin_breaks` that:
##'     + with `start_integer = TRUE` are powers of 2, i.e. ..., 0.25, 0.5, 1, 2, 4, 8, 16,....
##'     + with `start_integer = FALSE` are bins that double in size and  start with
##'       `min(x)`; not yet implemented, since have to think about what the width of
##'       the first bin should be.
##'   * numeric value (call it `a`) will result in bin breaks that are separated by `a` and span the
##'       data, that:
##'     + with `start_integer = TRUE` start from `z = floor(min(x))` and are then
##'          `z, z+a, z+2a, z+3a, ....`   (if `z = 0` then power-law cannot be fit
##'        so then need to use `start_integer = FALSE`)
##'     + with `start_integer = FALSE` start from `z = min(x)` and are then
##'           `z, z+a, z+2a, z+3a, ....`
##'   * only `bin_width` or `bin_breaks` can be specified.
##' @param bin_breaks pre-defined bin breaks as a vector. Only `bin_width`
##'   or `bin_breaks` can be specified.
##' @param start_integer TRUE or FALSE, whether to start the bin breaks at an integer
##'   power of 2 (for method `"2k"`) or an integer. See `bin_width` above.
##'   `start_integer` is ignored if `bin_breaks` is specified.
##' @return list containing:
##'   * indiv: dataframe with a row for each `x` value, with columns:
##'      + `x`: original `x` values
##'      + `bin_mid`, `bin_min`, `bin_max`, `bin_width`: midpoint, minimum,
##'      maximum, and width, respectively, of the bin within
##'      which the `x` value falls.  If indiv has `>= 10^6` rows then it isn't saved.
##'      If `counts` was specified then an equivalent `x`
##'      vector is created and is column `x` (i.e. `x` values are repeated). May
##'      not be the most efficient way, but is easiest to program.
##'   * bin_vals: dataframe with a row for each new bin and columns:
##'      + `bin_mid`, `bin_min`, `bin_max`, `bin_width`: midpoint, minimum,
##'         maximum, and width, respectively, of the bin
##'      + `bin_count`: total number of individuals in that bin
##'      + `bin_count_norm`: normalised bin count, `bin_count / bin_width`
##'      + `bin_sum`: sum of individual values in that bin (appropriate if `x`
##'         represents biomass, but not length)
##'      + `bin_sum_norm`: `bin_sum / bin_width`
##'      + `log10....` - `log10()` of some of the above quantities
##' @export
##' @author Andrew Edwards
bin_data <- function(x = NULL,
                     counts = NULL,
                     bin_width = NULL,
                     bin_breaks = NULL,
                     start_integer = TRUE){
  if(!is.null(x) & !is.null(counts)) {
    stop("need only one of x or counts in bin_data") }
  if(is.null(x) & is.null(counts)) {
    stop("need x or counts in bin_data") }
  if(!is.null(x)) {
    if(!is.vector(x))stop("x not a vector in bin_data")
    if(anyNA(x)) stop("x cannot contain NA's in bin_data")
    if(min(x) <= 0)stop("x needs to be >0 in bin_data")
  }
  if(!is.null(counts))  {
    if(dim(counts)[2] != 2)stop("counts needs two cols in bin_data")
    if(min(counts[,1]) < 0) {
      stop("x values in counts need to be >= 0 in bin_data") }
    if(min(counts[,2]) < 0) {
      stop("numbers in counts need to be >= 0 in bin_data") }
    if(sum(!is.wholenumber(counts[,2])) > 0) {
      stop("numbers in counts need to be integers in bin_data;
                      for non-integer count see a new function. Currently,
                      such a new function has no name [so says Jaqen H'ghar]. Or it may be easier to
                      adapt bin_data.") }
  }
  if(is.null(bin_width) & is.null(bin_breaks)) {
    stop("need one of bin_width or bin_breaks in bin_data") }
  if(!is.null(bin_width) & !is.null(bin_breaks)) {
    stop("need only one of bin_width or bin_breaks in bin_data") }
  if(start_integer != "TRUE" & start_integer != "FALSE"){
    stop("start_integer must be TRUE or FALSE in bin_data") }
  # As for LBNbiom.method(), could write code that would make
  #  use of the counts dataframe explicitly, but actually quite easy
  #  to just create the longer vector x (though may be slightly slower
  #  computationally), to save writing extensive new code. But do this
  #  at some point for noninteger counts.
  if(is.null(x))
  { x = rep(counts[,1], counts[,2])
    minx = min(counts[,1])
    maxx = max(counts[,1])
  } else
  { minx = min(x)
    maxx = max(x)
  }
  #
  if(!is.null(bin_breaks))
  {
    if(minx < min(bin_breaks) | maxx > max(bin_breaks) )
    { stop("bin_breaks do not span data in bin_data")
    }
  } else           # create bin_breaks based on bin_width
  {
    if(bin_width == "2k")
    {
      if(start_integer)
      { bin_breaks = 2^( floor(log2(minx)) : ceiling(log2(maxx)) )
      } else
      { stop("start_integer currently needs to be TRUE when
                   bin_width = 2k")
      }
    } else     # If not "2k"
    {
      if(!is.numeric(bin_width))
      { stop("bin_width must be 2k or a number (in quotes is okay
                         in quotes) in bin_data().")
      }
      # start_integer says whether to start from an integer value or
      #  start from min(x),
      z = floor(minx) * start_integer + minx * !start_integer
      bin_breaks = seq( z, by=bin_width,
                       length = ceiling( (maxx - z)/bin_width) + 1)
    }
  }

        indiv = data.frame(x)       # dataframe with one row for each individual
        indiv$bin_mid = cut(x, breaks = bin_breaks, right = FALSE, include.lowest = TRUE,
            labels  =  bin_breaks[-length(bin_breaks)] + 0.5*diff(bin_breaks))
        indiv$bin_min =cut(x, breaks = bin_breaks, right = FALSE, include.lowest = TRUE,
            labels = bin_breaks[-length(bin_breaks)])
        indiv$bin_max =cut(x, breaks = bin_breaks, right = FALSE, include.lowest = TRUE,
            labels = bin_breaks[-1])
        #
        indiv$bin_mid = as.numeric(as.character(indiv$bin_mid))
        indiv$bin_min = as.numeric(as.character(indiv$bin_min))
        indiv$bin_max = as.numeric(as.character(indiv$bin_max))
        # Now calculate biomass in each bin class:
        bin_vals = dplyr::summarise(dplyr::group_by(indiv, bin_mid),
            bin_min = unique(bin_min),
            bin_max = unique(bin_max),
            bin_width = bin_max - bin_min,
            bin_count = length(x),
            bin_count_norm = bin_count / bin_width,
            bin_sum = sum(x),
            bin_sum_norm = bin_sum / bin_width )
           # bin_width uses new columns bin_max and bin_min
        # Indices for minima of bins that have zero counts and so do not
        #  appear in bin_vals yet:
        emptyBinMinInd = !(signif(bin_breaks[-length(bin_breaks)], digits = 8) %in%
                           signif(bin_vals$bin_min, digits = 8))
                         # to avoid not-real differences due to rounding/storing
        emptyBinMin = bin_breaks[emptyBinMinInd]
        empties = length(emptyBinMin)
        emptyBinMax = bin_breaks[-1][emptyBinMinInd]
        emptyBinWidth = emptyBinMax - emptyBinMin
        emptyBinMid = emptyBinMin + emptyBinWidth/2

        emptyVals = as.data.frame(cbind(emptyBinMid,
                                        emptyBinMin,
                                        emptyBinMax,
                                        emptyBinWidth,
                                        matrix(0, nrow = empties, ncol = 4)))
        names(emptyVals) = names(bin_vals)
        bin_vals = rbind(bin_vals, emptyVals)         # still a local df

        bin_vals = bin_vals[order(bin_vals$bin_mid), ]   # order by bin_mid

        bin_vals = dplyr::mutate(bin_vals,
                                log10_bin_mid = log10(bin_mid),
                                log10_bin_count = log10(bin_count),
                                log10_bin_sum = log10(bin_sum),
                                log10_bin_count_norm = log10(bin_count_norm),
                                # Had thought that last line is needed to avoid
                                # warnings (e.g. simulate-data2.R) and whole
                                # column being NA's. Maybe don't actually use it
                                # in results, but have put it in, may need to
                                # test it more.
                                log10_bin_sum_norm = log10(bin_sum_norm))
        bin_vals[is.infinite(bin_vals$log10_bin_count),
                "log10_bin_count"] = NA
                  # lm can't cope with -Inf, which appear if 0 counts in a bin
        bin_vals[is.infinite(bin_vals$log10_bin_count_norm),
                "log10_bin_count_norm"] = NA
        bin_vals[is.infinite(bin_vals$log10_bin_sum),
                "log10_bin_sum"] = NA
        bin_vals[is.infinite(bin_vals$log10_bin_sum_norm),
                "log10_bin_sum_norm"] = NA
        if(dim(indiv)[1] < 10^6) {       # only save indiv if not too big
          y = list(indiv = indiv, bin_vals = bin_vals)
          } else
          {
          y = list(bin_vals = bin_vals)
          }
        return(y)
    }
