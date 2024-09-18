##' Construct bins either double in size or are of equal width, and encompass
##'  the data, and allows for non-integer counts.
##'
##' Taking from `goodnessOfFit/bin_data()` and adapting here, tidying up case
##' etc.
##'
##' Adapted from sizeSpectra::binData() that did not allow for non-integer
##' counts. Had to write new function as non-integer counts implies we only have
##' the `counts_df` dataframe input, cannot have the `x` vector of individual
##' values.
##'
##' This sums the counts of each value into bins.
##'
##' TODO Construct bins that start from `floor(min(x))` or `min(x)` and either double
##'    in size or are of equal width, and encompass the data. More generalised
##'    version of `log2bins()`.
##'
##' @param counts_df dataframe (or array, can be a tibble) with first column `x` being the measured values
##'  (e.g. body masses or lengths), and second column `counts` being the counts of the
##'  number of individuals for that value. The `counts` column can have
##'   non-integer values.
##' @param bin_width type of bins to use:
##'   * `"2k"` will result in `bin_breaks` that:
##'     + with `start_integer=TRUE` are powers of 2, i.e. ..., 0.25, 0.5, 1, 2, 4, 8, 16,....
##'     + with `start_integer=FALSE` are bins that double in size and  start with
##'       `min(x)`; not yet implemented, since have to think about what the width of
##'       the first bin should be.
##'   * numeric value (call it `a`) will result in bin_breaks are separated by `a` and span the
##'       data, that:
##'     + with `start_integer=TRUE` start from `z = floor(min(x))` and are then
##'          `z, z+a, z+2a, z+3a, ....`   (if `z = 0` then power-law cannot be fit
##'        so then need to use `start_integer=FALSE`)
##'     + with `start_integer=FALSE` start from `z = min(x)` and are then
##'           `z, z+a, z+2a, z+3a, ....`
##'   * only `bin_width` or `bin_breaks` can be specified.
##' @param bin_breaks pre-defined bin breaks as a vector. Only `bin_width`
##'   or `bin_breaks` can be specified.
##' @param start_integer TRUE or FALSE, whether to start the bin breaks at an integer
##'   power of 2 (for method `"2k"`) or an integer. See `bin_width` above.
##'   `start_integer` is ignored if `bin_breaks` is specified.
##' @return list containing:
##'   * indiv: dataframe with a row for each `counts_df$x` value, with columns:
##'      + `x`: original `counts_df$x` values
##'      + `bin_mid`, `bin_min`, `bin_max`, `bin_width`: midpoint, minimum,
##'      maximum, and width, respectively, of the bin within
##'      which the `x` value falls.  If indiv has `>=10^6` rows then it isn't
##'      saved. Keeping the name `indiv` as for `binData()`, but these are
##'      individual counts not individual organisms. TODO prob change
##'   * bin_vals: dataframe with a row for each new bin and columns:
##'      + `bin_mid`, `bin_min`, `bin_max`, `bin_width`: midpoint, minimum,
##'         maximum, and width, respectively, of the bin
##'      + `bin_count`: total count of numbers of individuals in that bin
##'      + `bin_count_norm`: normalised bin count, `bin_count / bin_width`
##'      + `bin_sum`: sum of numbers of individuals * x values in that bin (appropriate if `x`
##'         represents biomass, but not length)
##'      + `bin_sum_norm`: `bin_sum / bin_width`
##'      + `log10....` - `log10()` of some of the above quantities
##' @export
##' @examples
##' \dontrun{
##' counts_ex <- tibble::tibble(x = as.numeric(1:50), counts = rep(c(0.19, 27.05, 9, 3.1, 0.001), 10))
##' bin_data(counts_ex, bin_width = 6)
##' }
##' @author Andrew Edwards
bin_data.data.frame <- function(counts_df,
                                bin_width = NULL,
                                bin_breaks = NULL,
                                start_integer = TRUE){
      if(dim(counts_df)[2] != 2)stop("counts_df needs two cols in binData")
      if(min(counts_df$x) < 0) {
        stop("x values in counts_df need to be >= 0 in binData") }
      if(min(counts_df$counts) < 0) {
        stop("numbers in counts_df need to be >= 0 in binData") }
        if(is.null(bin_width) & is.null(bin_breaks)) {
          stop("need one of bin_width or bin_breaks in binData") }
        if(!is.null(bin_width) & !is.null(bin_breaks)) {
          stop("need only one of bin_width or bin_breaks in binData") }
        if(start_integer != "TRUE" & start_integer != "FALSE"){
          stop("start_integer must be TRUE or FALSE in binData") }

      x = counts_df$x                   # need a lot, these are
                                        # measurements
      minx = min(x)  # min(dplyr::pull(counts_df ,1))
      maxx = max(x)  # max(dplyr::pull(counts_df ,1))

        if(!is.null(bin_breaks))
           {
           if(minx < min(bin_breaks) | maxx > max(bin_breaks) )
             { stop("bin_breaks do not span data in binData")
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
                         in quotes) in binData().")
               }
             # start_integer says whether to start from an integer value or
             #  start from min(x),
             z = floor(minx) * start_integer + minx * !start_integer
             bin_breaks = seq( z, by=bin_width,
                        length = ceiling( (maxx - z)/bin_width) + 1)
             }
           }

      indiv = counts_df           # data.frame(x = x)       # dataframe with one
                                  # row for each x value in x
      # x
      indiv$bin_mid =cut(x, breaks=bin_breaks, right=FALSE, include.lowest=TRUE,
                        labels = bin_breaks[-length(bin_breaks)] + 0.5*diff(bin_breaks))
      indiv$bin_min =cut(x, breaks=bin_breaks, right=FALSE, include.lowest=TRUE,
                        labels = bin_breaks[-length(bin_breaks)])
      indiv$bin_max =cut(x, breaks=bin_breaks, right=FALSE, include.lowest=TRUE,
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
                                   bin_count = sum(counts),         # was length(x) for binData
                                   bin_count_norm = bin_count / bin_width,
                                   bin_sum = sum(x * counts),       # only appropriate for body masses
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
                                        matrix(0, nrow=empties, ncol=4)))
        names(emptyVals) = names(bin_vals)
        bin_vals = rbind(bin_vals, emptyVals)         # still a local df

        bin_vals = bin_vals[order(bin_vals$bin_mid), ]   # order by bin_mid

        bin_vals = dplyr::mutate(bin_vals,
                                log10bin_mid = log10(bin_mid),
                                log10bin_count = log10(bin_count),
                                log10bin_sum = log10(bin_sum),
                                log10bin_count_norm = log10(bin_count_norm),
                                # Had thought that last line is needed to avoid
                                # warnings (e.g. simulate-data2.R) and whole
                                # column being NA's. Maybe don't actually use it
                                # in results, but have put it in, may need to
                                # test it more.
                                log10bin_sum_norm = log10(bin_sum_norm))
        bin_vals[is.infinite(bin_vals$log10bin_count),
                "log10bin_count"] = NA
                  # lm can't cope with -Inf, which appear if 0 counts in a bin
        bin_vals[is.infinite(bin_vals$log10bin_count_norm),
                "log10bin_count_norm"] = NA
        bin_vals[is.infinite(bin_vals$log10bin_sum),
                "log10bin_sum"] = NA
        bin_vals[is.infinite(bin_vals$log10bin_sum_norm),
                "log10bin_sum_norm"] = NA
        if(dim(indiv)[1] < 10^6) {       # only save indiv if not too big
          y = list(indiv = indiv, bin_vals = bin_vals)
          } else
          {
          y = list(bin_vals = bin_vals)
          }
        return(y)
    }
