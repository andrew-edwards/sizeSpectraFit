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
##'   non-integer values, unlike for `binData()`.
##' @param binWidth type of bins to use:
##'   * `"2k"` will result in `binBreaks` that:
##'     + with `startInteger=TRUE` are powers of 2, i.e. ..., 0.25, 0.5, 1, 2, 4, 8, 16,....
##'     + with `startInteger=FALSE` are bins that double in size and  start with
##'       `min(x)`; not yet implemented, since have to think about what the width of
##'       the first bin should be.
##'   * numeric value (call it `a`) will result in binBreaks are separated by `a` and span the
##'       data, that:
##'     + with `startInteger=TRUE` start from `z = floor(min(x))` and are then
##'          `z, z+a, z+2a, z+3a, ....`   (if `z = 0` then power-law cannot be fit
##'        so then need to use `startInteger=FALSE`)
##'     + with `startInteger=FALSE` start from `z = min(x)` and are then
##'           `z, z+a, z+2a, z+3a, ....`
##'   * only `binWidth` or `binBreaks` can be specified.
##' @param binBreaks pre-defined bin breaks as a vector. Only `binWidth`
##'   or `binBreaks` can be specified.
##' @param startInteger TRUE or FALSE, whether to start the bin breaks at an integer
##'   power of 2 (for method `"2k"`) or an integer. See `binWidth` above.
##'   `startInteger` is ignored if `binBreaks` is specified.
##' @return list containing:
##'   * indiv: dataframe with a row for each `counts_df$x` value, with columns:
##'      + `x`: original `counts_df$x` values
##'      + `binMid`, `binMin`, `binMax`, `binWidth`: midpoint, minimum,
##'      maximum, and width, respectively, of the bin within
##'      which the `x` value falls.  If indiv has `>=10^6` rows then it isn't
##'      saved. Keeping the name `indiv` as for `binData()`, but these are
##'      individual counts not individual organisms.
##'   * binVals: dataframe with a row for each new bin and columns:
##'      + `binMid`, `binMin`, `binMax`, `binWidth`: midpoint, minimum,
##'         maximum, and width, respectively, of the bin
##'      + `binCount`: total count of numbers of individuals in that bin
##'      + `binCountNorm`: normalised bin count, `binCount / binWidth`
##'      + `binSum`: sum of numbers of individuals * x values in that bin (appropriate if `x`
##'         represents biomass, but not length)
##'      + `binSumNorm`: `binSum / binWidth`
##'      + `log10....` - `log10()` of some of the above quantities
##' @export
##' @examples
##' \dontrun{
##' counts_ex <- tibble::tibble(x = as.numeric(1:50), counts = rep(c(0.19, 27.05, 9, 3.1, 0.001), 10))
##' bin_data(counts_ex, binWidth = 6)
##' }
##' @author Andrew Edwards
bin_data = function(counts_df,
                    binWidth = NULL,
                    binBreaks = NULL,
                    startInteger = TRUE)
    {
      if(dim(counts_df)[2] != 2)stop("counts_df needs two cols in binData")
      if(min(counts_df$x) < 0) {
        stop("x values in counts_df need to be >= 0 in binData") }
      if(min(counts_df$counts) < 0) {
        stop("numbers in counts_df need to be >= 0 in binData") }
        if(is.null(binWidth) & is.null(binBreaks)) {
          stop("need one of binWidth or binBreaks in binData") }
        if(!is.null(binWidth) & !is.null(binBreaks)) {
          stop("need only one of binWidth or binBreaks in binData") }
        if(startInteger != "TRUE" & startInteger != "FALSE"){
          stop("startInteger must be TRUE or FALSE in binData") }

      x = counts_df$x                   # need a lot, these are
                                        # measurements
      minx = min(x)  # min(dplyr::pull(counts_df ,1))
      maxx = max(x)  # max(dplyr::pull(counts_df ,1))

        if(!is.null(binBreaks))
           {
           if(minx < min(binBreaks) | maxx > max(binBreaks) )
             { stop("binBreaks do not span data in binData")
             }
           } else           # create binBreaks based on binWidth
           {
           if(binWidth == "2k")
             {
             if(startInteger)
               { binBreaks = 2^( floor(log2(minx)) : ceiling(log2(maxx)) )
               } else
               { stop("startInteger currently needs to be TRUE when
                   binWidth = 2k")
               }
             } else     # If not "2k"
             {
             if(!is.numeric(binWidth))
               { stop("binWidth must be 2k or a number (in quotes is okay
                         in quotes) in binData().")
               }
             # startInteger says whether to start from an integer value or
             #  start from min(x),
             z = floor(minx) * startInteger + minx * !startInteger
             binBreaks = seq( z, by=binWidth,
                        length = ceiling( (maxx - z)/binWidth) + 1)
             }
           }

      indiv = counts_df           # data.frame(x = x)       # dataframe with one
                                  # row for each x value in x
      # x
      indiv$binMid =cut(x, breaks=binBreaks, right=FALSE, include.lowest=TRUE,
                        labels = binBreaks[-length(binBreaks)] + 0.5*diff(binBreaks))
      indiv$binMin =cut(x, breaks=binBreaks, right=FALSE, include.lowest=TRUE,
                        labels = binBreaks[-length(binBreaks)])
      indiv$binMax =cut(x, breaks=binBreaks, right=FALSE, include.lowest=TRUE,
                        labels = binBreaks[-1])
      #
      indiv$binMid = as.numeric(as.character(indiv$binMid))
      indiv$binMin = as.numeric(as.character(indiv$binMin))
      indiv$binMax = as.numeric(as.character(indiv$binMax))
      # Now calculate biomass in each bin class:

        binVals = dplyr::summarise(dplyr::group_by(indiv, binMid),
                                   binMin = unique(binMin),
                                   binMax = unique(binMax),
                                   binWidth = binMax - binMin,
                                   binCount = sum(counts),         # was length(x) for binData
                                   binCountNorm = binCount / binWidth,
                                   binSum = sum(x * counts),       # only appropriate for body masses
                                   binSumNorm = binSum / binWidth )
      # binWidth uses new columns binMax and binMin
      # Indices for minima of bins that have zero counts and so do not
      #  appear in binVals yet:
        emptyBinMinInd = !(signif(binBreaks[-length(binBreaks)], digits = 8) %in%
                           signif(binVals$binMin, digits = 8))
                         # to avoid not-real differences due to rounding/storing
        emptyBinMin = binBreaks[emptyBinMinInd]
        empties = length(emptyBinMin)
        emptyBinMax = binBreaks[-1][emptyBinMinInd]
        emptyBinWidth = emptyBinMax - emptyBinMin
        emptyBinMid = emptyBinMin + emptyBinWidth/2

        emptyVals = as.data.frame(cbind(emptyBinMid,
                                        emptyBinMin,
                                        emptyBinMax,
                                        emptyBinWidth,
                                        matrix(0, nrow=empties, ncol=4)))
        names(emptyVals) = names(binVals)
        binVals = rbind(binVals, emptyVals)         # still a local df

        binVals = binVals[order(binVals$binMid), ]   # order by binMid

        binVals = dplyr::mutate(binVals,
                                log10binMid = log10(binMid),
                                log10binCount = log10(binCount),
                                log10binSum = log10(binSum),
                                log10binCountNorm = log10(binCountNorm),
                                # Had thought that last line is needed to avoid
                                # warnings (e.g. simulate-data2.R) and whole
                                # column being NA's. Maybe don't actually use it
                                # in results, but have put it in, may need to
                                # test it more.
                                log10binSumNorm = log10(binSumNorm))
        binVals[is.infinite(binVals$log10binCount),
                "log10binCount"] = NA
                  # lm can't cope with -Inf, which appear if 0 counts in a bin
        binVals[is.infinite(binVals$log10binCountNorm),
                "log10binCountNorm"] = NA
        binVals[is.infinite(binVals$log10binSum),
                "log10binSum"] = NA
        binVals[is.infinite(binVals$log10binSumNorm),
                "log10binSumNorm"] = NA
        if(dim(indiv)[1] < 10^6) {       # only save indiv if not too big
          y = list(indiv = indiv, binVals = binVals)
          } else
          {
          y = list(binVals = binVals)
          }
        return(y)
    }
