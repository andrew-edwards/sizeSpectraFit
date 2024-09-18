##' Construct bins that either double in size or are of equal width, and encompass
##'  the data
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
##'   * indiv: dataframe with a row for each `x` value, with columns:
##'      + `x`: original `x` values
##'      + `binMid`, `binMin`, `binMax`, `binWidth`: midpoint, minimum,
##'      maximum, and width, respectively, of the bin within
##'      which the `x` value falls.  If indiv has `>=10^6` rows then it isn't saved.
##'      If `counts` was specified then an equivalent `x`
##'      vector is created and is column `x` (i.e. `x` values are repeated). May
##'      not be the most efficient way, but is easiest to program.
##'   * binVals: dataframe with a row for each new bin and columns:
##'      + `binMid`, `binMin`, `binMax`, `binWidth`: midpoint, minimum,
##'         maximum, and width, respectively, of the bin
##'      + `binCount`: total number of individuals in that bin
##'      + `binCountNorm`: normalised bin count, `binCount / binWidth`
##'      + `binSum`: sum of individual values in that bin (appropriate if `x`
##'         represents biomass, but not length)
##'      + `binSumNorm`: `binSum / binWidth`
##'      + `log10....` - `log10()` of some of the above quantities
##' @export
##' @author Andrew Edwards
binData = function(x = NULL, counts = NULL, binWidth = NULL, binBreaks = NULL,
                   startInteger = TRUE)
    {
        if(!is.null(x) & !is.null(counts)) {
            stop("need only one of x or counts in binData") }
        if(is.null(x) & is.null(counts)) {
            stop("need x or counts in binData") }
        if(!is.null(x)) {
          if(!is.vector(x))stop("x not a vector in binData")
          if(anyNA(x)) stop("x contains NA's in binData")
          if(min(x) <= 0)stop("x needs to be >0 in binData")
          }
        if(!is.null(counts))  {
          if(dim(counts)[2] != 2)stop("counts needs two cols in binData")
          if(min(counts[,1]) < 0) {
              stop("x values in counts need to be >= 0 in binData") }
          if(min(counts[,2]) < 0) {
              stop("numbers in counts need to be >= 0 in binData") }
          if(sum(!is.wholenumber(counts[,2])) > 0) {
              stop("numbers in counts need to be integers in binData;
                      for non-integer count see a new function. Currently,
                      such a new function has no name [so says Jaqen H'ghar]. Or it may be easier to
                      adapt binData.") }
          }
        if(is.null(binWidth) & is.null(binBreaks)) {
            stop("need one of binWidth or binBreaks in binData") }
        if(!is.null(binWidth) & !is.null(binBreaks)) {
            stop("need only one of binWidth or binBreaks in binData") }
        if(startInteger != "TRUE" & startInteger != "FALSE"){
            stop("startInteger must be TRUE or FALSE in binData") }
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

        indiv = data.frame(x)       # dataframe with one row for each individual
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
            binCount = length(x),
            binCountNorm = binCount / binWidth,
            binSum = sum(x),
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
