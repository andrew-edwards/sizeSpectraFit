##' Total and normalised biomass in each bin for a fitted distribution and given
##' bin breaks
##'
##' Takes either a `numeric` vector of bin breaks, or a `tibble` that contains
##' columns `bin_min` and `bin_max` (and possibly more), as well as a value of
##' `b` of the fitted distribution.
##' HERE HERE
##' TODO TODO - actually, input the results list (since we have b and conf ints
##' and data all in there), and then do two different methods dependent upon the
##' class of the results.
##' TODO make .numeric and .data.frame options like for bin_data.
##'
##' @param bin_vals either a `numeric` vector of bin breaks, or a `data.frame` that
##'   contains columns `bin_min` and `bin_max` (and possibly more; e.g. the
##'   format of the `bin_vals` component output from [bin_data()].
##'   Either `p_biomass_bins.numeric()` or `p_biomass_bins.data.frame()` gets called
##'   appropriately.
##' @param b numeric value of `b` of the fitted distribution
##' @param n total number of individuals in the system (needed to calculate biomass)
##' @return tibble; if the input is a `tibble` then columns *** are appended to
##'   the input tibble. If the input is a vector then a `tibble` is created with
##'   columns **
##' TODO: somethin like:
##' then  with each row corresponding to a bin, and columns `wmin`,
##'   `wmax`,  `binWidth`, `estBiomass`, and `estBiomassNorm`. If the input is a tibble with
##'   columns `wmin` and `wmax` (or `binMin` and `binMax`), then
##'   columns `estBiomass` and `estBiomassNorm` are appended to the input tibble.
##'
##' @export
##' @author Andrew Edwards
##' @examples
##' @donttest{
##' # TODO
##' binBreaks = c(1, 10, 20, 50, 100)
##' pBiomassBins(binBreaks = binBreaks) # uses default pBiomass() values
##'
##' # Same data as a tibble:
##' testTibble <- dplyr::tibble(binMin = binBreaks[-length(binBreaks)],
##'                             binMax = binBreaks[-1])
##' pBiomassBins(binValsTibble = testTibble)
##' @}
##'
##' TODO when both methods working:
##' p_biomass_bins <- function(dat,
##'                     ...){
##'  UseMethod("p_biomass_bins")
##'}
##' TODO numeric version could create a data.frame first like bin_data.numeric does,
##'   then use p_biomass_bins.data.frame. Something like:
##'   ifelse(!is.null(binValsTibble),
##'         binTibble <- binValsTibble,
##'      # Create tibble from the vector binBreaks:
##'         binTibble <- dplyr::tibble(wmin = binBreaks[-length(binBreaks)],
##'                                    wmax = binBreaks[-1])
##'         )
p_biomass_bins.data.frame <- function(bin_vals,
                                      b,   # HERE - maybe use the results
                                        # instead, then have n caclulatable and
                                        # xmin and xmax. Could include n in
                                        # outputs from MLE calculations.
                                      n,
                                      ...){
# from sizeSpectra, this did both ways, had to include the ... at the beginning,
# I think because of `b` issue; might avoid here
pBiomassBins <- function(...,
                         binValsTibble = NULL,
                         binBreaks = NULL,
                         xmin = NULL,
                         xmax = NULL,
                         n){

  HERE

  # Should really insist on one format of input tibble; trying to be flexible
  # and back compatible
  if("wmin" %in% names(binTibble)){
    if(is.null(xmin)){
      xmin <- min(binTibble$wmin)
    }
    if(is.null(xmax)){
      xmax <- max(binTibble$wmax)
    }
  } else {
    if(is.null(xmin)){
      xmin <- min(binTibble$binMin)
    }
    if(is.null(xmax)){
      xmax <- max(binTibble$binMax)
    }
  }

  if("wmin" %in% names(binTibble)){
    binTibble <- dplyr::mutate(binTibble,
                               binWidth = wmax - wmin,
                               estBiomass = pBiomass(x = binTibble$wmax,
                                                     xmin = xmin,
                                                     xmax = xmax,
                                                     n = n,
                                                     ...) -
                                 pBiomass(x = binTibble$wmin,
                                          xmin = xmin,
                                          xmax = xmax,
                                          n = n,
                                          ...),
                               estBiomassNorm = estBiomass / binWidth)
  } else {
    binTibble <- dplyr::mutate(binTibble,
                               binWidth = binMax - binMin,
                               estBiomass = pBiomass(x = binTibble$binMax,
                                                     xmin = xmin,
                                                     xmax = xmax,
                                                     n = n,
                                                     ...) -
                                 pBiomass(x = binTibble$binMin,
                                          xmin = xmin,
                                          xmax = xmax,
                                          n = n,
                                          ...),
                               estBiomassNorm = estBiomass / binWidth)
  }
  return(binTibble)
}

##' Wrapper to call `pBiomassBins()` for three values of b (MLE and conf limits)
##'
##' Only takes a tibble as the input data (unlike `pBiomassBins()` and
##' `pBiomass()`. Currently needs `wmin`, `wmax` and `Number` as columns.
##'
##' @param ... extra arguments passed to `bBiomassBins()`: `b`, `xmin`, `xmax, `n`.
##' @param binValsTibble tibble of binned data in the form required for `pBiomassBins()`
##' @param b.MLE maximum likelihood estimate of *b* (ideally from the MLEbin method)
##' @param b.confMin lower 95\% confidence limits of *b*
##' @param b.confMax upper 95\% confidence limits of *b*
##' @return `binValsTibble` with extra columns `estBiomassMLE` and
##'   `estBiomassNormMLE` for the estimated biomass and normalised biomass for
##'   `b.MLE`, extra columns `estBiomassConfMin` and `estBiomassNormConfMin` for
##'   the same but using `b.confMin`, `estBiomassConfMax` and
##'   `estBiomassNormConfMax` for `b.confMax`.
##' @export
##' @author Andrew Edwards
##' @examples
##' @donttest{
##' # see `MLEbin_recommend` vignette
##' @}
pBiomassBinsConfs <- function(...,
                              binValsTibble,
                              b.MLE,
                              b.confMin,
                              b.confMax){

  # MLE value
  binTibbleConfs <- pBiomassBins(binValsTibble = binValsTibble,
                                 b = b.MLE,
                                 n = sum(binValsTibble$Number),
                                 ...) %>%
    dplyr::rename(estBiomassMLE = estBiomass,
                  estBiomassNormMLE = estBiomassNorm)

  # Minimum of confidence interval for b
  binTibbleConfs <- pBiomassBins(binValsTibble = binTibbleConfs,
#                                 xmin = min(binTibbleConfs$wmin),
#                                 xmax = max(binTibbleConfs$wmax),
                                 b = b.confMin,
                                 n = sum(binTibbleConfs$Number),
                                 ...) %>%
    dplyr::rename(estBiomassConfMin = estBiomass,
                  estBiomassNormConfMin = estBiomassNorm)

  # Maximum of confidence interval for b
  binTibbleConfs <- pBiomassBins(binValsTibble = binTibbleConfs,
#                                 xmin = min(binTibbleConfs$wmin),
#                                 xmax = max(binTibbleConfs$wmax),
                                 b = b.confMax,
                                 n = sum(binTibbleConfs$Number),
                                 ...) %>%
    dplyr::rename(estBiomassConfMax = estBiomass,
                  estBiomassNormConfMax = estBiomassNorm)

  invisible(binTibbleConfs)
}
