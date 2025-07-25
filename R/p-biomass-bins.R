##' Total and normalised biomass in each bin for a fitted distribution and given
##' bin breaks
##'
##'
##' For size_spectrum_numeric we know all the individual body sizes, so there is
##' no uncertainty in the biomass within each bin. So setting high and low
##' biomasses to be the same.
##'
##' HERE HERE
##' TODO TODO - actually, input the results list (since we have b and conf ints
##' and data all in there), and then do two different methods dependent upon the
##' class of the results.
##'
##' @param bin_vals either a `numeric` vector of bin breaks, or a `data.frame` that
##'   contains columns `bin_min` and `bin_max` (and possibly more; e.g. the
##'   format of the `bin_vals` component output from [bin_data()].
##'   Either `p_biomass_bins.numeric()` or `p_biomass_bins.data.frame()` gets called
##'   appropriately.
##' @param res results list, of either class `size_spectrum_numeric` or
##'   `size_spectrum_mlebin`.
##' @param n total number of individuals in the system (needed to calculate
##'   biomass), TODO change when resolve #5 as n will be in results.
##' @return tibble like that returned in `$data` object from
##'   [fit_size_spectrum.data.frame()]  (TODO if just size_spectrum_numeric
##'   entered then columns are a bit less), with extra columns appended here
##'   giving, for each bin:
##'   * median: median of the data
##'   * eti_lower: lower end of the ETI
##'
##'
##' TODO: somethin like:
##' then  with each row corresponding to a bin, and columns `wmin`,
##'   `wmax`,  `binWidth`, `estBiomass`, and `estBiomassNorm`. If the input is a tibble with
##'   columns `wmin` and `wmax` (or `binMin` and `binMax`), then
##'   columns `estBiomass` and `estBiomassNorm` are appended to the input tibble.
##'
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' # TODO
##' binBreaks = c(1, 10, 20, 50, 100)
##' pBiomassBins(binBreaks = binBreaks) # uses default pBiomass() values
##'
##' # Same data as a tibble:
##' testTibble <- dplyr::tibble(binMin = binBreaks[-length(binBreaks)],
##'                             binMax = binBreaks[-1])
##' pBiomassBins(binValsTibble = testTibble)
##' }
##'
p_biomass_bins <- function(res){
  UseMethod("p_biomass_bins")
}


##"can delete this once used for both methods
##' TODO numeric version could create a data.frame first like bin_data.numeric does,
##'   then use p_biomass_bins.data.frame. Something like:
##'   ifelse(!is.null(binValsTibble),
##'         binTibble <- binValsTibble,
##'      # Create tibble from the vector binBreaks:
##'         binTibble <- dplyr::tibble(wmin = binBreaks[-length(binBreaks)],
##'                                    wmax = binBreaks[-1])
##'         )
