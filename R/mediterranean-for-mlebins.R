##' Extract and format the required Mediterranean data for MLEbins analysis, for
##'  a given strata and group
##'
##' See `zabala-data-analysis.Rmd`. This is specific to this data set, but can
##'   easily be adapted/generalised for others
##' @param dat tibble of data already with certain columns TODO
##' @param strata strata to analyse
##' @param group species group to analyse
##' @return tibble to go into [fit_size_spectrum()], including the species
##'   column so it uses MLEbins method TODO double check that flow through
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##'
##' }
mediterranean_for_mlebins <- function(dat,
                                      group_name,
                                      strata_name){
# Could write a function for this, probably worth it:
  temp <- dplyr::filter(dat,
                        group == group_name,
                        strata == strata_name) %>%
    dplyr::select(species,
                  bin_count = number,
                  bin_min = weight_bin_min,
                  bin_max = weight_bin_max)

  res <- dplyr::summarise(dplyr::group_by(temp,
                                          species,
                                          bin_min,
                                          bin_max),    # need to retain, not needed
                                                       # for grouping
                          bin_count = sum(bin_count)) %>%
    dplyr::ungroup()

  return(res)
}
