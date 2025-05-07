##' Extract and format the required Mediterranean data for MLEbins analysis, for
##'  a given strata and group
##'
##' See `zabala-data-analysis.Rmd`. This is specific to this data set, but can
##'   easily be adapted/generalised for others
##' @param dat tibble of data already with certain columns TODO
##' @param strata strata to analyse, if not specified then use all
##' @param group species group to analyse, if not specified then use all
##' @return tibble to go into [fit_size_spectrum()], including the species
##'   column so it uses MLEbins method TODO double check that flow through
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##'
##' }
mediterranean_for_mlebins <- function(dat,
                                      group_name = NULL,
                                      strata_name = NULL,
                                      minimum_length = NULL){
  if(is.null(group_name)){
    group_name <- unique(dat$group)
  }

  if(is.null(strata_name)){
    strata_name <- unique(dat$strata)
  }


  temp <- dplyr::filter(dat,
                        group %in% group_name,
                        strata %in% strata_name)

  if(!is.null(minimum_length)){
    temp <- dplyr::filter(temp,
                          length >= minimum_length)
  }

  temp <- dplyr::select(temp,
                        species,
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
