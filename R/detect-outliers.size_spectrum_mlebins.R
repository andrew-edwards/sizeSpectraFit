##' @rdname detect-outliers
##' @export
detect_outliers.size_spectrum_mlebins <- function(res){

  dat_orig <- res$dat
  dat_appended <- dplyr::select(dat_orig,
                                species,
                                bin_min,
                                bin_max,
                                bin_count) %>%
    dplyr::mutate(gap = bin_min - lag(bin_min))

  gap_second_largest <- sort(dat_appended$gap,
                             decreasing = TRUE)[2]

  dat_appended <- dplyr::mutate(dat_appended,
                                gap_ratio = gap / gap_second_largest)

  return(dat_appended)
}
