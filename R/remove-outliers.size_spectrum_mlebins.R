##' @rdname remove-outliers
##' @export
remove_outliers.size_spectrum_mlebins <- function(res,
                                                  number = 1){
  dat_orig <- res$dat
  dat_orig_trimmed <- dplyr::select(dat_orig,
                                    species,
                                    bin_min,
                                    bin_max,
                                    bin_count)

  dat_keep <- dat_orig_trimmed[1:(nrow(dat_orig) - number), ]
  dat_removed <- dplyr::setdiff(dat_orig_trimmed,
                                dat_keep)

  bin_count_removed <- sum(dat_removed$bin_count)
  bin_count_removed_prop <- bin_count_removed / sum(dat_orig$bin_count)

  return(list(dat_keep = dat_keep,
              dat_removed = dat_removed,
              bin_count_removed = bin_count_removed,
              bin_count_removed_prop = bin_count_removed_prop))
}
