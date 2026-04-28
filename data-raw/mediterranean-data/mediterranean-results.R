# Going to save the MLEbins results for cephalopoda (small) fishing grounds, to
# show on main README without rerunning each time (takes a few minutes to run)

load_all()

dat_filtered <- dplyr::filter(mediterranean_data,
                              group == "Cephalopoda",
                              strata == "fg")

dat_with_breaks <- calc_bin_breaks(dat_filtered,
                                   bin_width = 1) %>%
  dplyr::rename(bin_count = number)

dat_joined <-
  length_bins_to_body_mass_bins(dat_with_breaks,
                                mediterranean_length_weight_coefficients,
                                length_data_unit = "mm")

data_cephsmall_fg <- mediterranean_for_mlebins(dat_joined) %>%
  dplyr::filter(bin_min < 20) %>%
  dplyr::arrange(bin_min)                           # To show the overlapping bins for
                             # different species (else just ordered by species)
                                             # Could use these arguments, but
                                             #  already filtered above:
                                             # group_name = "Cephalopoda",
                                             # strata_name = "fg")

res_cephsmall_fg <- determine_xmin_and_fit_mlebins(data_cephsmall_fg)  # takes a few minutes

plot(res_cephsmall_fg)

# Then plot in main README.Rmd

usethis::use_data(data_cephsmall_fg,
                  overwrite = TRUE)

usethis::use_data(res_cephsmall_fg,
                  overwrite = TRUE)
