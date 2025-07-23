# Bin the sim_vec values.

sim_vec_binned <- bin_data(sim_vec,
                           bin_width = "2k")$bin_vals %>%
                                           dplyr::select(bin_min,
                                                         bin_max,
                                                         bin_count)

usethis::use_data(sim_vec_binned,
                  overwrite = TRUE)
