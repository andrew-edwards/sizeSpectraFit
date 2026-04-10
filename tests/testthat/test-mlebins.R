# Test MLEbins related fitting and plotting (better than doing them separately,
# as take a short while to fit).

# Based on med-analysis-4.Rmd, doing Cephalopda (small) Fishing Grounds, which
# should not take too long to fit.

test_that("MLEbins fitting and plotting works and matches original results", {

  dat <- dplyr::filter(mediterranean_data,
                       group == "Cephalopoda",
                       strata == "fg")

  dat_with_breaks <- calc_bin_breaks(dat,
                                     bin_width = 1) %>%
    dplyr::rename(bin_count = number)

  dat_joined <-
    length_bins_to_body_mass_bins(dat_with_breaks,
                                  mediterranean_length_weight_coefficients,
                                  length_data_unit = "mm")

  dat_needed <- mediterranean_for_mlebins(dat_joined) %>%
    dplyr::filter(bin_min < 20)

  # sum(dat_needed$bin_count)
   #   [1] 1085.234   doesn't seem to match figure

  res_cephsmall_fg <- determine_xmin_and_fit_mlebins(dat_needed)


  expect_invisible(plot(res_cephsmall_fg,
                        xlim_hist = c(0, 21),
                        main = "Cephalopoda (small) FG"))

  expect_equal(res_cephsmall_fg$mlebins_fit$b_mle,
               -2.99753213)

  expect_equal(res_cephsmall_fg$mlebins_fit$b_conf,
               c(-3.16269213,
                 -2.83629213))

# Prob want some more based on these to up the coverage.

  ## expect_equal(fit_size_spectrum(sim_vec,
  ##                                x_min = 2)$b_mle,
  ##              -1.976335953)

  ## expect_equal(fit_size_spectrum(sim_vec,
  ##                                x_max = 100)$b_mle,
  ##              -2.02652123)

  ## # For MLEbin, from the fit-data.html vignette:
  ## res_mlebin <- fit_size_spectrum(sim_vec_binned)

  ## sim_vec_binned_misnamed <- sim_vec_binned
  ## names(sim_vec_binned_misnamed) <- c("bish_bash", "bin_max", "bin_count")
  ## expect_error(fit_size_spectrum(sim_vec_binned_misnamed))

  ## expect_equal(res_mlebin$b_mle,
  ##              -2.03502861)

  ## expect_equal(fit_size_spectrum(sim_vec_binned,
  ##                                x_min = 1.5)$b_mle,
  ##              -1.98307875)

  ## expect_equal(fit_size_spectrum(sim_vec_binned,
  ##                                x_max = 200)$b_mle,
  ##              -2.02285725)

  ## expect_error(fit_size_spectrum(sim_vec_binned,
  ##                                x_min = 20,
  ##                                x_max = 15))

  ## # Zero count in first bin, this currently passes but should fail if I fix the
  ## # function (see TODO's in it).
  ## sim_vec_binned_zero <- sim_vec_binned
  ## sim_vec_binned_zero[1, "bin_count"] <- 0
  ## expect_error(fit_size_spectrum(sim_vec_binned_zero))
})
