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

  expect_equal(nrow(mediterranean_for_mlebins(dat_joined, minimum_length =
                                                            100)),
               48)

  # sum(dat_needed$bin_count)
   #   [1] 1085.234   doesn't seem to match figure

  res_cephsmall_fg <- determine_xmin_and_fit_mlebins(dat_needed)  # seems to
  # take a few minutes, surprisingly since small dataset

  expect_equal(fit_size_spectrum_mlebins(dat_needed,
                                         x_min = NULL,
                                         x_max = 40)$b_mle,
               -2.53006084)


  expect_error(fit_size_spectrum_mlebins(dat_needed,
                                         x_min = 20,
                                         x_max = 10))

  expect_invisible(plot(res_cephsmall_fg))

  expect_invisible(plot(res_cephsmall_fg,
                        xlim_hist = c(0, 21),
                        main = "Cephalopoda (small) FG"))

  expect_invisible(plot(res_cephsmall_fg$mlebins_fit))

  expect_equal(neg_ll_mlebins_method(b = -1,
                                     x_min = 1,  # made up numbers just for b=-1
                                     x_max = 10,
                                     dat_needed, 1000),
               4222.39879)

  expect_equal(res_cephsmall_fg$mlebins_fit$b_mle,
               -2.99753213)

  expect_equal(res_cephsmall_fg$mlebins_fit$b_conf,
               c(-3.16269213,
                 -2.83629213))

  # Testing detect_outliers and remove_outliers. You detect first then look at
  # results to judge what to remove (it's not automatic, you assing
  # remove_outliers(number = **) value.
  # We didn't remove outliers for cephsmall_fg, so this is not replicating our
  # actual analysis, just an example to test the code.

  res_detect <- detect_outliers(res_cephsmall_fg)   # gave error when
  # printing, if not done library(  dplyr or tibble?), see note in READpbs..

  # plot(res_detect$gap_ratio)  # shows wouldn't remove, but let's do 2
  # to just test the results. You give it the number, it's not automatic based
  # on gap_ratio.

  res_remove <- remove_outliers(res_cephsmall_fg,
                                number = 2)

  expect_equal(res_remove$bin_count_removed,
               14.463)

})
