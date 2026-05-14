# Test MLEbin related fitting and plotting (better than doing them separately,
# to have all on one place).

test_that("MLEbin fitting and plotting works and matches original results", {

  # For MLEbin, from the fit-data.html vignette:
  res_mlebin <- fit_size_spectrum(sim_vec_binned)

  sim_vec_binned_misnamed <- sim_vec_binned
  names(sim_vec_binned_misnamed) <- c("bish_bash", "bin_max", "bin_count")
  expect_error(fit_size_spectrum(sim_vec_binned_misnamed))

  expect_equal(res_mlebin$b_mle,
               -2.03502861)

  expect_equal(fit_size_spectrum(sim_vec_binned,
                                 x_min = 1.5)$b_mle,
               -1.98307875)

  expect_equal(fit_size_spectrum(sim_vec_binned,
                                 x_max = 200)$b_mle,
               -2.02285725)

  expect_error(fit_size_spectrum(sim_vec_binned,
                                 x_min = 20,
                                 x_max = 15))

  expect_error(fit_size_spectrum(sim_vec_binned,
                                 strata = "hello"))
  expect_invisible(plot(res_mlebin))
  expect_invisible(plot(res_mlebin,
                        style = "both_y_axes"))
  expect_invisible(plot(res_mlebin,
                        style = "biomass"))
  expect_invisible(plot(res_mlebin,
                        style = "biomass_and_log"))

  # Zero count in first bin, this currently passes but should fail if I fix the
  # function (see TODO's in it).
  sim_vec_binned_zero <- sim_vec_binned
  sim_vec_binned_zero[1, "bin_count"] <- 0
  expect_error(fit_size_spectrum(sim_vec_binned_zero))



  # MLEbin determine xmin:
  sim_vec_binned_2 <- sim_vec_binned
  sim_vec_binned_2[1, "bin_count"] <- 100  # lower count for first bin

  res_binned_2 <- determine_xmin_and_fit_mlebin(sim_vec_binned_2)

  expect_equal(res_binned_2$mlebin_fit$b_mle,
               -1.98307875)

  expect_invisible(plot(res_binned_2))

  expect_error(determine_xmin_and_fit_mlebin(sim_vec_binned[-4, ]))

  # TODO need some outlier detection here?
})
