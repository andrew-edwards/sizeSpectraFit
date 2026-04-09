# Test fitting functions with different options

test_that("fit_size_spectrum() works with different settings and matches previous results", {
  res_vec <- fit_size_spectrum(sim_vec)

  # Taking values from sizeSpectra vignette, for which we then simulation tested
  #  the code.
  expect_equal(res_vec$b_mle,
               -2.02969679)

  expect_equal(res_vec$b_conf,
               c(-2.09741679,
                 -1.96431679))

  expect_equal(fit_size_spectrum(sim_vec,
                                 x_min = 2)$b_mle,
               -1.976335953)

  expect_equal(fit_size_spectrum(sim_vec,
                                 x_max = 100)$b_mle,
               -2.02652123)

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

  # Zero count in first bin, this currently passes but should fail if I fix the
  # function (see TODO's in it).
  sim_vec_binned_zero <- sim_vec_binned
  sim_vec_binned_zero[1, "bin_count"] <- 0
  expect_error(fit_size_spectrum(sim_vec_binned_zero))
})
