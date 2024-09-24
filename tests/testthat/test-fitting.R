# Test fitting functions with different options

test_that("fit_size_spectrum() works with different settings and matches previous results", {
  res_vec <- fit_size_spectrum(sim_vec)

  # Taking values from sizeSpectra vignette, for which we then simulation tested
  #  the code.
  expect_equal(res_vec$b_mle,
               -2.029697,
               tolerance = 1e-06)

  expect_equal(res_vec$b_conf,
               c(-2.097417,
                 -1.964317),
               tolerance = 1e-06)
})
