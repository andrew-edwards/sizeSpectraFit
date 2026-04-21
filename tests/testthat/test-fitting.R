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

  # To estimate xmin from the data
  set.seed(42)
  sim_vec_2 <- c(runif(100, 0.1, 10),
                 rPLB(1000, -2, xmin = 10))     #a few values then a PLB
  res_vec_2 <- determine_xmin_and_fit(sim_vec_2)

  expect_equal(res_vec_2$mle_fit$b_mle,
               -2.084612867)
  expect_invisible(plot(res_vec_2))
  expect_invisible(plot(res_vec_2,
                        xlim_hist = c(7, 100)))
  expect_error(determine_xmin_and_fit("hello"))


