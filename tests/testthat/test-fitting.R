# Test fitting functions with different options

test_that("fit_size_spectrum() works with different settings and matches previous results", {
  res_vec <- fit_size_spectrum(sim_vec)

  expect_visible(res_vec)    # test print.size_spectrum_numeric

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

  # Trying to force it hitting a bound but setting p way out, and test suppress_warnings
  expect_equal(calc_mle_conf(neg_ll_mle_method,
                             p = -10,
                             vec = seq(-2, -1.9, by = 0.00001),  # -2 should be inside and
                             # force the repeating of the conf calc, L() of function.
                             vec_inc = 0.00001,
                             x = sim_vec,
                             n = length(sim_vec),
                             x_min = min(sim_vec),
                             x_max = max(sim_vec),
                             sum_log_x = sum(log(sim_vec)),
                             suppress_warnings = TRUE),
               list(mle = -2.02969679,
                    conf = c(-2.09742,
                             -1.96431)))  # Not exactly same as different increments


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

  # Likelihood functions
  expect_equal(neg_ll_mle_method(b = -1,               # edge case
                                 x = sim_vec,
                                 n = 1000,
                                 x_min = min(sim_vec),
                                 x_max = max(sim_vec),
                                 sum_log_x = sum(log(sim_vec))),
               2748.5854)
  expect_error(neg_ll_mle_method(b = -2,
                                 x = sim_vec,
                                 n = 1000,
                                 x_min = -1,
                                 x_max = max(sim_vec),
                                 sum_log_x = sum(log(sim_vec))))

  expect_error(neg_ll_mle_method(b = -2,
                                 x = sim_vec,
                                 n = 1000,
                                 x_min = 1,
                                 x_max = 0.5,
                                 sum_log_x = sum(log(sim_vec))))

  # Test p_biomass_bins(), need to contrive some examples
  res_vec_3 <- res_vec
  res_vec_3$b_mle <- -1        # not tweaking conf so results likely meaningless

  expect_equal(p_biomass_bins(res_vec_3)[2, ]$mle_biomass,
               333.991968)

  res_vec_4 <- res_vec
  res_vec_4$b_mle <- -2

  expect_equal(p_biomass_bins(res_vec_4)[2, ]$mle_biomass,
               695.056023)

  res_vec_5 <- res_vec
  res_vec_5$x_min <- -77
  expect_error(p_biomass_bins(res_vec_5))
})
