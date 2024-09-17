# Test plotting functions with different options

test_that("plot.size_spectrum_numeric() works", {
  res_vec <- fit_size_spectrum(sim_vec)

  expect_invisible(plot(res_vec))
  expect_invisible(plot(res_vec, log = "x"))
  expect_invisible(plot(res_vec, log = "y"))
  expect_invisible(plot(res_vec, log = ""))
  expect_invisible(plot(res_vec,
                        x_small_ticks_labels = c(5, 50, 500,5000),
                        log = "y"))
  expect_invisible(plot(res_vec,
                        x_small_ticks_labels = c(5, 50, 500,5000)))
})
