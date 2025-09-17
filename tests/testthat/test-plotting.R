# Test plotting functions with different options

test_that("plot.size_spectrum_numeric() works", {
  res_vec <- fit_size_spectrum(sim_vec)

  expect_invisible(plot(res_vec))
  expect_invisible(plot(res_vec, style = "log_y_axis"))
  expect_invisible(plot(res_vec, style = "linear_y_axis"))
  expect_invisible(plot(res_vec, style = "both_y_axes"))
  expect_invisible(plot(res_vec, style = "biomass_and_isd"))
  expect_invisible(plot(res_vec,
                        x_small_ticks_labels = c(5, 50, 500, 5000)))
  expect_invisible(plot(res_vec,
                        x_small_ticks_labels = c(5, 50, 500, 5000),
                        style = "log_y_axis"))
  expect_invisible(plot(res_vec,
                        style = "log_y_axis",
                        x_big_ticks = c(5, 50, 500, 5000)))
  expect_invisible(plot(res_vec,
                        style = "log_y_axis",
                        x_small_ticks = c(5, 50, 500)))
  expect_invisible(plot(res_vec,
                        style = "log_y_axis",
                        x_small_ticks_by = 25))
  expect_invisible(plot(res_vec,
                        style = "log_y_axis",
                        x_small_ticks_by = NA))
  expect_error(plot(res_vec,
                    log = "xy"))

})
