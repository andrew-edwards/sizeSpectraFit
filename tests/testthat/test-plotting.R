# Test plotting functions with different options, some MLEbin  and MLEbins
#  plotting is in fitting code to save repeating calculations here

test_that("plot.size_spectrum_numeric() works", {
  res_vec <- fit_size_spectrum(sim_vec)

  expect_visible(res_vec)
  expect_invisible(plot(res_vec))
  expect_invisible(plot(res_vec, style = "log_y_axis"))
  expect_invisible(plot(res_vec, style = "linear_y_axis", y_big_ticks = c(10, 57, 99)))
  expect_invisible(plot(res_vec, style = "both_y_axes"))
  expect_invisible(plot(res_vec, style = "biomass"))
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
                        y_big_ticks = c(10, 200, 700)))  # but need to specify
  # small ticks as they're not right -- see help

  expect_invisible(plot(res_vec,
                        style = "linear_y_axis",
                        y_small_ticks_by = 25))
  expect_invisible(plot(res_vec,
                        style = "linear_y_axis",
                        y_small_ticks = c(50, 100, 525, 889),
                        y_small_ticks_labels = c(50, 100, 252, 889)))  # it
  # makes and labels these


  expect_error(plot(res_vec,
                    log = "xy"))

  expect_invisible(plot(res_vec,
                        y_small_ticks = c(126:135),
                        y_big_ticks = c(125, 250, 777),
                        x_big_ticks = NULL,  # not sure covers L98 of
                                             # add_ticks_to_one_axis()
                        x_small_ticks = 10:19))

  expect_invisible(plot(res_vec,
                        style = "linear_y_axis"))
  expect_error(plot(res_vec,
                    log = "xy"))

  expect_invisible(plot(res_vec,
                    y_small_ticks_by = NA,    # no tick marks
                    style = "linear_y_axis"))

  expect_error(plot(res_vec,
                    y_small_ticks_by = 57))   # not allowed for log axis

  expect_invisible(plot(res_vec,
                        x_small_ticks_labels = c(3, 7, 60, 80)))

  expect_invisible({
    plot(1:10, axes = FALSE)
    add_ticks()
    add_minor_tickmarks()       # to test x_tick_end = NULL
  })

})
