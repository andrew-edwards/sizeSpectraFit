# Test bin_data() and related functions.
# Some expected values are calculated from the code on the date given (so not
#  completely independent), but other fitting tests will test earlier published
#  results from sizeSpectra, so those will somewhat confirm aspects of the code
#  are working. But have not checked every possibility by hand.
test_that("bin_data() works correctly on an example", {
  xx <- c(1:5, 3:7, 7, 7, 7)
  xx_binned <- bin_data(xx, bin_width = 2)
  xx_bin_count_expected <- c(sum(xx >= 1 & xx<3),
                             sum(xx >= 3 & xx<5),
                             sum(xx >= 5 & xx<=7))
  expect_equal(xx_binned$bin_vals$bin_count,
               xx_bin_count_expected)

  sim_vec_binned <- bin_data(sim_vec, bin_width = "2k")
  expect_equal(sim_vec_binned$bin_vals$bin_count,
               c(528, 228, 113, 75, 33, 14, 6, 2, 1)) # Calculated 19/9/24 from
                                                      # the code.

  # Make up a data.frame of counts
  df <- tibble::tibble(x = as.numeric(1:50),
                       counts = rep(c(0.19, 27.05, 9, 3.1, 0.001), 10))
  df_binned  <- bin_data(df,
                         bin_width = 6)
  expect_equal(df_binned$bin_vals$bin_count_norm,
               c(6.5885000,
                 11.0651667,
                 8.0568333,
                 7.0735000,
                 6.5570000,
                 6.5885000,
                 11.0651667,
                 8.0568333,
                 0.5168333))    # Calculated 19/9/24 from the code.

  # Work through the errors to get full code coverage
  expect_error(bin_data.numeric(df, bin_width = "2k"))
  expect_error(bin_data(c(1, 2, 3, NA), bin_width = "2k"))
  expect_error(bin_data(c(-5, 1, 2, 3), bin_width = "2k"))

  expect_error(bin_data(cbind(df, df), bin_width = "2k"))
  df_2 <- df
  df_2[1, "x"] <- -1
  expect_error(bin_data(df_2, bin_width = "2k"))
  df_3 <- df
  df_3[1, "counts"] <- -7
  expect_error(bin_data(df_3, bin_width = "2k"))

  expect_error(bin_data(df))
  expect_error(bin_data(df, bin_width = "2k", bin_breaks = 1:5))
  expect_error(bin_data(df, bin_width = "2k", start_integer = 7))
  expect_error(bin_data(df, bin_breaks = 5:60))
  expect_error(bin_data(df, bin_width = "2k", start_integer = FALSE))
  expect_error(bin_data(df, bin_width = "hello"))
})
