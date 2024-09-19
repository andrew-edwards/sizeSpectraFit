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



})
