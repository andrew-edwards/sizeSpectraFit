# Test dPLB_agg() etc. and plotting with different options

test_that("dPLB_agg() and plotting functions work with different settings", {

  x <- 1:2000

  # dPLB_agg
  expect_error(dPLB_agg(x,
                        b_vec = c(-1, -2, -3, -4),
                        n_vec = c(6000, 6000, 1600),  # left one out
                        xmin_vec = c(0.3, 10, 100, 500),
                        xmax_vec = c(80, 800, 1000, 1500)))

  y <- dPLB_agg(x,
                b_vec = c(-1, -2, -3, -4),
                n_vec = c(6000, 6000, 1600, 2000),
                xmin_vec = c(0.3, 10, 100, 500),
                xmax_vec = c(80, 800, 1000, 1500))

  # Pick out some values to confirm (albeit calculated from the function):
  expect_equal(y[c(10, 20, 100, 500, 1900)],
               c(0.0458337393, 0.0131797713, 0.0024614860,
                 0.0008309719, 0.0000000000))


  # pPLB_agg
  expect_error(pPLB_agg(x,
                        b_vec = c(-1, -2, -3, -4),
                        n_vec = c(6000, 6000, 1600),  # left one out
                        xmin_vec = c(0.3, 10, 100, 500),
                        xmax_vec = c(80, 800, 1000, 1500)))

  p <- pPLB_agg(x,
                b_vec = c(-1, -2, -3, -4),
                n_vec = c(6000, 6000, 1600, 2000),
                xmin_vec = c(0.3, 10, 100, 500),
                xmax_vec = c(80, 800, 1000, 1500))

  # Pick out some values to confirm:
  expect_equal(p[c(10, 20, 100, 500, 1900)],
               c(0.241438641, 0.483906189, 0.735150925, 0.865765739, 1.000000000))

  # Should match when just have one PLB to aggregate:
  expect_equal(dPLB_agg(x,
                        b_vec = -1.5,
                        n_vec = 6000,
                        xmin_vec = 0.3,
                        xmax_vec = 80),
               dPLB(x,
                    b = -1.5,
                    xmin = 0.3,
                    xmax = 80))

  # Examples for plotting, taken from fit-aggregated.Rmd vignette (smaller
  # sample sizes take longer, presumably to do with conf intervals, so using the same)
  set.seed(42)
  S <- 4

  # For each sample, prescribe the sample size, exponent b, xmin and xmax.
  n_vec <- c(6000, 6000, 1600, 2000)
  b_vec_known <- c(-1.09, -2, -3, -4)
  xmin_known <- c(0.3, 10, 100, 300)
  xmax_known <- c(80, 4000, 1000, 1500)

  res_list <- list()                      # To save results
  res_mlebin_list <- list()                      # To save results

  for(s in 1:S){
    x_values <- rPLB(n_vec[s],
                     b = b_vec_known[s],
                     xmin = xmin_known[s],
                     xmax = xmax_known[s])

    res_list[[s]] <- fit_size_spectrum(x_values)    # x_values get included in
    # MLE_res[[s]]

    # Binned values for later
    x_values_binned <- bin_data(x_values,
                                bin_width = 20)$bin_vals

    # Without this x_min would get set to zero, as detailed in error message in
    #  fit_size_spectrum_mlebin
    if(min(x_values_binned$bin_min) == 0){
      x_values_binned[which(x_values_binned$bin_min == 0), "bin_min"] <- 0.00001
    }

    res_mlebin_list[[s]] <- fit_size_spectrum(x_values_binned)
                 # binned data get included in res_mlebin_list[[s]]
  }

  expect_invisible(orig_agg_fit <- plot_aggregate(res_list))
  expect_equal(orig_agg_fit$x_plb_agg[15],
               0.342922352)

  # Remove some individuals, repeat, to use plot_aggregate_fits()
  res_fished_list_2 <- list()
  set.seed(42)
  for(s in 1:S){
    indiv_sizes <- sort(res_list[[s]]$x)

    # Remove 50% of any individuals >= 100 g
    num_sizes <- length(indiv_sizes)
    indiv_sizes_over_100 <- indiv_sizes[indiv_sizes >= 100]

    x_values <- c(indiv_sizes[indiv_sizes < 100],
                  sample(indiv_sizes_over_100,
                         size = floor(0.5 * length(indiv_sizes_over_100))))
    res_fished_list_2[[s]] <- fit_size_spectrum(x_values)
  }

  expect_invisible(fished_agg_fit_2 <- plot_aggregate(res_fished_list_2))

  agg_list_2 <- list(orig_agg_fit,
                     fished_agg_fit_2)
  strata <- c("unfished",
              "fished")
  expect_invisible(plot_aggregate_fits(agg_list_2,
                                       strata_names = strata,
                                       ylim = c(10^(-4), 1)))

  # MLEbin plots
  expect_invisible(mlebin_agg_fit <- plot_aggregate_mlebin(res_mlebin_list))
  expect_invisible(mlebin_agg_fit <- plot_aggregate_mlebin(res_mlebin_list,
                                       rect_shading_equal_col_vec= TRUE,
                                       rect_border_equal_col_vec = FALSE))

  expect_error(plot_aggregate_mlebin(res_mlebin_list,
                                     col_vec = "pink"))
  expect_error(plot_aggregate_mlebin(1:10))
  expect_error(plot_aggregate_mlebin(list(a = 3)))
})


