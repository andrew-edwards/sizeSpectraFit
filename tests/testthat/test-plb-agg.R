# Test dPLB_agg() etc.with different options

test_that("dPLB_agg() etc. functions work with different settings", {

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
               c(0.2414386, 0.4839062, 0.7351509, 0.8657657, 1.0000000))


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
})


