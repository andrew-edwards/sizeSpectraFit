# Test helper function a()

test_that("a() works on a tibble", {
  expect_equal(tibble::tibble(x = 1:50, y = 51:100) %>% a(),
               as.data.frame(tibble::tibble(x = 1:50, y = 51:100)))


})
