# Test helper functions a() and f()

test_that("a() works on a tibble", {
  expect_equal(tibble::tibble(x = 1:50, y = 51:100) %>% a(),
               as.data.frame(tibble::tibble(x = 1:50, y = 51:100)))
})

test_that("f() works as expected", {
  expect_equal(f(123456),
               "123,456")
})
