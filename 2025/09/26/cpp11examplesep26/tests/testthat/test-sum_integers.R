test_that("sum_integers works", {
  expect_equal(sum_integers(c(1L,2L), c(2L,3L)), 3L)
  expect_error(sum_integers(c(1,2), c(2,3)))
})
