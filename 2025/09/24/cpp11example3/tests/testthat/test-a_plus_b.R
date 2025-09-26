test_that("sum works", {
  expect_equal(a_plus_b(1L,3L), 4L)
  expect_equal(a_plus_b(2L,2L), 4L)

  expect_equal(a_plus_b(c(3L,7L,-1L), c(1L,6L)), 4L)
  expect_equal(a_plus_b(c(5L,7L,-1L), c(-1L,6L)), 4L)
})
