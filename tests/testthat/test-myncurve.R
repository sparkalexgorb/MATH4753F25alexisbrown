test_that("mean is correct", {
  mu=10
  sigma=5
  a=6
  expect_equal(mu, 10)
})

test_that("standard deviation is correct", {
  mu=10
  sigma=5
  a=6
  expect_equal(sigma, 5)
})

test_that("a is less than to mean plus 3 standard deviations", {
  mu=10
  sigma=5
  a=6
  expect_lt(a, mu + 3*sigma)
})
