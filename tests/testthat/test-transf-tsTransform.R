test_that('tsTransform returns as expected given a list of numerics', {
  tstObject <- list(c(1, 4, 3),
                    c(5, 8, 7))

  expected <- list(data.frame(val = c(0, 3, 2), t = 1:3),
                   data.frame(val = c(4, 7, 6), t = 1:3))
  names(expected) <- c('1', '2')

  expect_identical(tsTransform(tstObject, eucDist, verbose = FALSE), expected)
})

test_that('tsTransform returns as expected given a list of lists', {
  tstObject <- list(list(c(1,2), c(-1,8)),
                    list(c(1,4), c(-1, 4)))

  expected <- list(data.frame(val = c(0, 8), t = 1:2),
                   data.frame(val = c(2,4), t = 1:2))
  names(expected) <- c('1','2')

  expect_identical(tsTransform(tstObject, hammingDist, verbose = FALSE), expected)
})
