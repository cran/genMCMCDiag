test_that('fitNN works with input of length 1', {
  expect_identical(fitNN(1, '1', eucDist, 0), list(tsSolution = '1',
                                                   tsValues = 1,
                                                   tsDiffs = 0))
})

test_that('fitNN works with input of length 5, ordered and un-ordered labels', {
  uniqueDraws <- c(10, 11, 25, 14, 18)
  uniqueLabels1 <- c('1', '2', '3', '4', '5')

  soln1 <- list(tsSolution = c('1', '2', '4', '5', '3'),
                tsValues = c(10, 11, 14, 18, 25),
                tsDiffs = c(1, 3, 4, 7, 15))
  names(soln1$tsSolution) <- c('1', '2', '4', '5', '3')

  expect_identical(fitNN(uniqueDraws, uniqueLabels1, eucDist, 0), soln1)

  # Unordered
  uniqueLabels2 <- c('11', '22', '2', '40', '14')

  soln2 <- list(tsSolution = c('11', '22', '40', '14', '2'),
                tsValues = c(10, 11, 14, 18, 25),
                tsDiffs = c(1, 3, 4, 7, 15))
  names(soln2$tsSolution) <- c('11', '22', '40', '14', '2')

  expect_identical(fitNN(uniqueDraws, uniqueLabels2, eucDist, 0), soln2)
})

test_that('fitNN cutpoint selection works (tsDiffs ends with max(tsDiffs))', {
  uniqueDraws <- list(c(0,0), c(1,0), c(-5, -5), c(-6, -6), c(-5, -6))
  uniqueLabels <- c('1', '2', '3', '4', '5')

  f <- fitNN(uniqueDraws, uniqueLabels, eucDist, 0)

  expect_equal(f$tsDiffs[5], max(f$tsDiffs))
})
