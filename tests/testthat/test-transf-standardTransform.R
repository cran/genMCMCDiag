test_that('standard transform returns as expected', {
  tstObj <- list(1:5,
                 1:5,
                 2:6)

  expectObj <- list(data.frame('val' = 1:5, 't' = 1:5),
                    data.frame('val' = 1:5, 't' = 1:5),
                    data.frame('val' = 2:6, 't' = 1:5))

  expect_identical(standardTransform(tstObj), expectObj)
})

test_that('standard transform throws appropriate error when given a list of lists', {
  testObject <- list(list(1:3), list(2:4))
  expect_error(standardTransform(testObject), regexp = 'standard transformation')
})
