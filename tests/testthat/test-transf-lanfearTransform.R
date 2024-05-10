test_that('lanfearTransform returns as expected given numeric chains', {
  testChains <- list(c(.1, .4, -3, .5, -.2),
                     c(-12, -1.4, 3, 2, 1))
  expected <- list(data.frame(val = abs(testChains[[1]]), t = 1:5),
                   data.frame(val = abs(testChains[[2]]), t = 1:5))

  expect_identical(lanfearTransform(testChains, eucDist, reference = 0),
                   expected)
})

test_that('lanfearTransform returns as expected given list chains', {
  testChains <- list(list(c(1,1), c(1,2), c(1,3)),
                     list(c(1,4), c(1,5), c(1,6)))
  expected <- list(data.frame(val = c(0, 1, 2), t = 1:3),
                   data.frame(val = c(3,4,5), t = 1:3))
  expect_identical(lanfearTransform(testChains, hammingDist, reference = c(1,1)),
                   expected)
})
