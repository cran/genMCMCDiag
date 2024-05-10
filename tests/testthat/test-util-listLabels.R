test_that('listLabels produces unique labels as expected', {
  expect_identical(listLabels(c(1,2,3,2,4,2,1)),
                   c('1', '2', '3', '2', '4', '2', '1'))
})

test_that('listLabels produces unique labels ordered by the original', {
  expect_identical(listLabels(c(4,3,1,2)),
                   c('1', '2', '3', '4'))
})

test_that('listLabels works on list of matrices', {
  m1 <- matrix(0, nrow = 2, ncol = 2)
  m2 <- matrix(c(1,0,0,0), nrow = 2, ncol = 2)
  m3 <- matrix(c(1,1,0,0), nrow = 2, ncol = 2)

  expect_identical(listLabels(list(m1, m2, m1, m3, m3)),
                   c('1', '2', '1', '3', '3'))
})

test_that('listLabels works on list of lists', {
  l1 <- list(A = c(1,2), B = c('1', '2'))
  l2 <- list(A = c(1,1), B = c('1', '2'))
  l3 <- list(A = c(1,2), B = c('1', '1'))

  expect_identical(listLabels(list(l1, l2, l1, l3, l3)),
                   c('1', '2', '1', '3', '3'))
})
