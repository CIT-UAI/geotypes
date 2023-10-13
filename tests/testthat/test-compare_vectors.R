test_that("throw compare_vectors error", {
  expect_error({
    wrong_columns(c(), c())
  })
})

test_that("compare_vectors any_of right", {
  expect_no_error({
    compare_vectors(c(1, 2), c(1, 2), "any_of")
  })
})

test_that("compare_vectors all_of right", {
  expect_no_error({
    compare_vectors(c(1, 2, 4), c(1, 2), "all_of")
  })
})

test_that("compare_vectors all_of bad", {
  expect_error({
    compare_vectors(c(1), c(1, 2), "all_of")
  })
})

test_that("compare_vectors only_from right", {
  expect_no_error({
    compare_vectors(c(1, 2), c(1, 2, 3), "only_from")
  })
})

test_that("compare_vectors only_from bad", {
  expect_error({
    compare_vectors(c(1, 4), c(1, 2), "only_from")
  })
})

test_that("compare_vectors exclusively right", {
  expect_no_error({
    compare_vectors(c(1, 2, 3), c(1, 2, 3), "exclusively")
  })
})

test_that("compare_vectors exclusively bad", {
  expect_error({
    compare_vectors(c(1, 2, 4), c(1, 2), "exclusively")
  })
})

test_that("compare_vectors_any_of", {
  expect_no_error({
    compare_vectors_any_of(c(), c())
  })
  expect_no_error({
    compare_vectors_any_of(c(1), c())
  })
  expect_no_error({
    compare_vectors_any_of(c(), c(1))
  })
  expect_no_error({
    compare_vectors_any_of(c(1), c(1))
  })
})

test_that("compare_vectors_only_from", {
  expect_no_error({
    compare_vectors_only_from(c(), c())
  })
  expect_no_error({
    compare_vectors_only_from(c(), c(1))
  })
  expect_no_error({
    compare_vectors_only_from(c(1), c(1, 2))
  })
  expect_no_error({
    compare_vectors_only_from(c(1, 2), c(1, 2))
  })
  expect_no_error({
    compare_vectors_only_from(c(), c(1))
  })

  expect_error({
    compare_vectors_only_from(c(2), c())
  })
  expect_error({
    compare_vectors_only_from(c(2), c(1))
  })
  expect_error({
    compare_vectors_only_from(c(1, 2), c(1))
  })
})

test_that("compare_vectors_all_of", {
  expect_no_error({
    compare_vectors_all_of(c(), c())
  })
  expect_no_error({
    compare_vectors_all_of(c(1), c())
  })
  expect_no_error({
    compare_vectors_all_of(c(1), c(1))
  })
  expect_no_error({
    compare_vectors_all_of(c(1, 2), c(1))
  })
  expect_no_error({
    compare_vectors_all_of(c(1, 2), c(1, 2))
  })

  expect_error({
    compare_vectors_all_of(c(), c(1))
  })
  expect_error({
    compare_vectors_all_of(c(2), c(1))
  })
  expect_error({
    compare_vectors_all_of(c(1, 3), c(1, 2))
  })
  expect_error({
    compare_vectors_all_of(c(4, 3), c(1, 2))
  })
})

test_that("compare_vectors_exclusively", {
  expect_no_error({
    compare_vectors_exclusively(c(), c())
  })
  expect_no_error({
    compare_vectors_exclusively(c(1), c(1))
  })
  expect_no_error({
    compare_vectors_exclusively(c(1, 2), c(1, 2))
  })

  expect_error({
    compare_vectors_exclusively(c(), c(1))
  })
  expect_error({
    compare_vectors_exclusively(c(1), c())
  })
  expect_error({
    compare_vectors_exclusively(c(1, 2), c(1))
  })
  expect_error({
    compare_vectors_exclusively(c(1), c(1, 2))
  })
})