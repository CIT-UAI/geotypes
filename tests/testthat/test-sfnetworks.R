test_that("sfnetworks sfnetwork Right", {
  expect_no_error({
    sfnetworks_sfnetwork()(
      sfnetworks::roxel %.>%
      sfnetworks::as_sfnetwork(.)
    )
  })
})

test_that("sfnetworks sfnetwork number Bad", {
  expect_error({
    sfnetworks_sfnetwork()(10)
  })
})

test_that("sfnetworks sfnetwork string Bad", {
  expect_error({
    sfnetworks_sfnetwork()("a")
  })
})

test_that("sfnetworks sfnetwork null_ok Right", {
  expect_no_error({
    sfnetworks_sfnetwork(null_ok = TRUE)(NULL)
  })
})

test_that("sfnetworks sfnetwork null_ok Bad", {
  expect_error({
    sfnetworks_sfnetwork(null_ok = FALSE)(NULL)
  })
})
