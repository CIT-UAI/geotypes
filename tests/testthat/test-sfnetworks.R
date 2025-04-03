network <-
  sfnetworks::roxel %.>%
  sfnetworks::as_sfnetwork(.)

test_that("sfnetworks sfnetwork Right", {
  expect_equal(
    sfnetworks_sfnetwork()(network),
    network
  )
})

test_that("sfnetworks sfnetwork number Bad", {
  expect_snapshot(error = TRUE, {
    sfnetworks_sfnetwork()(10)
  })
})

test_that("sfnetworks sfnetwork string Bad", {
  expect_snapshot(error = TRUE, {
    sfnetworks_sfnetwork()("a")
  })
})

test_that("sfnetworks sfnetwork null_ok Right", {
  expect_equal(
    sfnetworks_sfnetwork(null_ok = TRUE)(NULL),
    NULL
  )
})

test_that("sfnetworks sfnetwork null_ok Bad", {
  expect_snapshot(error = TRUE, {
    sfnetworks_sfnetwork(null_ok = FALSE)(NULL)
  })
})

test_that("get_sfnetwork_dims", {
  expect_equal(
    get_sfnetwork_dims(network),
    2
  )
})
