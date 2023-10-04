test_that("SF sfg Right", {
  expect_no_error({
    sf_sfg() ? n
    n <- sf::st_linestring()
    n <- sf::st_point()
    n <- sf::st_polygon()
  })
})

test_that("SF sfg Number Bad", {
  expect_error({
    sf_sfg()(1)
  })
})

test_that("SF sfg sfc Bad", {
  expect_error({
    sf_sfg()(sf::st_sfc())
  })
})

test_that("SF sfg types Right", {
  expect_no_error({
    sf_sfg(types = c("LINESTRING", "POINT")) ? n
    n <- sf::st_linestring()
    n <- sf::st_point()
  })
})

test_that("SF sfg types Bad", {
  expect_error({
    sf_sfg(types = c("LINESTRING", "POINT")) ? n
    n <- sf::st_multilinestring()
  })
})

test_that("SF sfc Right", {
  expect_no_error({
    sf_sfc() ? n
    n <- sf::st_linestring() %.>%
      sf::st_sfc(.)
    n <- sf::st_point() %.>%
      sf::st_sfc(.)
    n <- sf::st_polygon() %.>%
      sf::st_sfc(.)
  })
})

test_that("SF sfc Number Bad", {
  expect_error({
    sf_sfc()(1)
  })
})

test_that("SF sfc sfh Bad", {
  expect_error({
    sf_sfc()(sf::st_sfg())
  })
})

test_that("SF sfc types Right", {
  expect_no_error({
    sf_sfc(types = c("LINESTRING", "POINT")) ? n <- sf::st_sfc()
    n <- append(n, sf::st_linestring() %.>% sf::st_sfc(.))
    n <- append(n, sf::st_point() %.>% sf::st_sfc(.))
  })
})

test_that("SF sfc types Bad", {
  expect_error({
    sf_sfc(types = c("LINESTRING", "POINT")) ? n <- sf::st_sfc()
    n <- append(n, sf::st_linestring() %.>% sf::st_sfc(.))
    n <- append(n, sf::st_point() %.>% sf::st_sfc(.))
    n <- append(n, sf::st_polygon() %.>% sf::st_sfc(.))
  })
})

sf_example <- system.file("shape/nc.shp", package="sf") %.>%
  sf::st_read(., quiet = TRUE)

sf_df_opts <-
  list(
    columns = list(
      "AREA" = typed::Double(),
      "PERIMETER" = typed::Double(),
      "CNTY_" = typed::Double(),
      "CNTY_ID" = typed::Double(),
      "NAME" = typed::Character(),
      "FIPS" = typed::Character(),
      "FIPSNO" = typed::Double(),
      "CRESS_ID" = typed::Integer(),
      "BIR74" = typed::Double(),
      "SID74" = typed::Double(),
      "NWBIR74" = typed::Double(),
      "BIR79" = typed::Double(),
      "SID79" = typed::Double(),
      "NWBIR79" = typed::Double(),
      "geometry" = sf_sfc(types = "MULTIPOLYGON")
    ),
    select = "all_of"
  )

test_that("Data.frame other table opts Right", {
  expect_no_error({
    do.call(Data.frame, sf_df_opts)(sf_example)
  })
})

test_that("SF sf base Right", {
  expect_no_error({
    sf_sf()(sf_example)
  })
})

test_that("SF sf base number Bad", {
  expect_error({
    sf_sf()(10)
  })
})

test_that("SF sf base sfc Bad", {
  expect_error({
    sf_sf()(sf::st_sfc())
  })
})

test_that("SF sf bad column name Bad", {
  expect_error({
    nc2 <- nc
    df_names <- names(nc2) %.>%
      replace(
        .,
        . == attr(nc2, "sf_column"),
        "geom2"
      )
    names(nc2) <- df_names
    sf_sf()(nc2)
  })
})

test_that("SF sf df_opts Right", {
  expect_no_error({
    sf_sf(
      df_opts = sf_df_opts
    )(sf_example)
  })
})

test_that("SF sf df_opts Bad", {
  expect_error({
    new_df_opts <- sf_df_opts
    new_df_opts$columns$AREA <- typed::Integer()
    sf_sf(
      df_opts = new_df_opts
    )(sf_example)
  })
})

test_that("SF sf active_column Right", {
  expect_no_error({
    sf_sf(active_column = "geometry")(sf_example)
  })
})

test_that("SF sf active_column Bad", {
  expect_error({
    sf_sf(active_column = "geom")(sf_example)
  })
})

test_that("SF sf active_types Right", {
  expect_error({
    sf_sf(active_types = "MULTIPOLYGONS")(sf_example)
  })
})

test_that("SF sf active_types Bad", {
  expect_error({
    sf_sf(active_types = "POLYGONS")(sf_example)
  })
})