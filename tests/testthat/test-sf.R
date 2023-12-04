test_that("SF sfg Right", {
  line <- sf::st_linestring()
  point <- sf::st_point()
  polygon <- sf::st_polygon()
  expect_equal(sf_sfg()(line), line)
  expect_equal(sf_sfg()(point), point)
  expect_equal(sf_sfg()(polygon), polygon)
})

test_that("SF sfg Number Bad", {
  expect_snapshot(error = TRUE, {
    sf_sfg()(1)
  })
})

test_that("SF sfg sfc Bad", {
  expect_snapshot(error = TRUE, {
    sf_sfg()(sf::st_sfc())
  })
})

test_that("SF sfg types Right", {
  line <- sf::st_linestring()
  point <- sf::st_point()
  expect_equal(
    sf_sfg(types = c("LINESTRING", "POINT"))(line),
    line
  )
  expect_equal(
    sf_sfg(types = c("LINESTRING", "POINT"))(point),
    point
  )
})

test_that("SF sfg types Bad", {
  expect_snapshot(error = TRUE, {
    sf_sfg(types = c("LINESTRING", "POINT"))(
      sf::st_multilinestring()
    )
  })
})

test_that("SF sfc Right", {
  line <- sf::st_linestring() %.>%
    sf::st_sfc(.)
  point <- sf::st_point() %.>%
    sf::st_sfc(.)
  polygon <- sf::st_polygon() %.>%
    sf::st_sfc(.)
  expect_equal(sf_sfc()(line), line)
  expect_equal(sf_sfc()(point), point)
  expect_equal(sf_sfc()(polygon), polygon)
})

test_that("SF sfc Number Bad", {
  expect_snapshot(error = TRUE, {
    sf_sfc()(1)
  })
})

test_that("SF sfc sfc Bad", {
  expect_snapshot(error = TRUE, {
    sf_sfc()(sf::st_point())
  })
})

test_that("SF sfc types Right", {
  line <- sf::st_linestring() %.>%
    sf::st_sfc(.)
  point <- sf::st_point() %.>%
    sf::st_sfc(.)
  expect_equal(
    sf_sfc(types = c("LINESTRING", "POINT"))(line),
    line
  )
  expect_equal(
    sf_sfc(types = c("LINESTRING", "POINT"))(point),
    point
  )
})

test_that("SF sfc types Bad", {
  expect_snapshot(error = TRUE, {
    sf_sfc(types = c("LINESTRING", "POINT"))(
      sf::st_polygon() %.>% sf::st_sfc(.)
    )
  })
})

sf_example <- system.file("shape/nc.shp", package = "sf") %.>%
  sf::st_read(., quiet = TRUE)

#This one have MULTIPOLYGONS, POLYGONS, POINTS
#Use spsUtil just to hide a warning from sf
sf_example_multi <-  spsUtil::quiet({
  sf_example %.>%
    sf::st_cast(., "POLYGON", warn = FALSE) %.>%
    rbind(., sf::st_centroid(.)) %.>%
    rbind(., sf_example)
})

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

test_that("SF sf works with Data.frame Right", {
  expect_equal(
    do.call(Data.frame, sf_df_opts)(sf_example),
    sf_example
  )
})

test_that("SF sf base Right", {
  expect_equal(
    sf_sf()(sf_example),
    sf_example
  )
})

test_that("SF sf base number Bad", {
  expect_snapshot(error = TRUE, {
    sf_sf()(10)
  })
})

test_that("SF sf base sfc Bad", {
  expect_snapshot(error = TRUE, {
    sf_sf()(sf::st_sfc())
  })
})

test_that("SF sf bad column name Bad", {
  nc2 <- sf_example
  df_names <- names(nc2) %.>%
    replace(
      .,
      . == attr(nc2, "sf_column"),
      "geom2"
    )
  names(nc2) <- df_names
  expect_snapshot(error = TRUE, {
    sf_sf()(nc2)
  })
})

test_that("SF sf df_opts Right", {
  expect_equal(
    sf_sf(
      df_opts = sf_df_opts
    )(sf_example),
    sf_example
  )
})

test_that("SF sf df_opts Bad", {
  new_df_opts <- sf_df_opts
  #This should be typed::Double force error
  new_df_opts$columns$AREA <- typed::Integer()
  expect_snapshot(error = TRUE, {
    sf_sf(
      df_opts = new_df_opts
    )(sf_example)
  })
})

test_that("SF sf active_column Right", {
  expect_equal(
    sf_sf(active_column = "geometry")(sf_example),
    sf_example
  )
})

test_that("SF sf active_column Bad", {
  expect_snapshot(error = TRUE, {
    sf_sf(active_column = "geom")(sf_example)
  })
})

test_that("SF sf active_types Right", {
  expect_equal(
    sf_sf(active_types = "MULTIPOLYGON")(sf_example),
    sf_example
  )
})

test_that("SF sf active_types Bad", {
  expect_snapshot(error = TRUE, {
    sf_sf(active_types = "POLYGON")(sf_example)
  })
})

test_that("SF sf active_types multi Right", {
  expect_equal(
    sf_sf(
      active_types = c(
        "MULTIPOLYGON",
        "POLYGON",
        "POINT"
      )
    )(sf_example_multi),
    sf_example_multi
  )
})

test_that("SF sf active_types multi Bad", {
  expect_snapshot(error = TRUE, {
    sf_sf(
      active_types = c(
        "MULTIPOLYGON",
        "POLYGON"
      )
    )(sf_example_multi)
  })
})

test_that("SF sf active_types multi extra Right", {
  expect_equal(
    sf_sf(
      active_types = c(
        "MULTIPOLYGON",
        "POLYGON",
        "POINT",
        "LINESTRING"
      )
    )(sf_example_multi),
    sf_example_multi
  )
})

test_that("SF sf: select all columns and active column", {
  df <- data.frame(
    a = 1,
    geom = sf::st_linestring() %.>% sf::st_sfc(.)
  )
  sf_df <- sf::st_as_sf(df)
  expect_equal(
    sf_sf(
      df_opts = list(
        columns = list(
          a = typed::Double()
        ),
        #Select all columns, notice the
        #active column does no exists in columns param
        select = "all_of"
      )
    )(sf_df),
    sf_df
  )
})
