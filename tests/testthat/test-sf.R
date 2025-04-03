gen_test_sfg_dims <- function() {
  points <- list()
  for (dim in 2:4) {
    points[dim] <- sf::st_sfc(sf::st_point(rep(0, dim)))
  }
  multipoints <- list()
  for (n in 1:3) {
    for (dim in 2:4) {
      multipoints[dim] <- sf::st_combine(
        rep(sf::st_sfc(points[[dim]]), n)
      ) %.>%
        sf::st_sfc(.)
    }
  }
  lines <- list()
  for (dim in 2:4) {
    lines[dim] <- sf::st_sfc(
      sf::st_linestring(c(
        points[[dim]],
        points[[dim]]
      ))
    )
  }
  multilines <- list()
  for (n in 1:3) {
    for (dim in 2:4) {
      multilines[dim] <- sf::st_multilinestring(
        rep(sf::st_sfc(lines[dim]), n)
      ) %.>%
        sf::st_sfc(.)
    }
  }
  polygons <- list()
  for (dim in 2:4) {
    polygons[dim] <- sf::st_sfc(sf::st_polygon(multilines[[dim]]))
  }
  multipolygons <- list()
  for (n in 1:3) {
    for (dim in 2:4) {
      multipolygons[dim] <- sf::st_multipolygon(
        rep(sf::st_sfc(polygons[dim]), n)
      ) %.>%
        sf::st_sfc(.)
    }
  }
  pre_geoms <- data.frame(dim = integer(0L), geom = sf::st_sfc())
  for (dim in 2:4) {
    pre_geoms <- dplyr::bind_rows(
      pre_geoms,
      data.frame(
        dim = dim,
        geom = sf::st_sfc(
          points[[dim]],
          multipoints[[dim]],
          lines[[dim]],
          multilines[[dim]],
          polygons[[dim]],
          multipolygons[[dim]]
        )
      )
    )
  }
  geometrycollections <- list()
  for (dim in 2:4) {
    pre_geoms_ <- dplyr::filter(pre_geoms, .data$dim == .env$dim)
    for (n in seq_len(nrow(pre_geoms_))) {
      for (re in 1:3) {
        combs <- t(combn(seq_len(nrow(pre_geoms_)), n))
        for (comb in row(combs)) {
          rn <- pre_geoms_[combs[comb, ], ]
          geometrycollections[[dim]] <- sf::st_geometrycollection(
            rep(rn$geom, re)
          )
        }
      }
    }
  }
  list(
    points = points,
    multipoints = multipoints,
    lines = lines,
    multilines = multilines,
    polygons = polygons,
    multipolygons = multipolygons,
    geometrycollections_geom = geometrycollections
  )
}

gen_test_sfc_dims <- function() {
  sfg_samples <- gen_test_sfg_dims()
  geoms <- list()
  for (dim in 2:4) {
    dim_geoms <- sf::st_sfc()
    for (name in names(sfg_samples)) {
      dim_geoms <- append(
        dim_geoms,
        sf::st_sfc(sfg_samples[[name]][[dim]])
      )
    }
    geoms[[dim]] <- dim_geoms
  }
  geoms
}

test_that("sf sfg valid geometry", {
  point <- sf::st_point(c(0, 1))
  expect_equal(sf_sfg(only_valid = TRUE)(point), point)
})

test_that("sf sfg valid geometry is not important", {
  point <- sf::st_point(c(0, 1))
  expect_equal(sf_sfg(only_valid = FALSE)(point), point)
})

test_that("sf sfg valid geometry is not important by default", {
  point <- sf::st_point(c(0, 1))
  expect_equal(sf_sfg()(point), point)
})

test_that("sf sfg valid point dim", {
  samples <- gen_test_sfg_dims()
  for (geom_type in names(samples)) {
    for (dim in 2:4) {
      geom <- samples[[geom_type]][[dim]]
      expect_equal(sf_sfg(point_dims = dim)(geom), geom)
    }
  }
})

test_that("sf sfg valid point dims", {
  samples <- gen_test_sfg_dims()
  for (geom_type in names(samples)) {
    for (dim in 2:4) {
      geom <- samples[[geom_type]][[dim]]
      expect_equal(sf_sfg(point_dims = 2:4)(geom), geom)
    }
  }
})

test_that("sf sfg invalid point dim", {
  samples <- gen_test_sfg_dims()
  for (geom_type in names(samples)) {
    for (invalid_dim in 2:4) {
      geom <- samples[[geom_type]][[invalid_dim]]
      allowed_dims <- setdiff(2:4, invalid_dim)
      err_msg <- paste0(
        "The geometry can only have points of dimensions of ",
        paste(allowed_dims, collapse = ","),
        "\nThe input has ",
        paste(invalid_dim, collapse = ",")
      )
      expect_error(
        {
          sf_sfg(point_dims = allowed_dims)(geom)
        },
        err_msg,
        fixed = TRUE
      )
    }
  }
})
test_that("sf sfg invalid geometry", {
  polygon <- sf::st_polygon(sf::st_sfc(c(
    sf::st_point(c(0, 0)),
    sf::st_point(c(0, 10)),
    sf::st_point(c(10, 0)),
    sf::st_point(c(10, 10)),
    sf::st_point(c(0, 0))
  )))
  expect_error(
    sf_sfg(only_valid = TRUE)(polygon),
    "The geometry is not valid."
  )
})

test_that("sf sfg invalid geometry is not important", {
  polygon <- sf::st_polygon(sf::st_sfc(c(
    sf::st_point(c(0, 0)),
    sf::st_point(c(0, 10)),
    sf::st_point(c(10, 0)),
    sf::st_point(c(10, 10)),
    sf::st_point(c(0, 0))
  )))
  expect_equal(
    sf_sfg(only_valid = FALSE)(polygon),
    polygon
  )
})

test_that("sf sfg invalid geometry is not important by default", {
  polygon <- sf::st_polygon(sf::st_sfc(c(
    sf::st_point(c(0, 0)),
    sf::st_point(c(0, 10)),
    sf::st_point(c(10, 0)),
    sf::st_point(c(10, 10)),
    sf::st_point(c(0, 0))
  )))
  expect_equal(
    sf_sfg()(polygon),
    polygon
  )
})

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


test_that("sf sfc valid point dim", {
  samples <- gen_test_sfc_dims()
  for (sel in 1:3) {
    opts <- t(combn(2:4, sel))
    for (opt in seq_len(nrow(opts))) {
      val <- sf::st_sfc()
      allow_dims <- opts[opt, ]
      for (n in allow_dims) {
        val <- append(val, samples[[n]])
      }
      expect_equal(
        sf_sfc(point_dims = allow_dims)(val),
        val
      )
    }
  }
})

test_that("sf sfc invalid point dim", {
  samples <- gen_test_sfc_dims()
  for (sel in 1:3) {
    opts <- t(combn(2:4, sel))
    for (opt in seq_len(nrow(opts))) {
      val <- sf::st_sfc()
      deny_dims <- opts[opt, ]
      allow_dims <- setdiff(2:4, deny_dims)
      for (n in deny_dims) {
        val <- append(val, samples[[n]])
      }
      msg <- paste0(
        "The geometries can only have points of dimensions of ",
        paste(allow_dims, collapse = ","),
        "\nThere is geometries with ",
        paste(deny_dims, collapse = ",")
      )
      expect_error(
        sf_sfc(point_dims = allow_dims)(val),
        msg,
        fixed = TRUE
      )
    }
  }
})

sf_example <- system.file("shape/nc.shp", package = "sf") %.>%
  sf::st_read(., quiet = TRUE)

# This one have MULTIPOLYGONS, POLYGONS, POINTS
# Use spsUtil just to hide a warning from sf
sf_example_multi <- spsUtil::quiet({
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
  # This should be typed::Double force error
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

test_that("SF sf active_opts Right", {
  expect_equal(
    sf_sf(active_opts = list("types" = "MULTIPOLYGON"))(sf_example),
    sf_example
  )
})

test_that("SF sf active_opts Bad", {
  expect_snapshot(error = TRUE, {
    sf_sf(active_opts = list("types" = "POLYGON"))(sf_example)
  })
})

test_that("SF sf active_opts multi Right", {
  expect_equal(
    sf_sf(
      active_opts = list(
        "types" = c(
          "MULTIPOLYGON",
          "POLYGON",
          "POINT"
        )
      )
    )(sf_example_multi),
    sf_example_multi
  )
})

test_that("SF sf active_opts multi Bad", {
  expect_snapshot(error = TRUE, {
    sf_sf(
      active_opts = list(
        "types" = c(
          "MULTIPOLYGON",
          "POLYGON"
        )
      )
    )(sf_example_multi)
  })
})

test_that("SF sf active_opts multi extra Right", {
  expect_equal(
    sf_sf(
      active_opts = list(
        "types" = c(
          "MULTIPOLYGON",
          "POLYGON",
          "POINT",
          "LINESTRING"
        )
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
        # Select all columns, notice the
        # active column does no exists in columns param
        select = "all_of"
      )
    )(sf_df),
    sf_df
  )
})

test_that("sf sfc valid geometry", {
  point <- sf::st_point(c(0, 1)) %.>%
    sf::st_sfc(.)
  expect_equal(sf_sfc(only_valid = TRUE)(point), point)
})

test_that("sf sfc valid geometry is not important", {
  point <- sf::st_point(c(0, 1)) %.>%
    sf::st_sfc(.)
  expect_equal(sf_sfc(only_valid = FALSE)(point), point)
})

test_that("sf sfc valid geometry is not important by default", {
  point <- sf::st_point(c(0, 1)) %.>%
    sf::st_sfc(.)
  expect_equal(sf_sfc()(point), point)
})

test_that("sf sfg invalid geometry", {
  polygon <- sf::st_polygon(sf::st_sfc(c(
    sf::st_point(c(0, 0)),
    sf::st_point(c(0, 10)),
    sf::st_point(c(10, 0)),
    sf::st_point(c(10, 10)),
    sf::st_point(c(0, 0))
  )))

  data <- sf::st_sfc(polygon, sf::st_point())

  expect_snapshot(error = TRUE, {
    sf_sfc(only_valid = TRUE)(data)
  })
})

test_that("sf sfc invalid geometry is not important", {
  polygon <- sf::st_polygon(sf::st_sfc(c(
    sf::st_point(c(0, 0)),
    sf::st_point(c(0, 10)),
    sf::st_point(c(10, 0)),
    sf::st_point(c(10, 10)),
    sf::st_point(c(0, 0))
  )))

  data <- sf::st_sfc(polygon, sf::st_point())
  expect_equal(
    sf_sfc(only_valid = FALSE)(data),
    data
  )
})

test_that("sf sfg invalid geometry is not important by default", {
  polygon <- sf::st_polygon(sf::st_sfc(c(
    sf::st_point(c(0, 0)),
    sf::st_point(c(0, 10)),
    sf::st_point(c(10, 0)),
    sf::st_point(c(10, 10)),
    sf::st_point(c(0, 0))
  )))

  data <- sf::st_sfc(polygon, sf::st_point())
  expect_equal(
    sf_sfc()(data),
    data
  )
})

test_that("sf sf valid column_sfc_opts", {
  sdf <- sf::st_sf(
    a = c(),
    geom = sf::st_sfc()
  )
  expect_equal(
    sf_sf(column_sfc_opts = list(
      geom = list()
    ))(sdf),
    sdf
  )
})

test_that("sf sf valid column_sfc_opts one property", {
  sdf <- sf::st_sf(
    a = 1,
    geom = sf::st_sfc(sf::st_point())
  )
  expect_equal(
    sf_sf(column_sfc_opts = list(
      geom = list(
        types = "POINT"
      )
    ))(sdf),
    sdf
  )
})

test_that("sf sf invalid column_sfc_opts one property", {
  sdf <- sf::st_sf(
    a = 1,
    geom = sf::st_sfc(sf::st_linestring())
  )
  expect_snapshot(
    error = TRUE,
    sf_sf(column_sfc_opts = list(
      geom = list(
        types = "POINT"
      )
    ))(sdf)
  )
})

test_that("sf sf invalid column_sfc_opts", {
  sdf <- sf::st_sf(
    a = c(),
    geom = sf::st_sfc()
  )
  expect_snapshot(
    error = TRUE,
    sf_sf(column_sfc_opts = list(
      geom2 = list()
    ))(sdf)
  )
})
