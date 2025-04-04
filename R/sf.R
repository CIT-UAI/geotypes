#' @include base_types.R

#' @title SF: Geometry Type
#' @name sf_sfg
#' @description typed for geometry types from SF
#'
#' @param types Vector with possible acceptable geometry types
#' @param only_valid TRUE to only accept a valid geometry
#' @param point_dims A vector which declares how much dimensions are accepted
#' @param ... Parsed to assertion_factory
#' @return Assertion for sfg
#' @export
sf_sfg <- typed::as_assertion_factory(function(
    value,
    types = NULL,
    only_valid = NULL,
    point_dims = NULL) {
  if (!inherits(value, "sfg")) {
    e <- sprintf(
      "%s\n%s",
      "Not geometry",
      waldo::compare(
        class(value),
        "sfg",
        x_arg = "class(value)",
        y_arg = "expected"
      )
    )
    stop(e, call. = FALSE)
  }

  if (!is.null(types)) {
    val_type <- as.vector(sf::st_geometry_type(value))

    if (!(val_type %in% types)) {
      e <- sprintf(
        "%s\n%s",
        "Wrong geometry type",
        waldo::compare(
          val_type,
          types,
          x_arg = "sf::st_geometry_type(value)",
          y_arg = "expected"
        )
      )
      stop(e, call. = FALSE)
    }
  }

  if (!is.null(only_valid) && only_valid) {
    if (!sf::st_is_valid(value)) {
      stop("The geometry is not valid.")
    }
  }

  if (!is.null(point_dims)) {
    dims <- sf::st_sfc(value) %.>%
      sf::st_cast(., "POINT") %.>%
      sapply(
        .,
        function(x) {
          txt <- sf::st_as_text(x)
          if (startsWith(txt, "POINT M (")) {
            return("XYM")
          }
          if (startsWith(txt, "POINT Z (")) {
            return("XYZ")
          }
          if (startsWith(txt, "POINT ZM (")) {
            return("XYZM")
          }
          if (startsWith(txt, "POINT (")) {
            return("XY")
          }
          stop(paste0(
            "Point dimension not recognized :",
            txt
          ))
        }
      ) %.>%
      unique(.) %.>%
      sort(.)
    if (!(TRUE %in% all(dims %in% point_dims))) {
      stop(paste0(
        "The geometry can only have points of dimensions of ",
        paste(sort(point_dims), collapse = ","),
        "\nThe input has ",
        paste(sort(dims), collapse = ",")
      ))
    }
  }

  value
})

#' @title Return dimensions of sfc
#' @name get_sfc_dims
#' @description Get the dimensions of the geometries in sfc
#' @param value SFC object
#' @return Vector with contained dimensions
#' @export
get_sfc_dims <- function(value) {
  value %.>%
    sapply(
      .,
      function(x) {
        sf::st_sfc(x) %.>%
          sf::st_cast(., "POINT") %.>%
          sapply(
            .,
            function(x) {
              txt <- sf::st_as_text(x)
              if (startsWith(txt, "POINT M (")) {
                return("XYM")
              }
              if (startsWith(txt, "POINT Z (")) {
                return("XYZ")
              }
              if (startsWith(txt, "POINT ZM (")) {
                return("XYZM")
              }
              if (startsWith(txt, "POINT (")) {
                return("XY")
              }
              stop(paste0(
                "Point dimension not recognized :",
                txt
              ))
            }
          )
      }
    ) %.>%
    unlist(.) %.>%
    unique(.) %.>%
    sort(.)
}

#' @title SF: Geometry Column Type
#' @name sf_sfc
#' @description typed for geometry column types from SF
#'
#' @param types Vector with possible acceptable geometry types
#' @param only_valid TRUE to accept sfc only with valid geometries
#' @param point_dims A vector which declares how much dimensions are accepted
#' @param uniform_dim The sfc can only have geometries of the same dimension
#' @param ... Parsed to assertion_factory
#' @return Assertion for sfg
#' @export
sf_sfc <- typed::as_assertion_factory(function(
    value,
    types = NULL,
    only_valid = NULL,
    point_dims = NULL,
    uniform_dim = NULL) {
  if (!inherits(value, "sfc")) {
    e <- sprintf(
      "%s\n%s",
      "Not geometry column",
      waldo::compare(
        class(value),
        "sfc",
        x_arg = "class(value)",
        y_arg = "expected"
      )
    )
    stop(e, call. = FALSE)
  }

  if (!is.null(types)) {
    vals_type <- as.vector(sf::st_geometry_type(value))

    for (id in seq_len(length(vals_type))) {
      val_type <- vals_type[[id]]
      if (!(val_type %in% types)) {
        e <- sprintf(
          "%s\n%s",
          "Wrong geometry type",
          waldo::compare(
            val_type,
            types,
            x_arg = paste0("sf::st_geometry_type(value[[", id, "]])"),
            y_arg = "expected"
          )
        )
        stop(e, call. = FALSE)
      }
    }
  }

  if (!is.null(only_valid) && only_valid) {
    is_valid <- sf::st_is_valid(value)
    if (!(TRUE %in% all(is_valid))) {
      print(which(!(is_valid %in% TRUE)))
      stop("This geometries are not valid.")
    }
  }

  if (!is.null(point_dims) || !is.null(uniform_dim)) {
    dims <- get_sfc_dims(value)
    if (!is.null(point_dims)) {
      if (!(TRUE %in% all(dims %in% point_dims))) {
        stop(paste0(
          "The geometries can only have points of dimensions of ",
          paste(sort(point_dims), collapse = ","),
          "\nThere is geometries with ",
          paste(sort(dims), collapse = ",")
        ))
      }
    }
    if (!is.null(uniform_dim) && uniform_dim && (length(dims) > 1)) {
      stop(paste0(
        "The geometry column can only have one dimesion,",
        " it have a mix of: ",
        paste(sort(dims), collapse = ",")
      ))
    }
  }

  value
})

#' @title SF: SF Object
#' @name sf_sf
#' @description typed for SF object from SF
#'
#' @param df_opts SF object is a dataframe too,
#' this is a list where they will be parsed to the Data.frame type
#' @param active_column Name of the active column
#' @param active_opts List with possible params of sf_sfc to apply
#' to the active column
#' @param column_sfc_opts A list which the next propoerties:
#' name: Column name where the options will be applied
#' value: This values will be parsed to sf_sfc
#' @param default_sfc_opts All columns that are not specified on
#' column_sfc_opts will have this one as sfc options
#' @param ... Parsed to assertion_factory
#' @return Assertion for sf
#' @export
sf_sf <- typed::as_assertion_factory(function(
    value,
    df_opts = list(),
    active_column = NULL,
    active_opts = list(),
    column_sfc_opts = NULL,
    default_sfc_opts = NULL) {
  if (!inherits(value, "sf")) {
    e <- sprintf(
      "%s\n%s",
      "Not sf object",
      waldo::compare(
        class(value),
        "sf",
        x_arg = "class(value)",
        y_arg = "expected"
      )
    )
    stop(e, call. = FALSE)
  }

  current_active_column <- attr(value, "sf_column")
  # Check the active column exists
  if (!(current_active_column %in% names(value))) {
    e <- sprintf(
      "%s\n%s",
      "Active geometry column does not exists",
      waldo::compare(
        current_active_column,
        names(value),
        x_arg = "attr(value, \"sf_column\")",
        y_arg = "expected"
      )
    )
    stop(e, call. = FALSE)
  }

  if (!is.null(active_column) && (current_active_column != active_column)) {
    e <- sprintf(
      "%s\n%s",
      "SF does not have the required active geometry column",
      waldo::compare(
        current_active_column,
        active_column,
        x_arg = "attr(value, \"sf_column\")",
        y_arg = "expected"
      )
    )
    stop(e, call. = FALSE)
  }

  if (FALSE %in% ("columns" %in% names(df_opts))) {
    # If there is no df_opts, we need to include the active column
    # active_column can exist or not, but current_active_column
    # always exists, and if both exists they are the same
    df_opts$columns <- local({
      opts <- list()
      opts[[current_active_column]] <- do.call("sf_sfc", active_opts)
      opts
    })
  } else {
    if (FALSE %in% (current_active_column %in% names(df_opts$columns))) {
      # If the active column is not defined, we need to add it
      df_opts$columns[[current_active_column]] <- do.call("sf_sfc", active_opts)
    }
  }

  invisible({
    do.call("Data.frame", df_opts)(value)
  })

  if (!is.null(active_opts) && (length(active_opts) > 0)) {
    do.call("sf_sfc", active_opts)(sf::st_geometry(value))
  }

  if (!is.null(column_sfc_opts) && (length(column_sfc_opts) > 0)) {
    for (col in names(column_sfc_opts)) {
      if (!(TRUE %in% (col %in% names(value)))) {
        stop(paste0("Column ", col, " does not exists."))
      }
      do.call("sf_sfc", column_sfc_opts[[col]])(value[[col]])
    }
  }

  if (!is.null(default_sfc_opts)) {
    sfc_cols <- local({
      cols <- c()
      for (col in names(value)) {
        if (inherits(value[[col]], "sfc")) {
          cols <- append(cols, col)
        }
      }
      cols
    })
    for (col in setdiff(sfc_cols, names(column_sfc_opts))) {
      do.call("sf_sfc", default_sfc_opts)(value[[col]])
    }
  }

  value
})
