#' @include base_types.R

#' @title SF: Geometry Type
#' @name sf_sfg
#' @description typed for geometry types from SF
#'
#' @param types Vector with possible acceptable geometry types
#' @param only_valid TRUE to only accept a valid geometry
#' @param ... Parsed to assertion_factory
#' @return Assertion for sfg
#' @export
sf_sfg <- typed::as_assertion_factory(function(
    value,
    types = NULL,
    only_valid = NULL) {
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

  value
})

#' @title SF: Geometry Column Type
#' @name sf_sfc
#' @description typed for geometry column types from SF
#'
#' @param types Vector with possible acceptable geometry types
#' @param only_valid TRUE to accept sfc only with valid geometries
#' @param ... Parsed to assertion_factory
#' @return Assertion for sfg
#' @export
sf_sfc <- typed::as_assertion_factory(function(
    value,
    types = NULL,
    only_valid = NULL) {
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
#' @param ... Parsed to assertion_factory
#' @return Assertion for sf
#' @export
sf_sf <- typed::as_assertion_factory(function(
    value,
    df_opts = list(),
    active_column = NULL,
    active_opts = list()) {
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
    #If there is no df_opts, we need to include the active column
    #active_column can exist or not, but current_active_column
    #always exists, and if both exists they are the same
    df_opts$columns <- local({
      opts <- list()
      opts[[current_active_column]] <- do.call("sf_sfc", active_opts)
      opts
    })
  } else {
    if (FALSE %in% (current_active_column %in% names(df_opts$columns))) {
      #If the active column is not defined, we need to add it
      df_opts$columns[[current_active_column]] <- do.call("sf_sfc", active_opts)
    }
  }

  invisible({
    do.call("Data.frame", df_opts)(value)
  })

  if (!is.null(active_opts) && (length(active_opts) > 0)) {
    do.call("sf_sfc", active_opts)(sf::st_geometry(value))
  }

  value
})
