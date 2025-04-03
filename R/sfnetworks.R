#' @title SFnetwork type from sfnetworks
#' @name sfnetworks_sfnetwork
#' @description typed for sfnetwork object from sfnetworks
#'
#' @param null_ok If NULL is acceptable
#' @param nodes_sfc_opts Will use sf_sfc on goemetry column of nodes
#' @param edges_sfc_opts Will use sf_sfc on geometry column of edges
#' @param ... Parsed to assertion_factory
#' @return Assertion for sfnetwork
#' @export
sfnetworks_sfnetwork <- typed::as_assertion_factory(function(
    value,
    null_ok = FALSE,
    nodes_sfc_opts = NULL,
    edges_sfc_opts = NULL) {
  if (null_ok && is.null(value)) {
    return(NULL)
  }
  if (!sfnetworks::is.sfnetwork(value)) {
    e <- sprintf(
      "%s\n%s",
      "type mismatch",
      waldo::compare(
        typeof(value),
        "sfnetworks",
        x_arg = "typeof(value)",
        y_arg = "expected"
      )
    )
    stop(e, call. = FALSE)
  }

  if (!is.null(nodes_sfc_opts)) {
    value %.>%
      sfnetworks::activate(., "nodes") %.>%
      sf::st_as_sf(.) %.>%
      do.call("sf_sfc", nodes_sfc_opts)(sf::st_geometry(.))
  }

  if (!is.null(edges_sfc_opts)) {
    value %.>%
      sfnetworks::activate(., "edges") %.>%
      sf::st_as_sf(.) %.>%
      do.call("sf_sfc", edges_sfc_opts)(sf::st_geometry(.))
  }

  value
})

#' @title Return dimensions of sfnetwork
#' @name get_sfnetwork_dims
#' @description Get the dimensions of the geometries in sfnetwork
#' @param value sfnetwork object
#' @return Vector with contained dimensions
#' @export
get_sfnetwork_dims <- function(value) {
  append(
    value %.>%
      sfnetworks::activate(., "nodes") %.>%
      sf::st_as_sf(.) %.>%
      sf::st_geometry(.) %.>%
      get_sfc_dims(.),
    value %.>%
      sfnetworks::activate(., "edges") %.>%
      sf::st_as_sf(.) %.>%
      sf::st_geometry(.) %.>%
      get_sfc_dims(.),
  ) %.>%
    unique(.) %.>%
    sort(.)
}
