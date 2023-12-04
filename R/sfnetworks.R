#' @title SFnetwork type from sfnetworks
#' @name sfnetworks_sfnetwork
#' @description typed for sfnetwork object from sfnetworks
#'
#' @param null_ok If NULL is acceptable
#' @param ... Parsed to assertion_factory
#' @return Assertion for sfnetwork
#' @export
sfnetworks_sfnetwork <- typed::as_assertion_factory(function(
    value,
    null_ok = FALSE) {
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

  value
})
