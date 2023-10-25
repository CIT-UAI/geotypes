#' @title List type
#' @name List
#' @description typed List with custom types
#'
#' @param types List where each element must have a
#' declared type, ex list(a = typed::Double())
#' @param default_type On elements not declared on types,
#' this will be the default one
#' @param select Set the relation between the declared
#' elements on types and the existing ones
#' "any_of": Can exist any element
#' "only_from": Only can exists declared elements,
#' empty list is valid
#' "all_of": Must exist all the declared elements
#' "exclusively": Must exist all and only the declared elements
#' @param empty_ok If is acceptable to be empty
#' @param null_ok Parsed to typed::List
#' @param anyNA  Parsed to typed::List
#' @param ... Parsed to typed assertion factory
#' @return Assertion for List
#' @export
List <- typed::as_assertion_factory(function(
    value,
    types = list(),
    default_type = typed::Any(),
    select = "any_of",
    empty_ok = FALSE,
    null_ok = FALSE,
    anyNA = FALSE) {
  value <- typed::List(
    null_ok = null_ok,
    anyNA = anyNA,
    data_frame_ok = FALSE
  )(value)

  if (null_ok && is.null(value)) {
    return(NULL)
  }

  if (length(value) == 0) {
    if (empty_ok) {
      return(list())
    } else {
      stop("The list is empty.")
    }
  }

  # A List can have sections with names
  # and other listed with numbers
  value_named_id_part <- c()
  value_named_part <- c()
  # In this case, the id is the same as the name
  value_number_id_part <- c()
  names_value <- names(value)

  #If the list does not have any named part
  if (is.null(names_value)) {
    names_value <- rep("", length(value))
  }

  for (name_id in seq_len(length(names_value))) {
    if (
      is.null(names_value[[name_id]]) ||
        names_value[[name_id]] == ""
    ) {
      value_number_id_part <- append(value_number_id_part, name_id)
    } else {
      value_named_id_part <- append(value_named_part, name_id)
      value_named_part <- append(value_named_part, names_value[[name_id]])
    }
  }

  types_named_id_part <- c()
  types_named_part <- c()
  # In this case, the id is the same as the name
  types_number_id_part <- c()
  names_types <- names(types)

  #If the list does not have any named part
  if (is.null(names_types)) {
    names_types <- rep("", length(types))
  }

  for (name_id in seq_len(length(names_types))) {
    if (
      is.null(names_types[[name_id]]) ||
        names_types[[name_id]] == ""
    ) {
      types_number_id_part <- append(types_number_id_part, name_id)
    } else {
      types_named_id_part <- append(types_named_part, name_id)
      types_named_part <- append(types_named_part, names_types[[name_id]])
    }
  }

  compare_vectors(value_named_part, types_named_part, select)
  compare_vectors(value_number_id_part, types_number_id_part, select)

  local({
    mix <- types_named_id_part %in% value_number_id_part
    if (TRUE %in% mix) {
      print(types_named_part[mix])
      print("This named object should be numbered, not named")
    }
    mix <- types_number_id_part %in% value_named_id_part
    if (TRUE %in% mix) {
      print(types_number_id_part[mix])
      print("This numbered objects should be named, not numbered")
    }
  })

  for (name in value_named_part) {
    if (TRUE %in% (name %in% types_named_part)) {
      assert <- types[[name]]
    } else {
      assert <- default_type
    }
    tryCatch(assert(value[[name]]), error = function(e) {
      stop(sprintf("element %s %s", name, e$message), call. = FALSE)
    })
  }

  for (id in value_number_id_part) {
    if (TRUE %in% (id %in% types_number_id_part)) {
      assert <- types[[id]]
    } else {
      assert <- default_type
    }
    tryCatch(assert(value[[id]]), error = function(e) {
      stop(sprintf("element %s %s", id, e$message), call. = FALSE)
    })
  }

  value
})


#' @title Dataframe type
#' @name Data.frame
#' @description typed Dataframe with custom types
#'
#' @param columns List where each element is a column where
#' it must have a declared type, ex list(a = typed::Double())
#' @param default_type On elements not declared
#' on types, this will be the default one
#' @param select Set the relations of the declared columns and the existing ones
#' "any_of": Can exists any columns on the dataframe
#' "only_from": Can only exists declared columns
#'  (don't need to exists all of them), empty df is valid
#' "all_of": Must exists at least all the declared columns
#' "exclusively": Must exists and only the declared columns
#' @param empty_ok If is acceptable to be empty
#' @param null_ok Parsed to typed::Data.frame
#' @param anyNA  Parsed to typed::Data.frame
#' @param ... Parsed to assertion_factory
#' @return Assertion for Dataframe
#' @export
Data.frame <- typed::as_assertion_factory(function(
    value,
    columns = list(),
    default_type = typed::Any(),
    select = "any_of",
    empty_ok = TRUE,
    null_ok = FALSE,
    anyNA = FALSE) {

  value <- typed::Data.frame(null_ok = null_ok, anyNA = anyNA)(value)

  if (null_ok && is.null(value)) {
    return(NULL)
  }

  if (nrow(value) == 0) {
    if (empty_ok) {
      return(value)
    } else {
      e <- sprintf(
        "%s\n%s",
        "empty dataframe",
        waldo::compare(
          nrow(value),
          ">0",
          x_arg = "nrow(value)",
          y_arg = "expected"
        )
      )
      stop(e, call. = FALSE)
    }
  }

  name_cols <- names(columns)
  name_val <- names(value)

  compare_vectors(name_val, name_cols, select)

  for (name in name_val) {
    if (TRUE %in% (name %in% name_cols)) {
      assertion <- columns[[name]]
    } else {
      assertion <- default_type
    }
    tryCatch(assertion(value[[name]]), error = function(e) {
      stop(sprintf("column %s, %s", name, e$message), call. = FALSE)
    })
  }

  value
})
