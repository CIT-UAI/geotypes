#' @title List type
#' @name List
#' @description typed List with custom types
#'
#' @param types List where each element must have a declared type, ex list(a = typed::Double())
#' @param default_type On elements not declared on types, this will be the default one
#' @param select if "all_of" all the elements declared on types must exists
#' @param only_vals If must exists all and only the values declared on types
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
    only_vals = FALSE,
    empty_ok = FALSE,
    null_ok = FALSE,
    anyNA = FALSE) {
  invisible({
    typed::List(null_ok = null_ok, anyNA = anyNA)(value)
  })

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

  #A List can have sections with names
  #and other listed with numbers
  value_named_id_part <- c()
  value_named_part <- c()
  #In this case, the id is the same as the name
  value_number_id_part <- c()
  names_value <- names(value)

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
  #In this case, the id is the same as the name
  types_number_id_part <- c()
  names_types <- names(types)

  for (name_id in seq_len(length(names_types))) {
    if (
      is.null(names_value[[name_id]]) ||
      names_value[[name_id]] == ""
    ) {
      types_number_id_part <- append(types_number_id_part, name_id)
    } else {
      types_named_id_part <- append(types_named_part, name_id)
      types_named_part <- append(types_named_part, names_types[[name_id]])
    }
  }

  if (only_vals) {
    if (length(types_named_id_part) != length(value_named_id_part)) {
      stop("The Lists does not have the same amount of named elements")
    }
    if (length(types_number_id_part) != length(value_number_id_part)) {
      stop("The Lists does not have the same amount of numbered elements")
    }
    if (FALSE %in% (types_named_part %in% value_named_part)) {
      stop("The Lists does not have the same named elements")
    }
    if (FALSE %in% (types_number_id_part %in% value_number_id_part)) {
      stop("The Lists does not have the same numbered elements")
    }
  }

  if (select == "all_of") {
    local({
      all_named <- types_named_part %in% value_named_part
      if (FALSE %in% all_named) {
        print(types_named_part[!all_named])
        stop("The Lists does not have that named elements")
      }
      all_number <- types_number_id_part %in% value_number_id_part
      if (FALSE %in% all_number) {
        print(types_number_id_part[!all_number])
        stop("The Lists does not have that numbered elements")
      }
    })
  }

  local({
    mix <- types_named_id_part %in% value_number_id_part
    if (TRUE %in% mix){
      print(types_named_part[mix])
      print("This named object should be numbered, not named")
    }
    mix <- types_number_id_part %in% value_named_id_part
    if (TRUE %in% mix){
      print(types_number_id_part[mix])
      print("This numbered objects should be named, not numbered")
    }
  })

  for (name in value_named_part) {
    if (TRUE %in% (name %in% types_named_part)){
      assert <- types[[name]]
    } else {
      assert <- default_type
    }
    tryCatch(assert(value[[name]]), error = function(e) {
      stop(sprintf("element %s %s",i , e$message), call. = FALSE)
    })
  }

  for (id in value_number_id_part) {
    if (TRUE %in% (id %in% types_number_id_part)){
      assert <- types[[id]]
    } else {
      assert <- default_type
    }
    tryCatch(assert(value[[id]]), error = function(e) {
      stop(sprintf("element %s %s",i , e$message), call. = FALSE)
    })
  }

  value
})


#' @title Dataframe type
#' @name Data.frame
#' @description typed Dataframe with custom types
#'
#' @param columns List where each element is a column where it must have a declared type, ex list(a = typed::Double())
#' @param default_type On elements not declared on types, this will be the default one
#' @param select if "all_of" all the elements declared on types must exists
#' @param only_cols If must exists all and only the columns declared on types
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
    only_cols = FALSE,
    empty_ok = TRUE,
    null_ok = FALSE,
    anyNA = FALSE) {

  invisible({
    typed::Data.frame(null_ok = null_ok, anyNA = anyNA)(value)
  })

  if (null_ok && is.null(value)) {
    return(NULL)
  }

  if (!empty_ok && (nrow(value) == 0)) {
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

  name_cols <- names(columns)
  name_val <- names(value)

  if (
    only_cols &&
      (
        (length(name_cols) != length(name_val)) ||
          !setequal(name_cols, name_val)
      )
  ) {
    e <- sprintf(
      "%s\n%s",
      "Columns does not match",
      waldo::compare(
        name_val,
        name_cols,
        x_arg = "names(value)",
        y_arg = "expected"
      )
    )
    stop(e, call. = FALSE)
  }

  if (select == "all_of" && (FALSE %in% (name_cols %in% name_val))) {
    e <- sprintf(
      "%s\n%s",
      "Missed columns",
      waldo::compare(
        name_val,
        name_cols,
        x_arg = "names(value)",
        y_arg = "expected"
      )
    )
    stop(e, call. = FALSE)
  }

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
