#Some functions needs to compare vectors
#and see if they have some properties between them
compare_vectors <- function(vec1, vec2, select) {
  if (select == "exclusively") {
    compare_vectors_exclusively(vec1, vec2)
  } else if (select == "all_of") {
    compare_vectors_all_of(vec1, vec2)
  } else if (select == "only_from") {
    compare_vectors_only_from(vec1, vec2)
  } else if (select == "any_of") {
    compare_vectors_any_of(vec1, vec2)
  } else {
    stop(paste0("This select option does not exists: ", select))
  }
}

wrong_columns <- function(vec1, vec2) {
  e <- sprintf(
    "%s\n%s",
    "Columns does not match",
    waldo::compare(
      vec1,
      vec2,
      x_arg = "names(value)",
      y_arg = "expected"
    )
  )
  stop(e, call. = FALSE)
}

#vec1 and vec2 must be the same
compare_vectors_exclusively <- function(vec1, vec2) {
  if (!setequal(vec1, vec2)) {
    wrong_columns(vec1, vec2)
  }
}

#vec1 must contain all values from vec2
compare_vectors_all_of <- function(vec1, vec2) {
  if (length(setdiff(vec2, vec1)) != 0) {
    wrong_columns(vec1, vec2)
  }
}

#vec1 can only contain values from vec2
compare_vectors_only_from <- function(vec1, vec2) {
  if (length(setdiff(vec1, vec2)) != 0) {
    wrong_columns(vec1, vec2)
  }
}

#vec1 can contain any value
compare_vectors_any_of <- function(vec1, vec2) {}
