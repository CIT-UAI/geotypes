

test_that("DataFrame Columns Right", {
  expect_no_error({
    Data.frame(
      columns = list(
        a = typed::Double(),
        b = typed::Any(),
        c = typed::Integer()
      ),
      select = "any_of",
      only_cols = FALSE
    )(
      data.frame(
        a = as.double(1:10),
        b = 1:10,
        c = as.integer(1:10)
      )
    )
  })
})

test_that("DataFrame Columns Bad", {
  expect_error({
    Data.frame(
      columns = list(
        a = typed::Double(),
        b = typed::Any(),
        c = typed::Integer()
      ),
      select = "any_of",
      only_cols = FALSE
    )(
      data.frame(
        a = as.integer(1:10),
        b = 1:10,
        c = as.integer(1:10)
      )
    )
  })
})

test_that("DataFrame any_of", {
  expect_no_error({
    Data.frame(
      columns = list(
        a = typed::Double(),
        b = typed::Any(),
        c = typed::Integer()
      ),
      select = "any_of",
      only_cols = FALSE
    )(
      data.frame(
        a = as.double(1:10),
        d = as.integer(1:10)
      )
    )
  })
})

test_that("DataFrame all_of Right", {
  expect_no_error({
    Data.frame(
      columns = list(
        a = typed::Double()
      ),
      select = "all_of",
      only_cols = FALSE
    )(
      data.frame(
        a = as.double(1:10),
        b = 1:10,
        c = as.integer(1:10)
      )
    )
  })
})

test_that("DataFrame all_of Bad", {
  expect_error({
    Data.frame(
      columns = list(
        a = typed::Double(),
        b = typed::Any()
      ),
      select = "all_of",
      only_cols = FALSE
    )(
      data.frame(
        b = 1:10,
        c = as.integer(1:10)
      )
    )
  })
})

test_that("DataFrame only_cols Bad", {
  expect_error({
    Data.frame(
      columns = list(
        a = typed::Double(),
        b = typed::Any()
      ),
      only_cols = TRUE
    )(
      data.frame(
        a = as.double(1:10),
        b = 1:10,
        c = as.integer(1:10)
      )
    )
  })
})

test_that("DataFrame only_cols Right", {
  expect_no_error({
    Data.frame(
      columns = list(
        a = typed::Double(),
        b = typed::Any()
      ),
      only_cols = TRUE
    )(
      data.frame(
        a = as.double(1:10),
        b = 1:10
      )
    )
  })
})

test_that("DataFrame empty_ok Bad", {
  expect_error({
    Data.frame(
      empty_ok = FALSE
    )(
      data.frame()
    )
  })
})

test_that("DataFrame empty_ok Right", {
  expect_no_error({
    Data.frame(
      empty_ok = TRUE
    )(
      data.frame()
    )
  })
})

test_that("DataFrame default_type Right", {
  expect_no_error({
    Data.frame(
      columns = list(
        a = typed::Double(),
        b = typed::Any()
      ),
      default_type = typed::Integer()
    )(
      data.frame(
        a = as.double(1:10),
        b = 1:10,
        c = as.integer(1:10)
      )
    )
  })
})

test_that("DataFrame default_type Bad", {
  expect_error({
    Data.frame(
      columns = list(
        a = typed::Double(),
        b = typed::Any()
      ),
      default_type = typed::Integer()
    )(
      data.frame(
        a = as.double(1:10),
        b = 1:10,
        c = as.double(1:10)
      )
    )
  })
})

test_that("List types Right", {
  expect_no_error({
    List(
      types = list(
        typed::Integer(),
        a = typed::Double(),
        b = typed::Any()
      )
    )(
      list(
        as.integer(1),
        a = as.double(2),
        b = "j"
      )
    )
  })
})

test_that("List types Extra elements Right", {
  expect_no_error({
    List(
      types = list(
        typed::Integer(),
        a = typed::Double(),
        b = typed::Any()
      )
    )(
      list(
        as.integer(1),
        a = as.double(2),
        b = "j",
        c = 10,
        as.integer(20)
      )
    )
  })
})

test_that("List types Numbered Bad", {
  expect_error({
    List(
      types = list(
        typed::Integer(),
        a = typed::Double()
      )
    )(
      list(
        as.double(1),
        a = as.double(2)
      )
    )
  })
})

test_that("List types Named Bad", {
  expect_error({
    List(
      types = list(
        typed::Integer(),
        a = typed::Double()
      )
    )(
      list(
        as.integer(1),
        a = as.integer(2)
      )
    )
  })
})

test_that("List default_type Right", {
  expect_no_error({
    List(
      types = list(
        typed::Integer()
      ),
      default_type = typed::Integer()
    )(
      list(
        as.integer(1),
        a = as.integer(2)
      )
    )
  })
})

test_that("List default_type Bad", {
  expect_error({
    List(
      types = list(
        typed::Integer()
      ),
      default_type = typed::Integer()
    )(
      list(
        as.integer(1),
        a = as.double(2)
      )
    )
  })
})

test_that("List select any_of Right", {
  expect_no_error({
    List(
      types = list(
        typed::Integer(),
        typed::Double()
      ),
      select = "any_of"
    )(
      list(
        as.integer(1),
        as.double(2),
        3,
        a = 10
      )
    )
  })
})

test_that("List select all_of Right", {
  expect_no_error({
    List(
      types = list(
        typed::Integer(),
        a = typed::Any(),
        typed::Double(),
        b = typed::Double()
      ),
      select = "all_of"
    )(
      list(
        as.integer(1),
        a = 10,
        as.double(2),
        b = as.double(4),
        c = 10
      )
    )
  })
})

test_that("List select all_of Number Bad", {
  expect_error({
    List(
      types = list(
        typed::Integer(),
        a = typed::Any(),
        typed::Double(),
        b = typed::Double()
      ),
      select = "all_of"
    )(
      list(
        as.integer(1),
        a = 10,
        b = as.double(4)
      )
    )
  })
})

test_that("List select all_of Named Bad", {
  expect_error({
    List(
      types = list(
        typed::Integer(),
        a = typed::Any(),
        typed::Double(),
        b = typed::Double()
      ),
      select = "all_of"
    )(
      list(
        as.integer(1),
        as.double(1),
        b = as.double(4)
      )
    )
  })
})


test_that("List only_vals Order Right", {
  expect_no_error({
    List(
      types = list(
        typed::Integer(),
        a = typed::Any(),
        typed::Double(),
        b = typed::Double()
      ),
      only_vals = TRUE
    )(
      list(
        as.integer(1),
        a = 10,
        as.double(2),
        b = as.double(4)
      )
    )
  })
})

test_that("List only_vals Unorder Right", {
  expect_no_error({
    List(
      types = list(
        typed::Integer(),
        a = typed::Any(),
        typed::Double(),
        b = typed::Integer()
      ),
      only_vals = TRUE
    )(
      list(
        as.integer(1),
        b = as.integer(10),
        as.double(2),
        a = as.double(4)
      )
    )
  })
})

test_that("List only_vals Named Bad", {
  expect_error({
    List(
      types = list(
        typed::Integer(),
        a = typed::Any(),
        typed::Double(),
        b = typed::Double()
      ),
      only_vals = TRUE
    )(
      list(
        as.integer(1),
        a = 10,
        as.double(1)
      )
    )
  })
})

test_that("List only_vals Numbered Bad", {
  expect_error({
    List(
      types = list(
        typed::Integer(),
        a = typed::Any(),
        typed::Double(),
        b = typed::Double(),
        typed::Integer()
      ),
      only_vals = TRUE
    )(
      list(
        as.integer(1),
        a = 10,
        as.double(10),
        b = as.double(4)
      )
    )
  })
})


test_that("List empty_ok Right", {
  expect_no_error({
    List(
      types = list(
        typed::Integer(),
        a = typed::Any(),
        typed::Double(),
        b = typed::Integer()
      ),
      empty_ok = TRUE
    )(
      list()
    )
  })
})

test_that("List empty_ok Bad", {
  expect_error({
    List(
      types = list(
        typed::Integer(),
        a = typed::Any(),
        typed::Double(),
        b = typed::Double()
      ),
      empty_ok = FALSE
    )(
      list()
    )
  })
})