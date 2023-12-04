test_that("DataFrame Columns Right", {
  ret <-
    data.frame(
      a = as.double(1:10),
      b = 1:10,
      c = as.integer(1:10)
    )
  expect_equal(
    Data.frame(
      columns = list(
        a = typed::Double(),
        b = typed::Any(),
        c = typed::Integer()
      ),
      select = "any_of"
    )(ret),
    ret
  )
})

test_that("DataFrame Columns Bad", {
  expect_snapshot(error = TRUE, {
    Data.frame(
      columns = list(
        a = typed::Double(),
        b = typed::Any(),
        c = typed::Integer()
      ),
      select = "any_of"
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
  ret <-
    data.frame(
      a = as.double(1:10),
      d = as.integer(1:10)
    )
  expect_equal(
    Data.frame(
      columns = list(
        a = typed::Double(),
        b = typed::Any(),
        c = typed::Integer()
      ),
      select = "any_of"
    )(ret),
    ret
  )
})

test_that("DataFrame all_of Right", {
  ret <-
    data.frame(
      a = as.double(1:10),
      b = 1:10,
      c = as.integer(1:10)
    )
  expect_equal(
    Data.frame(
      columns = list(
        a = typed::Double()
      ),
      select = "all_of"
    )(ret),
    ret
  )
})

test_that("DataFrame all_of Bad", {
  expect_snapshot(error = TRUE, {
    Data.frame(
      columns = list(
        a = typed::Double(),
        b = typed::Any()
      ),
      select = "all_of"
    )(
      data.frame(
        b = 1:10,
        c = as.integer(1:10)
      )
    )
  })
})

test_that("DataFrame all_of empty Right 1", {
  expect_equal(
    Data.frame(
      columns = list(),
      select = "all_of"
    )(data.frame()),
    data.frame()
  )
})

test_that("DataFrame all_of empty Right 2", {
  expect_equal(
    Data.frame(
      columns = list(),
      select = "all_of"
    )(data.frame(b = 1:10)),
    data.frame(b = 1:10)
  )
})

test_that("DataFrame only_from Right", {
  expect_equal(
    Data.frame(
      columns = list(
        a = typed::Double(),
        b = typed::Integer()
      ),
      select = "only_from"
    )(data.frame(a = as.double(1:10))),
    data.frame(a = as.double(1:10))
  )
})

test_that("DataFrame only_from Bad", {
  expect_snapshot(error = TRUE, {
    Data.frame(
      columns = list(
        a = typed::Double(),
        b = typed::Any()
      ),
      select = "only_from"
    )(
      data.frame(
        b = 1:10,
        c = as.integer(1:10)
      )
    )
  })
})

test_that("DataFrame only_from empty Right", {
  expect_equal(
    Data.frame(
      columns = list(),
      select = "only_from"
    )(data.frame()),
    data.frame()
  )
})

test_that("DataFrame only_from empty Bad", {
  expect_snapshot(error = TRUE, {
    Data.frame(
      columns = list(),
      select = "only_from"
    )(
      data.frame(
        b = 1:10
      )
    )
  })
})

test_that("DataFrame exclusively Right", {
  ret <-
    data.frame(
      a = as.double(1:10),
      b = 1:10
    )
  expect_equal(
    Data.frame(
      columns = list(
        a = typed::Double(),
        b = typed::Any()
      ),
      select = "exclusively"
    )(ret),
    ret
  )
})

test_that("DataFrame exclusively Bad", {
  expect_snapshot(error = TRUE, {
    Data.frame(
      columns = list(
        a = typed::Double(),
        b = typed::Any()
      ),
      select = "exclusively"
    )(
      data.frame(
        a = as.double(1:10),
        b = 1:10,
        c = as.integer(1:10)
      )
    )
  })
})

test_that("DataFrame exclusively empty Right", {
  expect_equal(
    Data.frame(
      columns = c(
        a = typed::Any()
      ),
      select = "exclusively",
      empty_ok = TRUE
    )(data.frame()),
    data.frame()
  )
})

test_that("DataFrame exclusively empty Bad", {
  expect_snapshot(error = TRUE, {
    Data.frame(
      columns = list(),
      select = "exclusively",
      empty_ok = FALSE
    )(
      data.frame(
        a = as.double(1:10)
      )
    )
  })
})

test_that("DataFrame empty_ok Bad", {
  expect_snapshot(error = TRUE, {
    Data.frame(
      empty_ok = FALSE
    )(
      data.frame()
    )
  })
})

test_that("DataFrame empty_ok Right", {
  expect_equal(
    Data.frame(
      empty_ok = TRUE
    )(data.frame()),
    data.frame()
  )
})

test_that("DataFrame default_type Right", {
  ret <-
    data.frame(
      a = as.double(1:10),
      b = 1:10,
      c = as.integer(1:10)
    )
  expect_equal(
    Data.frame(
      columns = list(
        a = typed::Double(),
        b = typed::Any()
      ),
      default_type = typed::Integer()
    )(ret),
    ret
  )
})

test_that("DataFrame default_type Bad", {
  expect_snapshot(error = TRUE, {
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
  ret <-
    list(
      as.integer(1),
      a = as.double(2),
      b = "j"
    )
  expect_equal(
    List(
      types = list(
        typed::Integer(),
        a = typed::Double(),
        b = typed::Any()
      )
    )(ret),
    ret
  )
})

test_that("List only numbers right", {
  ret <-
    list(
      as.integer(1),
      as.double(2)
    )
  expect_equal(
    List(
      types = list(
        typed::Integer(),
        typed::Double()
      )
    )(ret),
    ret
  )
})

test_that("List only numbers bad", {
  expect_snapshot(error = TRUE, {
    List(
      types = list(
        typed::Integer(),
        typed::Double()
      )
    )(
      list(
        as.integer(1),
        as.integer(2)
      )
    )
  })
})

test_that("List only named right", {
  ret <-
    list(
      a = as.integer(1),
      b = as.double(2)
    )
  expect_equal(
    List(
      types = list(
        a = typed::Integer(),
        b = typed::Double()
      )
    )(ret),
    ret
  )
})

test_that("List only named bad", {
  expect_snapshot(error = TRUE, {
    List(
      types = list(
        a = typed::Integer(),
        b = typed::Double()
      )
    )(
      list(
        a = as.integer(1),
        b = as.integer(2)
      )
    )
  })
})

test_that("List types Extra elements Right", {
  ret <-
    list(
      as.integer(1),
      a = as.double(2),
      b = "j",
      c = 10,
      as.integer(20)
    )
  expect_equal(
    List(
      types = list(
        typed::Integer(),
        a = typed::Double(),
        b = typed::Any()
      )
    )(ret),
    ret
  )
})

test_that("List more types than elements Right", {
  ret <-
    list(
      as.integer(1),
      a = as.double(2)
    )
  expect_equal(
    List(
      types = list(
        typed::Integer(),
        typed::Integer(),
        a = typed::Double(),
        b = typed::Any()
      )
    )(ret),
    ret
  )
})

test_that("List types Numbered Bad", {
  expect_snapshot(error = TRUE, {
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
  expect_snapshot(error = TRUE, {
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
  ret <-
    list(
      as.integer(1),
      a = as.integer(2)
    )
  expect_equal(
    List(
      types = list(
        typed::Integer()
      ),
      default_type = typed::Integer()
    )(ret),
    ret
  )
})

test_that("List default_type Bad", {
  expect_snapshot(error = TRUE, {
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

test_that("List default_type number Right", {
  ret <-
    list(
      as.integer(1),
      as.integer(2)
    )
  expect_equal(
    List(
      types = list(
        typed::Integer()
      ),
      default_type = typed::Integer()
    )(ret),
    ret
  )
})

test_that("List default_type number Bad", {
  expect_snapshot(error = TRUE, {
    List(
      types = list(
        typed::Integer()
      ),
      default_type = typed::Integer()
    )(
      list(
        as.integer(1),
        as.double(2)
      )
    )
  })
})

test_that("List default_type named Right", {
  ret <-
    list(
      a = as.integer(1),
      b = as.integer(2)
    )
  expect_equal(
    List(
      types = list(
        a = typed::Integer()
      ),
      default_type = typed::Integer()
    )(ret),
    ret
  )
})

test_that("List default_type named Bad", {
  expect_snapshot(error = TRUE, {
    List(
      types = list(
        a = typed::Integer()
      ),
      default_type = typed::Integer()
    )(
      list(
        b = as.integer(1),
        a = as.double(2)
      )
    )
  })
})

test_that("List select any_of Right", {
  ret <-
    list(
      as.integer(1),
      as.double(2),
      3,
      a = 10
    )
  expect_equal(
    List(
      types = list(
        typed::Integer(),
        typed::Double()
      ),
      select = "any_of"
    )(ret),
    ret
  )
})

test_that("List select all_of Right", {
  ret <-
    list(
      as.integer(1),
      a = 10,
      as.double(2),
      b = as.double(4),
      c = 10
    )
  expect_equal(
    List(
      types = list(
        typed::Integer(),
        a = typed::Any(),
        typed::Double(),
        b = typed::Double()
      ),
      select = "all_of"
    )(ret),
    ret
  )
})

test_that("List select all_of Number Bad", {
  expect_snapshot(error = TRUE, {
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
  expect_snapshot(error = TRUE, {
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

test_that("List select only_from Right", {
  ret <-
    list(
      as.integer(1),
      a = 10
    )
  expect_equal(
    List(
      types = list(
        typed::Integer(),
        a = typed::Any(),
        typed::Double(),
        b = typed::Double()
      ),
      select = "only_from"
    )(ret),
    ret
  )
})

test_that("List select only_from Number Bad", {
  expect_snapshot(error = TRUE, {
    List(
      types = list(
        typed::Integer(),
        a = typed::Any(),
        typed::Double(),
        b = typed::Double()
      ),
      select = "only_from"
    )(
      list(
        a = 10,
        as.integer(1),
        b = as.double(4)
      )
    )
  })
})

test_that("List select only_from Named Bad", {
  expect_snapshot(error = TRUE, {
    List(
      types = list(
        typed::Integer(),
        a = typed::Any(),
        typed::Double(),
        b = typed::Double()
      ),
      select = "only_from"
    )(
      list(
        as.integer(1),
        c = as.double(4),
        as.double(1),
        b = as.double(4)
      )
    )
  })
})

test_that("List select exclusively Order Right", {
  ret <-
    list(
      as.integer(1),
      a = 10,
      as.double(2),
      b = as.double(4)
    )
  expect_equal(
    List(
      types = list(
        typed::Integer(),
        a = typed::Any(),
        typed::Double(),
        b = typed::Double()
      ),
      select = "exclusively"
    )(ret),
    ret
  )
})

test_that("List select exclusively Unorder Right", {
  ret <-
    list(
      as.integer(1),
      b = as.integer(10),
      as.double(2),
      a = as.double(4)
    )
  expect_equal(
    List(
      types = list(
        typed::Integer(),
        a = typed::Any(),
        typed::Double(),
        b = typed::Integer()
      ),
      select = "exclusively"
    )(ret),
    ret
  )
})

test_that("List select exclusively Named Bad", {
  expect_snapshot(error = TRUE, {
    List(
      types = list(
        typed::Integer(),
        a = typed::Any(),
        typed::Double(),
        b = typed::Double()
      ),
      select = "exclusively"
    )(
      list(
        as.integer(1),
        a = 10,
        as.double(1)
      )
    )
  })
})

test_that("List exclusively Numbered Bad", {
  expect_snapshot(error = TRUE, {
    List(
      types = list(
        typed::Integer(),
        a = typed::Any(),
        typed::Double(),
        b = typed::Double(),
        typed::Integer()
      ),
      select = "exclusively"
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
  expect_equal(
    List(
      types = list(
        typed::Integer(),
        a = typed::Any(),
        typed::Double(),
        b = typed::Integer()
      ),
      empty_ok = TRUE
    )(list()),
    list()
  )
})

test_that("List empty_ok Bad", {
  expect_snapshot(error = TRUE, {
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

#In typed you can mix them together,
#here for now, not do it until there
#is a good reason for it
test_that("Lists are not dataframes", {
  expect_snapshot(error = TRUE, {
    List()(data.frame())
  })
})

test_that("Issue 11 reprex", {
  expect_snapshot(error = TRUE, {
    List(
      default_type = typed::Integer()
    )(list(c(1.5)))
  })
})
