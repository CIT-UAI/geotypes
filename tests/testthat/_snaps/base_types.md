# DataFrame Columns Bad

    Code
      Data.frame(columns = list(a = typed::Double(), b = typed::Any(), c = typed::Integer()),
      select = "any_of")(data.frame(a = as.integer(1:10), b = 1:10, c = as.integer(1:
        10)))
    Condition
      Error:
      ! column a, type mismatch
      `typeof(value)`: "integer"
      `expected`:      "double" 

# DataFrame all_of Bad

    Code
      Data.frame(columns = list(a = typed::Double(), b = typed::Any()), select = "all_of")(
        data.frame(b = 1:10, c = as.integer(1:10)))
    Condition
      Error:
      ! Columns does not match
      `names(value)`: "b" "c"
      `expected`:     "a" "b"

# DataFrame only_from Bad

    Code
      Data.frame(columns = list(a = typed::Double(), b = typed::Any()), select = "only_from")(
        data.frame(b = 1:10, c = as.integer(1:10)))
    Condition
      Error:
      ! Columns does not match
      `names(value)`: "b" "c"
      `expected`:     "a" "b"

# DataFrame only_from empty Bad

    Code
      Data.frame(columns = list(), select = "only_from")(data.frame(b = 1:10))
    Condition
      Error:
      ! Columns does not match
      `names(value)` is a character vector ('b')
      `expected` is NULL

# DataFrame exclusively Bad

    Code
      Data.frame(columns = list(a = typed::Double(), b = typed::Any()), select = "exclusively")(
        data.frame(a = as.double(1:10), b = 1:10, c = as.integer(1:10)))
    Condition
      Error:
      ! Columns does not match
      `names(value)`: "a" "b" "c"
      `expected`:     "a" "b"    

# DataFrame exclusively empty Bad

    Code
      Data.frame(columns = list(), select = "exclusively", empty_ok = FALSE)(
        data.frame(a = as.double(1:10)))
    Condition
      Error:
      ! Columns does not match
      `names(value)` is a character vector ('a')
      `expected` is NULL

# DataFrame empty_ok Bad

    Code
      Data.frame(empty_ok = FALSE)(data.frame())
    Condition
      Error:
      ! empty dataframe
      `nrow(value)` is an integer vector (0)
      `expected` is a character vector ('>0')

# DataFrame default_type Bad

    Code
      Data.frame(columns = list(a = typed::Double(), b = typed::Any()), default_type = typed::Integer())(
        data.frame(a = as.double(1:10), b = 1:10, c = as.double(1:10)))
    Condition
      Error:
      ! column c, type mismatch
      `typeof(value)`: "double" 
      `expected`:      "integer"

# List only numbers bad

    Code
      List(types = list(typed::Integer(), typed::Double()))(list(as.integer(1),
      as.integer(2)))
    Condition
      Error:
      ! element 2 type mismatch
      `typeof(value)`: "integer"
      `expected`:      "double" 

# List only named bad

    Code
      List(types = list(a = typed::Integer(), b = typed::Double()))(list(a = as.integer(
        1), b = as.integer(2)))
    Condition
      Error:
      ! element b type mismatch
      `typeof(value)`: "integer"
      `expected`:      "double" 

# List types Numbered Bad

    Code
      List(types = list(typed::Integer(), a = typed::Double()))(list(as.double(1), a = as.double(
        2)))
    Condition
      Error:
      ! element 1 type mismatch
      `typeof(value)`: "double" 
      `expected`:      "integer"

# List types Named Bad

    Code
      List(types = list(typed::Integer(), a = typed::Double()))(list(as.integer(1),
      a = as.integer(2)))
    Condition
      Error:
      ! element a type mismatch
      `typeof(value)`: "integer"
      `expected`:      "double" 

# List default_type Bad

    Code
      List(types = list(typed::Integer()), default_type = typed::Integer())(list(
        as.integer(1), a = as.double(2)))
    Condition
      Error:
      ! element a type mismatch
      `typeof(value)`: "double" 
      `expected`:      "integer"

# List default_type number Bad

    Code
      List(types = list(typed::Integer()), default_type = typed::Integer())(list(
        as.integer(1), as.double(2)))
    Condition
      Error:
      ! element 2 type mismatch
      `typeof(value)`: "double" 
      `expected`:      "integer"

# List default_type named Bad

    Code
      List(types = list(a = typed::Integer()), default_type = typed::Integer())(list(
        b = as.integer(1), a = as.double(2)))
    Condition
      Error:
      ! element a type mismatch
      `typeof(value)`: "double" 
      `expected`:      "integer"

# List select all_of Number Bad

    Code
      List(types = list(typed::Integer(), a = typed::Any(), typed::Double(), b = typed::Double()),
      select = "all_of")(list(as.integer(1), a = 10, b = as.double(4)))
    Condition
      Error:
      ! Columns does not match
      `names(value)`: 1  
          `expected`: 1 3

# List select all_of Named Bad

    Code
      List(types = list(typed::Integer(), a = typed::Any(), typed::Double(), b = typed::Double()),
      select = "all_of")(list(as.integer(1), as.double(1), b = as.double(4)))
    Condition
      Error:
      ! Columns does not match
      `names(value)`: "b"    
      `expected`:     "a" "b"

# List select only_from Number Bad

    Code
      List(types = list(typed::Integer(), a = typed::Any(), typed::Double(), b = typed::Double()),
      select = "only_from")(list(a = 10, as.integer(1), b = as.double(4)))
    Condition
      Error:
      ! Columns does not match
      `names(value)`: 2  
          `expected`: 1 3

# List select only_from Named Bad

    Code
      List(types = list(typed::Integer(), a = typed::Any(), typed::Double(), b = typed::Double()),
      select = "only_from")(list(as.integer(1), c = as.double(4), as.double(1), b = as.double(
        4)))
    Condition
      Error:
      ! Columns does not match
      `names(value)`: "c" "b"
      `expected`:     "a" "b"

# List select exclusively Named Bad

    Code
      List(types = list(typed::Integer(), a = typed::Any(), typed::Double(), b = typed::Double()),
      select = "exclusively")(list(as.integer(1), a = 10, as.double(1)))
    Condition
      Error:
      ! Columns does not match
      `names(value)`: "a"    
      `expected`:     "a" "b"

# List exclusively Numbered Bad

    Code
      List(types = list(typed::Integer(), a = typed::Any(), typed::Double(), b = typed::Double(),
      typed::Integer()), select = "exclusively")(list(as.integer(1), a = 10,
      as.double(10), b = as.double(4)))
    Condition
      Error:
      ! Columns does not match
      `names(value)`: 1 3  
          `expected`: 1 3 5

# List empty_ok Bad

    Code
      List(types = list(typed::Integer(), a = typed::Any(), typed::Double(), b = typed::Double()),
      empty_ok = FALSE)(list())
    Condition
      Error in `f()`:
      ! The list is empty.

# Lists are not dataframes

    Code
      List()(data.frame())
    Condition
      Error:
      ! type mismatch
      `is.data.frame(value)`: TRUE 
      `expected`:             FALSE

# Issue 11 reprex

    Code
      List(default_type = typed::Integer())(list(c(1.5)))
    Condition
      Error:
      ! element 1 type mismatch
      `typeof(value)`: "double" 
      `expected`:      "integer"

