# throw compare_vectors error

    Code
      wrong_columns(c(), c())
    Condition

# compare_vectors all_of bad

    Code
      compare_vectors(c(1), c(1, 2), "all_of")
    Condition
      Error:
      ! Columns does not match
      `names(value)`: 1  
          `expected`: 1 2

# compare_vectors only_from bad

    Code
      compare_vectors(c(1, 4), c(1, 2), "only_from")
    Condition
      Error:
      ! Columns does not match
      `names(value)`: 1 4
          `expected`: 1 2

# compare_vectors exclusively bad

    Code
      compare_vectors(c(1, 2, 4), c(1, 2), "exclusively")
    Condition
      Error:
      ! Columns does not match
      `names(value)`: 1 2 4
          `expected`: 1 2  

# compare_vectors_only_from

    Code
      compare_vectors_only_from(c(2), c())
    Condition
      Error:
      ! Columns does not match
      `names(value)` is a double vector (2)
      `expected` is NULL

---

    Code
      compare_vectors_only_from(c(2), c(1))
    Condition
      Error:
      ! Columns does not match
      `names(value)`: 2
          `expected`: 1

---

    Code
      compare_vectors_only_from(c(1, 2), c(1))
    Condition
      Error:
      ! Columns does not match
      `names(value)`: 1 2
          `expected`: 1  

# compare_vectors_all_of

    Code
      compare_vectors_all_of(c(), c(1))
    Condition
      Error:
      ! Columns does not match
      `names(value)` is NULL
      `expected` is a double vector (1)

---

    Code
      compare_vectors_all_of(c(2), c(1))
    Condition
      Error:
      ! Columns does not match
      `names(value)`: 2
          `expected`: 1

---

    Code
      compare_vectors_all_of(c(1, 3), c(1, 2))
    Condition
      Error:
      ! Columns does not match
      `names(value)`: 1 3
          `expected`: 1 2

---

    Code
      compare_vectors_all_of(c(4, 3), c(1, 2))
    Condition
      Error:
      ! Columns does not match
      `names(value)`: 4 3
          `expected`: 1 2

# compare_vectors_exclusively

    Code
      compare_vectors_exclusively(c(), c(1))
    Condition
      Error:
      ! Columns does not match
      `names(value)` is NULL
      `expected` is a double vector (1)

---

    Code
      compare_vectors_exclusively(c(1), c())
    Condition
      Error:
      ! Columns does not match
      `names(value)` is a double vector (1)
      `expected` is NULL

---

    Code
      compare_vectors_exclusively(c(1, 2), c(1))
    Condition
      Error:
      ! Columns does not match
      `names(value)`: 1 2
          `expected`: 1  

---

    Code
      compare_vectors_exclusively(c(1), c(1, 2))
    Condition
      Error:
      ! Columns does not match
      `names(value)`: 1  
          `expected`: 1 2

