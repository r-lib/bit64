with_parameters_test_that("union works with basic R types (except double)", {
    if (getRversion() > "3.6.0" || !(type_x %in% c("POSIXct", "Date"))) {
      y = 5:10
      if (!is.na(type_x))
        x = eval(parse(text=paste0("as.", type_x, "(x)")))
      if (type_y == "integer64" && type_x %in% c(NA, "logical", "raw", "integer", "double")) {
        expected_result_x_y = as.integer64(base::union(x, y))
        expected_result_y_x = as.integer64(base::union(y, x))
      } else if (type_y == "integer64" && type_x %in% c("POSIXct", "Date")) {
        # as.POSIXct(1L) and as.Date(1L) stay internally integer
        # as.POSIXct(1) and as.Date(1) stay internally double
        y = as.double(y)
        expected_result_x_y = base::union(x, y)
        expected_result_y_x = base::union(y, x)
      } else {
        expected_result_x_y = base::union(x, y)
        expected_result_y_x = base::union(y, x)
      }
      y = as(y, type_y)
  
      expect_identical(
        union(x, y),
        expected_result_x_y
      )
      expect_identical(
        union(y, x),
        expected_result_y_x
      )
    }
  }, 
  .cases=expand.grid(
    x=I(list(NULL, c(1:7, 2L, 11L))),
    type_x=c(NA, "double", "logical", "integer", "character", "POSIXct", "Date", "complex", "factor", "ordered"),
    type_y=c("integer", "integer64"),
    stringsAsFactors=FALSE
  )
)

test_that("union works with special logic for double", {
  
  expect_identical(union(1.5:7, 5:10), base::union(1.5:7, 5:10))
  expect_identical(union(1.5:7, as.integer64(5:10)), as.integer64(1:10))
  expect_identical(union(as.integer64(5:10), 1.5:7), as.integer64(c(5:10, 1:4)))
  
})

test_that("union works (additional cases)", {
  
  expect_identical(union(c(5, 3, 5), c(0, 2, 1, 3, 6)), base::union(c(5, 3, 5), c(0, 2, 1, 3, 6)))
  expect_identical(union(c(5L, 3L, 5L), as.integer64(c(0, 2, 1, 3, 6))), as.integer64(c(5, 3, 0, 2, 1, 6)))
  
  expect_identical(union(NA, NA_integer_), base::union(NA, NA_integer_))
  expect_identical(union(NA_integer_, NA), base::union(NA_integer_, NA))
  expect_identical(union(NA, NA_integer64_), NA_integer64_)
  expect_identical(union(NA_integer64_, NA), NA_integer64_)
  
})


with_parameters_test_that("intersect works with basic R types (except double)", {
    if (getRversion() > "3.6.0" || !(type_x %in% c("POSIXct", "Date"))) {
      y = 5:10
      if (!is.na(type_x))
        x = eval(parse(text=paste0("as.", type_x, "(x)")))
      if (type_y == "integer64" && type_x %in% c(NA, "logical", "raw", "integer", "double")) {
        expected_result_x_y = as.integer64(base::intersect(x, y))
        expected_result_y_x = as.integer64(base::intersect(y, x))
      } else if (type_y == "integer64" && type_x %in% c("POSIXct", "Date")) {
        # as.POSIXct(1L) and as.Date(1L) stay internally integer
        # as.POSIXct(1) and as.Date(1) stay internally double
        y = as.double(y)
        expected_result_x_y = base::intersect(x, y)
        expected_result_y_x = base::intersect(y, x)
      } else {
        expected_result_x_y = base::intersect(x, y)
        if (getRversion() <= "3.6.0" && type_x %in% c("character", "factor", "ordered") && type_y == "integer64")
          expected_result_x_y = as.character(expected_result_x_y)
        if (getRversion() <= "3.6.0" && type_x == "complex" && type_y == "integer64")
          expected_result_x_y = as.complex(expected_result_x_y)
        expected_result_y_x = base::intersect(y, x)
      }
      y = as(y, type_y)
  
      expect_identical(
        intersect(x, y),
        expected_result_x_y
      )
      expect_identical(
        intersect(y, x),
        expected_result_y_x
      )
    }
  }, 
  .cases=expand.grid(
    x=I(list(NULL, c(1:7, 2L, 11L))),
    type_x=c(NA, "double", "logical", "integer", "character", "POSIXct", "Date", "complex", "factor", "ordered"),
    type_y=c("integer", "integer64"),
    stringsAsFactors=FALSE
  )
)

test_that("intersect works with special logic for double", {
  
  expect_identical(intersect(1.5:7, 5:10), base::intersect(1.5:7, 5:10))
  expect_identical(intersect(1.5:7, as.integer64(5:10)), as.integer64(5:6))
  expect_identical(intersect(as.integer64(5:10), 1.5:7), as.integer64(5:6))
  
})

test_that("intersect works (additional cases)", {
  
  expect_identical(intersect(c(5, 3, 5), c(0, 2, 1, 3, 6)), base::intersect(c(5, 3, 5), c(0, 2, 1, 3, 6)))
  expect_identical(intersect(as.integer64(c(5, 3, 5)), as.integer64(c(0, 2, 1, 3, 6))), as.integer64(3L))
  
  expect_identical(intersect(NA, NA_integer_), base::intersect(NA, NA_integer_))
  expect_identical(intersect(NA_integer_, NA), base::intersect(NA_integer_, NA))
  expect_identical(intersect(NA, NA_integer64_), NA_integer64_)
  expect_identical(intersect(NA_integer64_, NA), NA_integer64_)
  
})


with_parameters_test_that("setdiff works with basic R types (except double)", {
    if (getRversion() > "3.6.0" || !(type_x %in% c("POSIXct", "Date"))) {
      y = 5:10
      if (!is.na(type_x))
        x = eval(parse(text=paste0("as.", type_x, "(x)")))
      if (type_y == "integer64") {
        expected_result_x_y = base::setdiff(x, y)
        expected_result_y_x = as.integer64(base::setdiff(y, x))
      } else {
        expected_result_x_y = base::setdiff(x, y)
        expected_result_y_x = base::setdiff(y, x)
      }
      y = as(y, type_y)
  
      expect_identical(
        setdiff(x, y),
        expected_result_x_y
      )
      expect_identical(
        setdiff(y, x),
        expected_result_y_x
      )
    }
  }, 
  .cases=expand.grid(
    x=I(list(NULL, c(1:7, 2L, 11L))),
    type_x=c(NA, "double", "logical", "integer", "character", "POSIXct", "Date", "complex", "factor", "ordered"),
    type_y=c("integer", "integer64"),
    stringsAsFactors=FALSE
  )
)

test_that("setdiff works with special logic for double", {
  
  expect_identical(setdiff(1.5:7, 5:10), base::setdiff(1.5:7, 5:10))
  expect_identical(setdiff(1.5:7, as.integer64(5:10)), 1.5:4.5)
  expect_identical(setdiff(as.integer64(5:10), 1.5:7), as.integer64(7:10))
  
})

test_that("setdiff works (additional cases)", {
  
  expect_identical(setdiff(c(5, 3, 5), c(0, 2, 1, 3, 6)), base::setdiff(c(5, 3, 5), c(0, 2, 1, 3, 6)))
  expect_identical(setdiff(as.integer64(c(5, 3, 5)), as.integer64(c(0, 2, 1, 3, 6))), as.integer64(5L))
  
  expect_identical(setdiff(NA, NA_integer_), base::setdiff(NA, NA_integer_))
  expect_identical(setdiff(NA_integer_, NA), base::setdiff(NA_integer_, NA))
  expect_identical(setdiff(NA, NA_integer64_), logical())
  expect_identical(setdiff(NA_integer64_, NA), integer64())
  
})

with_parameters_test_that("setequal works with basic R types (except double)", {
    if (getRversion() > "3.6.0" || !(type_x %in% c("POSIXct", "Date"))) {
      y = 5:10
      if (!is.na(type_x))
        x = eval(parse(text=paste0("as.", type_x, "(x)")))
      expected_result_x_y = base::setequal(x, y)
      expected_result_y_x = base::setequal(y, x)
      y = as(y, type_y)
  
      expect_identical(
        setequal(x, y),
        expected_result_x_y
      )
      expect_identical(
        setequal(y, x),
        expected_result_y_x
      )
    }
  }, 
  .cases=expand.grid(
    x=I(list(NULL, c(1:7, 2L, 11L))),
    type_x=c(NA, "double", "logical", "integer", "character", "POSIXct", "Date", "complex", "factor", "ordered"),
    type_y=c("integer", "integer64"),
    stringsAsFactors=FALSE
  )
)

test_that("setequal works with special logic for double", {
  
  expect_identical(setequal(1.5:7, 5:10), base::setequal(1.5:7, 5:10))
  expect_true(setequal(1.5:7, as.integer64(6:1)))
  expect_true(setequal(as.integer64(6:1), 1.5:7))

})

test_that("setequal works (additional cases)", {
  
  expect_identical(setequal(c(5, 3, 5), c(0, 2, 1, 3, 6)), base::setequal(c(5, 3, 5), c(0, 2, 1, 3, 6)))
  expect_false(setequal(as.integer64(c(5, 3, 5)), as.integer64(c(0, 2, 1, 3, 6))))
  expect_true(setequal(as.integer64(c(5, 3, 5)), as.integer64(c(3, 3, 3, 3, 5))))
  
  expect_true(setequal(NA, NA_integer_)
  expect_true(setequal(NA_integer_, NA))
  expect_true(setequal(NA, NA_integer64_))
  expect_true(setequal(NA_integer64_, NA))
  
})

with_parameters_test_that("is.element works with basic R types (except double)", {
    if (getRversion() > "3.6.0" || !(type_x %in% c("POSIXct", "Date"))) {
      y = 5:10
      if (!is.na(type_x))
        x = eval(parse(text=paste0("as.", type_x, "(x)")))
      expected_result_x_y = base::is.element(x, y)
      expected_result_y_x = base::is.element(y, x)
      y = as(y, type_y)
  
      expect_identical(
        is.element(x, y),
        expected_result_x_y
      )
      expect_identical(
        is.element(y, x),
        expected_result_y_x
      )
    }
  }, 
  .cases=expand.grid(
    x=I(list(NULL, c(1:7, 2L, 11L))),
    type_x=c(NA, "double", "logical", "integer", "character", "POSIXct", "Date", "complex", "factor", "ordered"),
    type_y=c("integer", "integer64"),
    stringsAsFactors=FALSE
  )
)

test_that("is.element works with special logic for double", {
  
  expect_identical(is.element(1.5:7, 5:10), base::is.element(1.5:7, 5:10))
  expect_identical(is.element(1.5:7, as.integer64(6:1)), rep(TRUE, 6))
  expect_identical(is.element(as.integer64(6:1), 1.5:7), rep(TRUE, 6))

})

test_that("is.element works (additional cases)", {
  
  expect_identical(is.element(c(5, 3, 5), c(0, 2, 1, 3, 6)), base::is.element(c(5, 3, 5), c(0, 2, 1, 3, 6)))
  expect_identical(is.element(as.integer64(c(5, 3, 5)), as.integer64(c(0, 2, 1, 3, 6))), c(FALSE, TRUE, FALSE))
  expect_identical(is.element(as.integer64(c(5, 3, 5)), as.integer64(c(3, 3, 3, 3, 5))), rep(TRUE, 3))
  
  expect_true(is.element(NA, NA_integer_))
  expect_true(is.element(NA_integer_, NA))
  expect_true(is.element(NA, NA_integer64_))
  expect_true(is.element(NA_integer64_, NA))
  
})
