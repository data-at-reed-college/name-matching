test_that("join-style specs map differently named columns", {
  x <- data.frame(id = 1, firstName = "Ann", lastName = "Lee", zip = "1")
  y <- data.frame(VOTER_ID = "v", FIRST_NAME = "ANN", LAST_NAME = "LEE", ZIP_CODE = "1")
  m <- nm_setup(x, y,
    first = c("firstName" = "FIRST_NAME"), last = c("lastName" = "LAST_NAME"),
    id = c("id" = "VOTER_ID"), check_by = c("zip" = "ZIP_CODE")
  )
  expect_s3_class(m, "nm_match")
  expect_equal(m$spec$check_by$var, "zip")
  expect_equal(nm_results(m)$VOTER_ID, "v")
})

test_that("unnamed specs use the same column in both tables", {
  x <- make_x("Ann", "Lee", zip = "1")
  y <- make_y("Ann", "Lee", zip = "1")
  m <- setup_xy(x, y, block_by = "zip")
  expect_equal(m$spec$block_by$y, "zip")
})

test_that("bad specs give clear errors", {
  x <- make_x("Ann", "Lee")
  y <- make_y("Ann", "Lee")
  expect_error(setup_xy(x, y, check_by = c("zip" = "ZIP")), "not found in `x`")
  expect_error(nm_setup(x, y, first = c("first" = "FIRST"), last = "last", id = c("id" = "rid")),
               "not found in `y`")
  expect_error(nm_setup(x, y, first = c("first", "last"), last = "last", id = c("id" = "rid")),
               "exactly one")
  expect_error(nm_setup(x, y, first = "first", last = "last"), "`id` is required")
  x$zip <- "1"; y$zip <- "1"
  expect_error(setup_xy(x, y, block_by = "zip", check_by = "zip"), "both")
})

test_that("ids must be unique and present", {
  x <- make_x(c("Ann", "Bob"), c("Lee", "Lee"))
  x$id <- c(1, 1)
  expect_error(setup_xy(x, make_y("Ann", "Lee")), "not unique")
  x$id <- c(1, NA)
  expect_error(setup_xy(x, make_y("Ann", "Lee")), "missing values")
})

test_that("missing_values and blanks count as missing", {
  x <- make_x(c("Ann", "Bob"), c("Lee", "Kim"), zip = c("97000", " "))
  y <- make_y(c("Ann", "Bob"), c("Lee", "Kim"), zip = c("97000", "2"))
  m <- setup_xy(x, y, check_by = "zip", missing_values = "97000")
  expect_true(all(is.na(m$x$.v_zip)))
  m <- setup_xy(x, y, check_by = "zip", missing_values = list(zip = "97000"))
  expect_true(all(is.na(m$x$.v_zip)))
  expect_error(setup_xy(x, y, check_by = "zip", missing_values = list(state = "x")), "named")
})
