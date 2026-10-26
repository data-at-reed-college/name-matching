test_that("exact match accepts unique names only", {
  x <- make_x(c("Ann", "John", "Zed"), c("Lee", "Smith", "Q"))
  y <- make_y(c("ANN", "JOHN", "JOHN"), c("LEE", "SMITH", "SMITH"))
  m <- setup_xy(x, y) |> match_exact()
  expect_equal(unname(matched_to(m)), c("r1", NA, NA))
  expect_equal(nm_results(m, "unmatched")$unmatched_reason, c("ambiguous", "no_candidate"))
})

test_that("exact always runs first, even when skipped", {
  x <- make_x(c("Ann", "Bob"), c("Lee", "Kim"))
  y <- make_y(c("ANN", "ROBERT"), c("LEE", "KIM"))
  m <- setup_xy(x, y) |> match_nickname()
  expect_equal(m$log$step, c("exact", "nickname"))
  expect_equal(unname(matched_to(m)), c("r1", "r2"))
  expect_message(match_exact(m), "already run")
  # summary and results also run it
  m0 <- setup_xy(x, y)
  expect_equal(attr(nm_summary(m0), "totals")[["matched"]], 1)
  expect_equal(nrow(nm_results(m0)), 1)
})

test_that("ambiguous exact names are held back from fuzzy steps", {
  x <- make_x("John", "Smith")
  y <- make_y(c("JOHN", "JOHN", "JON"), c("SMITH", "SMITH", "SMITH"))
  m <- setup_xy(x, y) |> match_soundex() |> match_typo()
  expect_equal(nrow(m$matches), 0)
  expect_equal(attr(nm_summary(m), "totals")[["held"]], 1)
})

test_that("rows with a missing name are never matched", {
  x <- make_x(c("Ann", NA), c("Lee", "Lee"))
  y <- make_y("Ann", "Lee")
  s <- nm_summary(setup_xy(x, y))
  expect_equal(attr(s, "totals")[["missing_name"]], 1)
})
