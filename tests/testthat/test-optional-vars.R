test_that("check_by breaks ties between exact candidates", {
  x <- make_x(c("John", "John"), c("Smith", "Smith"), zip = c("2", "9"))
  y <- make_y(c("JOHN", "JOHN"), c("SMITH", "SMITH"), zip = c("1", "2"))
  m <- setup_xy(x, y, check_by = "zip") |> match_exact()
  expect_equal(unname(matched_to(m)), c("r2", NA))
  expect_equal(m$matches$n_candidates, 2L)
  expect_true(m$matches$agree_zip)
})

test_that("a tie on check variables stays ambiguous", {
  x <- make_x("John", "Smith", zip = "5")
  y <- make_y(c("JOHN", "JOHN"), c("SMITH", "SMITH"), zip = c("1", "2"))
  m <- setup_xy(x, y, check_by = "zip")
  expect_equal(nm_results(m, "unmatched")$unmatched_reason, "ambiguous")
})

test_that("placeholder values do not break ties", {
  x <- make_x("John", "Smith", zip = "97000")
  y <- make_y(c("JOHN", "JOHN"), c("SMITH", "SMITH"), zip = c("97000", "2"))
  m <- setup_xy(x, y, check_by = "zip", missing_values = "97000")
  expect_equal(nrow(m |> nm_results()), 0)
  m <- setup_xy(x, y, check_by = "zip")
  expect_equal(unname(matched_to(m)), "r1")
})

test_that("block_by removes candidates that disagree", {
  x <- make_x(c("Ann", "Ann", "Ann"), c("Lee", "Lee", "Lee"), st = c("OR", "WA", NA))
  y <- make_y(c("ANN", "ANN"), c("LEE", "LEE"), st = c("OR", "CA"))
  m <- setup_xy(x, y, block_by = "st") |> match_exact()
  # OR matches r1; WA has no candidates left; NA keeps both and is ambiguous
  expect_equal(unname(matched_to(m)), c("r1", NA, NA))
  un <- nm_results(m, "unmatched")
  expect_equal(un$unmatched_reason, c("no_candidate", "ambiguous"))
})

test_that("block_by applies to fuzzy steps", {
  x <- make_x("Bob", "Kim", st = "OR")
  y <- make_y("ROBERT", "KIM", st = "WA")
  m <- setup_xy(x, y, block_by = "st") |> match_nickname()
  expect_equal(nrow(m$matches), 0)
})

test_that("verify modes", {
  x <- make_x(c("Bob", "Bob", "Bob"), c("Kim", "Lee", "Wu"), zip = c("1", "1", NA))
  y <- make_y(c("ROBERT", "ROBERT", "ROBERT"), c("KIM", "LEE", "WU"), zip = c("1", "2", "3"))
  none <- setup_xy(x, y, check_by = "zip") |> match_nickname(verify = "none")
  nc <- setup_xy(x, y, check_by = "zip") |> match_nickname(verify = "no_conflict")
  ag <- setup_xy(x, y, check_by = "zip") |> match_nickname(verify = "agree")
  expect_equal(unname(matched_to(none)), c("r1", "r2", "r3"))
  expect_equal(unname(matched_to(nc)), c("r1", NA, "r3"))
  expect_equal(unname(matched_to(ag)), c("r1", NA, NA))
  expect_equal(nc$log$rejected[2], 1L)
  expect_equal(nm_results(nc, "unmatched")$unmatched_reason, "rejected")
  expect_error(match_nickname(none, verify = "maybe"))
})

test_that("verify is a no-op without check variables", {
  x <- make_x("Jon", "Smyth")
  y <- make_y("JOHN", "SMITH")
  m <- setup_xy(x, y) |> match_soundex(verify = "agree")
  expect_equal(unname(matched_to(m)), "r1")
})
