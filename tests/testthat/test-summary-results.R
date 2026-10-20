test_that("summary counts per step and cumulatively", {
  x <- make_x(c("Ann", "Bob", "Jon", "Zed", "Ann"), c("Lee", "Kim", "Smyth", "Q", NA),
              zip = c("1", "2", "9", "4", "5"))
  y <- make_y(c("ANN", "ROBERT", "JOHN"), c("LEE", "KIM", "SMITH"), zip = c("1", "3", "3"))
  s <- setup_xy(x, y, check_by = "zip") |>
    match_nickname() |>
    match_soundex(verify = "none") |>
    nm_summary()
  expect_s3_class(s, "nm_summary")
  expect_equal(s$step, c("exact", "nickname", "soundex"))
  expect_equal(s$matched, c(1L, 1L, 1L))
  expect_equal(s$cum_matched, c(1L, 2L, 3L))
  expect_equal(s$cum_pct, c(1, 2, 3) / 5)
  expect_equal(s$pool, c(4L, 3L, 2L))
  expect_equal(s$zip_agree, c(1, 0, 0))
  tot <- attr(s, "totals")
  expect_equal(tot[["matched"]], 3)
  expect_equal(tot[["unmatched"]], 2)
  expect_equal(tot[["missing_name"]], 1)
  expect_output(print(s), "Matched 3 of 5 rows")
})

test_that("agreement rate is NA when nothing can be compared", {
  x <- make_x("Ann", "Lee", zip = NA)
  y <- make_y("ANN", "LEE", zip = "1")
  s <- nm_summary(setup_xy(x, y, check_by = "zip"))
  expect_true(is.na(s$zip_agree))
})

test_that("results join both tables and suffix clashing columns", {
  x <- make_x(c("Ann", "Zed"), c("Lee", "Q"), zip = c("1", "2"))
  y <- make_y("ANN", "LEE", zip = "1", party = "DEM")
  m <- setup_xy(x, y, check_by = "zip")
  r <- nm_results(m)
  expect_equal(names(r), c("id", "first", "last", "zip", "rid", "first_ref", "last_ref",
                           "zip_ref", "party", "match_step", "n_candidates", "zip_agree"))
  expect_equal(r$match_step, "exact")
  expect_true(r$zip_agree)
  u <- nm_results(m, "unmatched")
  expect_equal(names(u), c("id", "first", "last", "zip", "unmatched_reason"))
  a <- nm_results(m, "all")
  expect_equal(nrow(a), 2)
  expect_equal(a$party, c("DEM", NA))
  expect_equal(a$unmatched_reason, c(NA, "no_candidate"))
})

test_that("print method summarizes the object", {
  m <- setup_xy(make_x("Ann", "Lee"), make_y("ANN", "LEE")) |> match_exact()
  expect_output(print(m), "steps run:     exact")
})
