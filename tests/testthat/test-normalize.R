test_that("normalize_name cleans punctuation, case, accents and suffixes", {
  expect_equal(normalize_name("O'Brien-Smith"), "OBRIEN SMITH")
  expect_equal(normalize_name("  José   María "), "JOSE MARIA")
  expect_equal(normalize_name("John Jr."), "JOHN")
  expect_equal(normalize_name("Smith Jr Sr"), "SMITH")
  expect_equal(normalize_name("Mrs. Ann"), "ANN")
  expect_equal(normalize_name(c("Mr.", "", NA)), c(NA_character_, NA, NA))
})

test_that("read_names reads every column as character", {
  path <- tempfile(fileext = ".csv")
  writeLines(c("id,zip", "007,02134"), path)
  d <- read_names(path)
  expect_equal(d$id, "007")
  expect_equal(d$zip, "02134")
})
