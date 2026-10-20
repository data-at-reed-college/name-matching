test_that("tokens matches swapped and split names", {
  x <- make_x(c("Frankenstein", "Baron Von"), c("Baron Von", "Frankenstein"))
  y <- make_y("Baron", "Von Frankenstein")
  m <- setup_xy(x, y) |> match_tokens()
  expect_equal(unname(matched_to(m)), c("r1", "r1"))
  expect_equal(m$matches$step, c("tokens", "tokens"))
})

test_that("typo matches close first or last names only", {
  x <- make_x(c("Katheryn", "Ann", "Jo"), c("Smith", "Frankenstien", "Smith"))
  y <- make_y(c("KATHRYN", "ANN", "JOE"), c("SMITH", "FRANKENSTEIN", "SMITH"))
  m <- setup_xy(x, y) |> match_typo()
  # JO is shorter than min_first, so it is not compared
  expect_equal(unname(matched_to(m)), c("r1", "r2", NA))
})

test_that("typo candidates above jw_accept count but are not accepted", {
  x <- make_x("Katheryn", "Smith")
  y <- make_y("KATHRYN", "SMITH")
  m <- setup_xy(x, y) |> match_typo(jw_accept = 0.001)
  expect_equal(nrow(m$matches), 0)
  expect_equal(m$log$rejected[2], 1L)
})

test_that("typo with two close candidates is ambiguous", {
  x <- make_x("Jonathon", "Smith")
  y <- make_y(c("JONATHAN", "JONATHEN"), c("SMITH", "SMITH"))
  m <- setup_xy(x, y) |> match_typo()
  expect_equal(nrow(m$matches), 0)
  expect_equal(m$log$ambiguous[2], 1L)
})

test_that("soundex matches names that sound alike", {
  x <- make_x("Jon", "Smyth")
  y <- make_y("JOHN", "SMITH")
  m <- setup_xy(x, y) |> match_soundex()
  expect_equal(unname(matched_to(m)), "r1")
})

test_that("nickname uses the built-in or a custom table", {
  x <- make_x(c("Bob", "Peggy", "Zed"), c("Kim", "Lee", "Wu"))
  y <- make_y(c("ROBERT", "MARGARET", "ZEDEKIAH"), c("KIM", "LEE", "WU"))
  m <- setup_xy(x, y) |> match_nickname()
  expect_equal(unname(matched_to(m)), c("r1", "r2", NA))
  custom <- data.frame(group = 1, name = c("Zedekiah", "Zed"))
  m <- setup_xy(x, y) |> match_nickname(nicknames = custom)
  expect_equal(unname(matched_to(m)), c(NA, NA, "r3"))
  expect_error(match_nickname(m, nicknames = data.frame(a = 1)), "group")
})

test_that("first_word drops middle names on either side", {
  x <- make_x(c("Lisa E", "Ann", "Laust Johan"), c("Wong", "Kim", "Berg"))
  y <- make_y(c("LISA", "ANN ELOISE", "LAUST"), c("WONG", "KIM", "BERG"))
  m <- setup_xy(x, y) |> match_first_word()
  expect_equal(unname(matched_to(m)), c("r1", "r2", "r3"))
})

test_that("initial matches a lone initial to a full first name", {
  x <- make_x(c("D", "Dracula", "D J", "S Renee"), c("Frankenstein", "Lee", "Wu", "Mitchell"))
  y <- make_y(c("DRACULA", "D", "DAVID", "STEVEN"), c("FRANKENSTEIN", "LEE", "WU", "MITCHELL"))
  m <- setup_xy(x, y) |> match_initial()
  # S RENEE is not an initial match for STEVEN
  expect_equal(unname(matched_to(m)), c("r1", "r2", "r3", NA))
})

test_that("initial is ambiguous when several first names share the letter", {
  x <- make_x("D", "Frankenstein")
  y <- make_y(c("DRACULA", "DAVID"), c("FRANKENSTEIN", "FRANKENSTEIN"))
  m <- setup_xy(x, y) |> match_initial()
  expect_equal(nrow(m$matches), 0)
})

test_that("compound_last matches partial compound last names", {
  x <- make_x(c("Maria", "Maria", "Ann", "Ann"), c("Garcia Lopez", "Lopez", "Garcia", "Ruiz Diaz"))
  y <- make_y(c("MARIA", "ANN", "ANN"), c("GARCIA LOPEZ", "GARCIA-LOPEZ", "RUIZ PEREZ"))
  m <- setup_xy(x, y) |> match_compound_last()
  # Garcia Lopez is exact; RUIZ DIAZ vs RUIZ PEREZ is not a subset
  expect_equal(unname(matched_to(m)), c("r1", "r1", "r2", NA))
  expect_equal(m$matches$step, c("exact", "compound_last", "compound_last"))
})

test_that("steps run in any order and a repeated step gets a suffix", {
  x <- make_x(c("Bob", "Jon"), c("Kim", "Smyth"))
  y <- make_y(c("ROBERT", "JOHN"), c("KIM", "SMITH"))
  m <- setup_xy(x, y) |> match_soundex() |> match_nickname() |> match_soundex()
  expect_equal(m$log$step, c("exact", "soundex", "nickname", "soundex_2"))
})

test_that("steps reject objects that are not nm_match", {
  expect_error(match_tokens(data.frame()), "nm_setup")
})
