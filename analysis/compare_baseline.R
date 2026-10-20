# Compare namematch to the original fuzzy_name_match.R on the example files.
# Run from the repo root after installing or loading the package:
#   Rscript analysis/compare_baseline.R
#
# legacy:      the original six steps in the original order, names only.
#              This reproduces fuzzy_name_match.R (2022: 9,139 matched, 7,803 exact).
# legacy_zip:  the same steps with zip as a check variable used only for
#              tie-breaking (every step verify = "none").
# recommended: zip check, placeholder zips as missing, default verify per step,
#              Soundex last, plus the new initial and compound_last steps.

if (requireNamespace("devtools", quietly = TRUE) && file.exists("DESCRIPTION")) {
  devtools::load_all(".", quiet = TRUE)
} else {
  library(namematch)
}

registry <- read_names("voter_registry.csv")
placeholders <- c("97000", "97200", "00000")

run <- function(x, config) {
  zip <- if (config == "legacy") NULL else c("zip" = "ZIP_CODE")
  m <- nm_setup(x, registry,
    first = c("firstName" = "FIRST_NAME"),
    last = c("lastName" = "LAST_NAME"),
    id = c("id" = "VOTER_ID"),
    check_by = zip,
    missing_values = placeholders
  )
  if (config == "recommended") {
    m |>
      match_exact() |>
      match_tokens() |>
      match_nickname() |>
      match_typo() |>
      match_first_word() |>
      match_initial() |>
      match_compound_last() |>
      match_soundex()
  } else {
    m |>
      match_exact() |>
      match_tokens() |>
      match_typo() |>
      match_soundex(verify = "none") |>
      match_nickname() |>
      match_first_word(verify = "none")
  }
}

# Zip agreement of a set of matches, among rows with a usable zip on both sides
zip_rate <- function(x, m, steps = NULL) {
  r <- nm_results(m)
  if (!is.null(steps)) r <- r[r$match_step %in% steps, ]
  zx <- ifelse(r$zip %in% placeholders, NA, r$zip)
  ok <- !is.na(zx) & !is.na(r$ZIP_CODE)
  if (!any(ok)) return(NA_real_)
  mean(zx[ok] == r$ZIP_CODE[ok])
}

rows <- list()
details <- list()
for (year in c(2020, 2022, 2024)) {
  x <- read_names(sprintf("example_%d.csv", year))
  for (config in c("legacy", "legacy_zip", "recommended")) {
    m <- run(x, config)
    s <- nm_summary(m)
    tot <- attr(s, "totals")
    rows[[length(rows) + 1]] <- data.frame(
      year = year, config = config, rows = tot[["rows"]],
      matched = tot[["matched"]],
      pct = round(100 * tot[["matched"]] / tot[["rows"]], 1),
      exact = s$matched[s$step == "exact"],
      ambiguous_left = tot[["ambiguous_left"]],
      zip_agree_all = round(100 * zip_rate(x, m), 1),
      zip_agree_exact = round(100 * zip_rate(x, m, "exact"), 1),
      zip_agree_fuzzy = round(100 * zip_rate(x, m, setdiff(s$step, "exact")), 1),
      soundex = s$matched[s$step == "soundex"],
      zip_agree_soundex = round(100 * zip_rate(x, m, "soundex"), 1)
    )
    details[[paste(year, config)]] <- s
  }
}
comparison <- do.call(rbind, rows)

options(width = 200)
print(comparison, row.names = FALSE)
for (k in names(details)) {
  cat("\n==", k, "==\n")
  print(details[[k]])
}

utils::write.csv(comparison, "analysis/comparison.csv", row.names = FALSE)
