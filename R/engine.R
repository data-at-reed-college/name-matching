# The acceptance routine shared by every step.
#
# `cands` holds name-level candidates: one row per (x name, y row) with
# columns .first, .last (the x name keys) and .y_row. An optional logical
# column `keep` marks candidates that count towards uniqueness and tie-breaking
# but may not be accepted (e.g. a typo above the acceptance threshold).
#
# For every pool row of x with the candidate name:
#   1. drop candidates that disagree on a block_by variable
#   2. one candidate left: take it; several: keep those agreeing on the most
#      check_by variables and take it if exactly one is left, else ambiguous
#   3. verify the chosen candidate ("none", "no_conflict", "agree")
# With `hold_unmatched = TRUE`, rows that had a candidate but were not
# accepted are held back from all later steps (used by exact matching).
run_step <- function(obj, cands, step, verify, hold_unmatched = FALSE) {
  verify <- match.arg(verify, c("none", "no_conflict", "agree"))
  step <- unique_step_name(obj, step)
  pool <- pool_rows(obj)

  if (!"keep" %in% names(cands)) cands$keep <- TRUE
  cands <- dplyr::summarise(
    cands,
    keep = any(.data$keep),
    .by = c(".first", ".last", ".y_row")
  )

  rows <- dplyr::inner_join(
    pool, cands,
    by = c(".first", ".last"), relationship = "many-to-many"
  )
  rows <- rows[, c(".x_row", ".y_row", "keep")]

  block_vars <- obj$spec$block_by$var
  check_vars <- obj$spec$check_by$var
  rows <- add_comparisons(rows, obj, c(block_vars, check_vars))

  for (v in block_vars) {
    clash <- rows[[paste0("agree_", v)]] %in% FALSE
    rows <- rows[!clash, ]
  }

  rows <- dplyr::mutate(rows, n_candidates = dplyr::n(), .by = ".x_row")
  if (length(check_vars) > 0) {
    agree_cols <- paste0("agree_", check_vars)
    rows$n_agree <- n_true(rows[agree_cols])
    rows <- dplyr::filter(rows, .data$n_agree == max(.data$n_agree), .by = ".x_row")
  } else {
    rows$n_agree <- 0L
  }
  rows <- dplyr::mutate(rows, n_left = dplyr::n(), .by = ".x_row")

  ambiguous <- unique(rows$.x_row[rows$n_left > 1])
  chosen <- rows[rows$n_left == 1, ]

  ok <- chosen$keep & verify_ok(chosen, check_vars, verify)
  accepted <- chosen[ok, ]
  rejected <- nrow(chosen) - nrow(accepted)

  new <- tibble::tibble(
    .x_row = accepted$.x_row,
    .y_row = accepted$.y_row,
    step = rep(step, nrow(accepted)),
    n_candidates = as.integer(accepted$n_candidates)
  )
  for (v in check_vars) new[[paste0("agree_", v)]] <- accepted[[paste0("agree_", v)]]

  if (hold_unmatched) {
    obj$held <- union(obj$held, setdiff(rows$.x_row, accepted$.x_row))
  }
  obj$matches <- dplyr::bind_rows(obj$matches, new)
  obj$ambiguous <- union(obj$ambiguous, ambiguous)
  obj$rejected <- union(obj$rejected, chosen$.x_row[!ok])
  obj$log <- dplyr::bind_rows(obj$log, tibble::tibble(
    step = step, verify = verify, pool = nrow(pool),
    matched = nrow(accepted), ambiguous = length(ambiguous),
    rejected = as.integer(rejected)
  ))
  obj
}

# Add agree_<var> columns: TRUE / FALSE when both values are present, else NA.
add_comparisons <- function(rows, obj, vars) {
  for (v in vars) {
    col <- extra_col(v)
    xv <- obj$x[[col]][rows$.x_row]
    yv <- obj$y[[col]][match(rows$.y_row, obj$y$.y_row)]
    rows[[paste0("agree_", v)]] <- ifelse(is.na(xv) | is.na(yv), NA, xv == yv)
  }
  rows
}

# With no check variables there is nothing to verify against, so every
# verify mode accepts.
verify_ok <- function(chosen, check_vars, verify) {
  if (verify == "none" || length(check_vars) == 0) {
    return(rep(TRUE, nrow(chosen)))
  }
  agree <- chosen[paste0("agree_", check_vars)]
  if (verify == "no_conflict") {
    return(n_true(!agree) == 0)
  }
  n_true(agree) > 0
}

# Per row, the number of TRUE values across logical columns (NA counts as not TRUE).
n_true <- function(df) {
  m <- as.matrix(df)
  as.integer(rowSums(!is.na(m) & m))
}

# Rows of x still available: not matched, not held, with both names present.
pool_rows <- function(obj) {
  done <- c(obj$matches$.x_row, obj$held)
  p <- obj$x[!is.na(obj$x$.first) & !is.na(obj$x$.last), c(".x_row", ".first", ".last")]
  p[!p$.x_row %in% done, ]
}

# Distinct x names still in the pool.
pool_names <- function(obj) {
  dplyr::distinct(pool_rows(obj), .data$.first, .data$.last)
}

unique_step_name <- function(obj, step) {
  if (!step %in% obj$log$step) return(step)
  i <- 2
  while (paste0(step, "_", i) %in% obj$log$step) i <- i + 1
  paste0(step, "_", i)
}

check_nm <- function(obj) {
  if (!inherits(obj, "nm_match")) {
    rlang::abort("Expected an `nm_match` object; create one with `nm_setup()`.")
  }
}

ensure_exact <- function(obj) {
  check_nm(obj)
  if (!"exact" %in% obj$log$step) obj <- match_exact(obj)
  obj
}
