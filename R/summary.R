#' Summarize a name match
#'
#' Reports what each step contributed. It can be called after any step, and
#' it runs exact matching first if no step has run yet.
#'
#' @param obj An `nm_match` object.
#' @return A tibble of class `nm_summary` with one row per step:
#'   \describe{
#'     \item{step, verify}{The step and its verify mode.}
#'     \item{pool}{Rows of `x` still unmatched when the step ran.}
#'     \item{matched}{Rows the step matched.}
#'     \item{ambiguous}{Rows with several candidates the step could not tell
#'       apart.}
#'     \item{rejected}{Rows with a single candidate that the step rejected
#'       (failed verification or above the typo acceptance threshold).}
#'     \item{cum_matched, cum_pct}{Cumulative rows matched and share of all
#'       rows of `x`.}
#'     \item{`<var>_agree`}{For each `check_by` variable, the share of the
#'       step's matches where the variable agrees, among those where it is
#'       present on both sides.}
#'   }
#'   Overall totals are stored in the `"totals"` attribute (a named integer
#'   vector) and printed with the table: rows, matched, unmatched,
#'   `ambiguous_left` (unmatched rows that were ambiguous in some step),
#'   `held` (unmatched rows whose exact name appears several times in `y`)
#'   and `missing_name`.
#' @export
#' @examples
#' x <- data.frame(id = 1:3, first = c("Ann", "Jon", "Ann"),
#'                 last = c("Lee", "Smith", "Kim"), zip = c("1", "2", "3"))
#' y <- data.frame(id = 1:4, first = c("ANN", "JOHN", "ANN", "ANN"),
#'                 last = c("LEE", "SMITH", "KIM", "KIM"),
#'                 zip = c("1", "2", "3", "4"))
#' nm_setup(x, y, "first", "last", "id", check_by = "zip") |>
#'   match_soundex() |>
#'   nm_summary()
nm_summary <- function(obj) {
  obj <- ensure_exact(obj)
  n <- nrow(obj$x)
  check_vars <- obj$spec$check_by$var

  out <- obj$log
  out$cum_matched <- cumsum(out$matched)
  out$cum_pct <- out$cum_matched / max(n, 1)
  for (v in check_vars) {
    a <- obj$matches[[paste0("agree_", v)]]
    rate <- vapply(out$step, function(s) {
      ai <- a[obj$matches$step == s]
      if (all(is.na(ai))) NA_real_ else mean(ai, na.rm = TRUE)
    }, numeric(1))
    out[[paste0(v, "_agree")]] <- unname(rate)
  }

  matched <- unique(obj$matches$.x_row)
  missing_name <- obj$x$.x_row[is.na(obj$x$.first) | is.na(obj$x$.last)]
  unmatched <- setdiff(obj$x$.x_row, matched)
  attr(out, "totals") <- c(
    rows = n,
    matched = length(matched),
    unmatched = length(unmatched),
    ambiguous_left = length(intersect(unmatched, obj$ambiguous)),
    held = length(intersect(unmatched, obj$held)),
    missing_name = length(missing_name)
  )
  class(out) <- c("nm_summary", class(out))
  out
}

#' @export
print.nm_summary <- function(x, ...) {
  tot <- attr(x, "totals")
  f <- function(k) format(tot[[k]], big.mark = ",")
  cat("Matched ", f("matched"), " of ", f("rows"), " rows (",
      fmt_pct(tot[["matched"]] / max(tot[["rows"]], 1)), "); ",
      f("unmatched"), " unmatched, of which ", f("ambiguous_left"),
      " ambiguous and ", f("missing_name"), " missing a name.\n", sep = "")
  df <- as.data.frame(x)
  df$cum_pct <- fmt_pct(df$cum_pct)
  agree <- grep("_agree$", names(df), value = TRUE)
  for (a in agree) df[[a]] <- ifelse(is.na(df[[a]]), "-", fmt_pct(df[[a]]))
  print(df, row.names = FALSE, right = TRUE)
  invisible(x)
}
