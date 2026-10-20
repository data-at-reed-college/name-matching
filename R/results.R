#' Get the results of a name match
#'
#' Combines the matches with the original data. Matched rows get the columns
#' of `x` followed by the columns of the matched `y` row; `y` column names
#' that clash with `x` get the suffix `_ref`.
#'
#' @param obj An `nm_match` object.
#' @param which `"matched"` for matched rows only, `"unmatched"` for the rows
#'   of `x` without a match, or `"all"` for every row of `x` (unmatched rows
#'   have missing `y` columns).
#' @return A tibble. Besides the data columns it has `match_step` (the step
#'   that made the match, or `NA`), `n_candidates` (candidate rows after
#'   blocking), one `<var>_agree` column per `check_by` variable (`NA` when
#'   either value is missing) and, for unmatched rows, `unmatched_reason`:
#'   `"missing_name"`, `"ambiguous"` (several candidates the steps could not
#'   tell apart), `"rejected"` (a single candidate that failed verification
#'   or the typo threshold) or `"no_candidate"`.
#' @export
#' @examples
#' x <- data.frame(id = 1:2, first = c("Ann", "Zed"), last = c("Lee", "Q"))
#' y <- data.frame(id = "a", first = "ANN", last = "LEE")
#' m <- nm_setup(x, y, "first", "last", "id")
#' nm_results(m, "all")
nm_results <- function(obj, which = c("matched", "unmatched", "all")) {
  which <- match.arg(which)
  obj <- ensure_exact(obj)
  check_vars <- obj$spec$check_by$var
  n <- nrow(obj$x)

  m <- obj$matches
  y_idx <- rep(NA_integer_, n)
  y_idx[m$.x_row] <- m$.y_row
  info <- tibble::tibble(
    match_step = rep(NA_character_, n),
    n_candidates = rep(NA_integer_, n)
  )
  info$match_step[m$.x_row] <- m$step
  info$n_candidates[m$.x_row] <- m$n_candidates
  for (v in check_vars) {
    col <- paste0(v, "_agree")
    info[[col]] <- NA
    info[[col]][m$.x_row] <- m[[paste0("agree_", v)]]
  }
  rows <- seq_len(n)
  info$unmatched_reason <- dplyr::case_when(
    !is.na(y_idx) ~ NA_character_,
    is.na(obj$x$.first) | is.na(obj$x$.last) ~ "missing_name",
    rows %in% obj$ambiguous ~ "ambiguous",
    rows %in% c(obj$rejected, obj$held) ~ "rejected",
    TRUE ~ "no_candidate"
  )

  xd <- obj$x_data
  yd <- obj$y_data[y_idx, , drop = FALSE]
  clash <- names(yd) %in% c(names(xd), names(info))
  names(yd)[clash] <- paste0(names(yd)[clash], "_ref")

  out <- dplyr::bind_cols(xd, yd, info)
  keep <- switch(which,
    matched = !is.na(y_idx),
    unmatched = is.na(y_idx),
    all = rep(TRUE, n)
  )
  out <- out[keep, , drop = FALSE]
  if (which == "matched") out$unmatched_reason <- NULL
  if (which == "unmatched") {
    out <- out[c(names(xd), "unmatched_reason")]
  }
  out
}
