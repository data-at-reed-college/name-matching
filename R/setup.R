#' Set up a name match
#'
#' Creates the matching object that every `match_*()` step takes and returns.
#' Rows of `x` are matched to at most one row of `y`. Columns are mapped in
#' the style of a dplyr join: `c("x_column" = "y_column")`, or a single
#' unnamed string when the column has the same name in both tables.
#'
#' Names are normalized with [normalize_name()]. Rows of `x` with a missing
#' first or last name are never matched; rows of `y` with a missing first or
#' last name are dropped from the reference.
#'
#' @section Optional variables:
#' Any other columns shared by the two tables can be given in one of two
#' roles:
#'
#' * `block_by`: the values must agree whenever both are present. A
#'   candidate whose value differs is never considered. Use this for
#'   variables that are reliable, such as state.
#' * `check_by`: the values are compared but not required. When a step
#'   finds several candidates for a row, the candidates that agree on the
#'   most check variables win (a tie that cannot be broken leaves the row
#'   unmatched). Steps can also use them to verify a match (see the
#'   `verify` argument of the steps), and [nm_summary()] reports how often
#'   they agree.
#'
#' Values are compared as upper-case, trimmed text. Empty strings and the
#' values in `missing_values` count as missing, which never blocks, breaks
#' a tie or fails verification.
#'
#' @param x Data frame of names to be matched.
#' @param y Data frame of reference names to match against.
#' @param first,last,id Column mappings for first name, last name and a
#'   unique row id, e.g. `first = c("firstName" = "FIRST_NAME")`.
#' @param block_by,check_by Optional column mappings for shared variables
#'   (see below), e.g. `check_by = c("zip" = "ZIP_CODE")`. Several variables
#'   can be given at once. The variable is labelled by its name in `x`.
#' @param missing_values Values to treat as missing in `block_by` and
#'   `check_by` variables, such as placeholder zip codes. Either a character
#'   vector applied to every variable, or a named list with one character
#'   vector per variable (named by the `x` column).
#' @return An object of class `nm_match`.
#' @seealso [match_exact()] and the other steps, [nm_summary()],
#'   [nm_results()].
#' @export
#' @examples
#' donors <- data.frame(
#'   id = 1:3,
#'   firstName = c("Ann", "Bob", "Cara"),
#'   lastName = c("Lee", "Smith", "Diaz"),
#'   zip = c("97202", "97214", "97000")
#' )
#' voters <- data.frame(
#'   VOTER_ID = c("a", "b", "c", "d"),
#'   FIRST_NAME = c("ANN", "ROBERT", "CARA", "CARA"),
#'   LAST_NAME = c("LEE", "SMITH", "DIAZ", "DIAZ"),
#'   ZIP_CODE = c("97202", "97214", "97211", "97202")
#' )
#' m <- nm_setup(donors, voters,
#'   first = c("firstName" = "FIRST_NAME"),
#'   last = c("lastName" = "LAST_NAME"),
#'   id = c("id" = "VOTER_ID"),
#'   check_by = c("zip" = "ZIP_CODE"),
#'   missing_values = "97000"
#' )
#' m
nm_setup <- function(x, y, first, last, id, block_by = NULL, check_by = NULL,
                     missing_values = character()) {
  if (!is.data.frame(x)) rlang::abort("`x` must be a data frame.")
  if (!is.data.frame(y)) rlang::abort("`y` must be a data frame.")

  first <- parse_spec(first, "first", x, y, single = TRUE)
  last <- parse_spec(last, "last", x, y, single = TRUE)
  id <- parse_spec(id, "id", x, y, single = TRUE)
  block_by <- parse_spec(block_by, "block_by", x, y)
  check_by <- parse_spec(check_by, "check_by", x, y)

  both <- intersect(block_by$var, check_by$var)
  if (length(both) > 0) {
    rlang::abort(paste0(
      "A variable cannot be in both `block_by` and `check_by`: ",
      paste(both, collapse = ", "), "."
    ))
  }
  extras <- rbind(block_by, check_by)
  missing_values <- parse_missing(missing_values, extras$var)

  x_id <- as.character(x[[id$x]])
  y_id <- as.character(y[[id$y]])
  check_ids(x_id, "x", id$x)
  check_ids(y_id, "y", id$y)

  xk <- tibble::tibble(
    .x_row = seq_len(nrow(x)),
    .first = normalize_name(x[[first$x]]),
    .last = normalize_name(x[[last$x]])
  )
  yk <- tibble::tibble(
    .y_row = seq_len(nrow(y)),
    .first = normalize_name(y[[first$y]]),
    .last = normalize_name(y[[last$y]])
  )
  for (i in seq_len(nrow(extras))) {
    v <- extras$var[i]
    xk[[extra_col(v)]] <- clean_extra(x[[extras$x[i]]], missing_values[[v]])
    yk[[extra_col(v)]] <- clean_extra(y[[extras$y[i]]], missing_values[[v]])
  }
  yk <- yk[!is.na(yk$.first) & !is.na(yk$.last), ]

  structure(
    list(
      x = xk,
      y = yk,
      x_data = tibble::as_tibble(x),
      y_data = tibble::as_tibble(y),
      spec = list(
        first = first, last = last, id = id,
        block_by = block_by, check_by = check_by,
        missing_values = missing_values
      ),
      matches = empty_matches(check_by$var),
      ambiguous = integer(),
      rejected = integer(),
      held = integer(),
      log = empty_log()
    ),
    class = "nm_match"
  )
}

# Turn a join-style spec into a data frame with columns var, x, y.
parse_spec <- function(spec, arg, x, y, single = FALSE) {
  if (is.null(spec)) {
    if (single) rlang::abort(paste0("`", arg, "` is required."))
    return(data.frame(var = character(), x = character(), y = character()))
  }
  if (!is.character(spec) || length(spec) == 0 || anyNA(spec)) {
    rlang::abort(paste0(
      "`", arg, "` must be a character vector such as ",
      'c("x_column" = "y_column").'
    ))
  }
  if (single && length(spec) != 1) {
    rlang::abort(paste0("`", arg, "` must map exactly one column."))
  }
  x_cols <- names(spec)
  if (is.null(x_cols)) x_cols <- rep("", length(spec))
  x_cols[x_cols == ""] <- spec[x_cols == ""]
  y_cols <- unname(spec)

  bad_x <- setdiff(x_cols, names(x))
  bad_y <- setdiff(y_cols, names(y))
  if (length(bad_x) > 0) {
    rlang::abort(paste0(
      "`", arg, "`: column(s) not found in `x`: ",
      paste(bad_x, collapse = ", "), "."
    ))
  }
  if (length(bad_y) > 0) {
    rlang::abort(paste0(
      "`", arg, "`: column(s) not found in `y`: ",
      paste(bad_y, collapse = ", "), "."
    ))
  }
  if (anyDuplicated(x_cols)) {
    rlang::abort(paste0("`", arg, "` maps the same `x` column twice."))
  }
  data.frame(var = x_cols, x = x_cols, y = y_cols)
}

parse_missing <- function(missing_values, vars) {
  if (is.list(missing_values)) {
    unknown <- setdiff(names(missing_values), vars)
    if (is.null(names(missing_values)) || any(names(missing_values) == "") ||
        length(unknown) > 0) {
      rlang::abort(paste0(
        "A list `missing_values` must be named by `block_by` or `check_by` ",
        "variables."
      ))
    }
    out <- lapply(vars, function(v) as.character(missing_values[[v]]))
  } else {
    out <- lapply(vars, function(v) as.character(missing_values))
  }
  stats::setNames(out, vars)
}

check_ids <- function(ids, arg, col) {
  if (anyNA(ids)) {
    rlang::abort(paste0("Id column `", col, "` in `", arg, "` has missing values."))
  }
  if (anyDuplicated(ids)) {
    rlang::abort(paste0("Id column `", col, "` in `", arg, "` is not unique."))
  }
}

clean_extra <- function(v, missing) {
  v <- toupper(trimws(as.character(v)))
  v[!is.na(v) & (v == "" | v %in% toupper(trimws(missing)))] <- NA_character_
  v
}

extra_col <- function(var) paste0(".v_", var)

empty_matches <- function(check_vars) {
  out <- tibble::tibble(
    .x_row = integer(), .y_row = integer(),
    step = character(), n_candidates = integer()
  )
  for (v in check_vars) out[[paste0("agree_", v)]] <- logical()
  out
}

empty_log <- function() {
  tibble::tibble(
    step = character(), verify = character(), pool = integer(),
    matched = integer(), ambiguous = integer(), rejected = integer()
  )
}

#' @export
print.nm_match <- function(x, ...) {
  n <- nrow(x$x)
  cat("<nm_match>\n")
  cat("  x rows:        ", format(n, big.mark = ","), "\n", sep = "")
  cat("  y rows:        ", format(nrow(x$y), big.mark = ","), "\n", sep = "")
  if (nrow(x$spec$block_by) > 0) {
    cat("  block_by:      ", paste(x$spec$block_by$var, collapse = ", "), "\n", sep = "")
  }
  if (nrow(x$spec$check_by) > 0) {
    cat("  check_by:      ", paste(x$spec$check_by$var, collapse = ", "), "\n", sep = "")
  }
  steps <- if (nrow(x$log) > 0) paste(x$log$step, collapse = " -> ") else "(none yet)"
  cat("  steps run:     ", steps, "\n", sep = "")
  cat("  matched:       ", format(nrow(x$matches), big.mark = ","),
      " (", fmt_pct(nrow(x$matches) / max(n, 1)), ")\n", sep = "")
  invisible(x)
}

fmt_pct <- function(p) paste0(formatC(100 * p, format = "f", digits = 1), "%")
