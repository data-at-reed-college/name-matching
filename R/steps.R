#' Matching steps
#'
#' Each step takes an `nm_match` object (from [nm_setup()]), tries to match
#' the rows of `x` that are still unmatched, and returns the updated object,
#' so steps can be piped in any order and any of them can be skipped.
#' A row matched by one step is not considered again.
#'
#' Exact matching always runs first: every other step (and [nm_summary()]
#' and [nm_results()]) runs `match_exact()` with its defaults if it has not
#' run yet. Rows whose exact name appears in `y` but could not be resolved
#' to a single row (for example two voters called JOHN SMITH and no check
#' variable to tell them apart) are held back from the fuzzy steps, since
#' the right match is very likely one of those people.
#'
#' Every step accepts a match only when exactly one candidate row of `y`
#' remains after `block_by` filtering and `check_by` tie-breaking (see
#' [nm_setup()]).
#'
#' @section Steps:
#' * `match_exact()`: same normalized first and last name.
#' * `match_tokens()`: the same words in first + last combined, in any order
#'   or split (FRANKENSTEIN, BARON VON vs VON FRANKENSTEIN, BARON).
#'   Verified against check variables by default: many names work as either
#'   a first or a last name (CRAIG TAYLOR / TAYLOR CRAIG), and a swapped
#'   match is often a different person.
#' * `match_typo()`: first name within a Jaro-Winkler distance of the
#'   reference and last name exact, or the other way round (KATHRYN /
#'   KATHERYN). Only names sharing a first letter are compared.
#' * `match_soundex()`: first and last name both have the same Soundex code
#'   (SMITH / SMYTH). Very loose; verified against check variables by
#'   default, but see the warning below.
#' * `match_nickname()`: last name exact and first names in the same
#'   nickname group (BOB / ROBERT). See [nicknames].
#' * `match_first_word()`: last name exact and the first word of the first
#'   name agrees (LISA E / LISA, ANN ELOISE / ANN).
#' * `match_initial()`: last name exact and one side's first name is only
#'   initials, the first of which starts the other's first name
#'   (D FRANKENSTEIN / DRACULA FRANKENSTEIN, D J LEE / DAVID LEE). A name
#'   such as S RENEE is not treated as an initial.
#' * `match_compound_last()`: first name exact and one side's last-name words
#'   are a subset of the other's (GARCIA LOPEZ / GARCIA).
#'
#' @section Verification:
#' `verify` decides what a step does with `check_by` variables once it has
#' a single candidate:
#' * `"none"`: accept it.
#' * `"no_conflict"`: reject it if any check variable is present on both
#'   sides and disagrees.
#' * `"agree"`: accept it only if at least one check variable is present on
#'   both sides and agrees.
#'
#' Without `check_by` variables every mode accepts. The defaults are
#' `"none"` for the stricter steps (exact, typo, nickname) and
#' `"no_conflict"` for the looser ones (tokens, Soundex, first word, initial,
#' compound last name).
#'
#' @section Warning:
#' `match_soundex()` confuses relatives at the same address. Family members
#' share a last name and a zip code, and many different first names share a
#' Soundex code (MICHELLE / MICHAEL, DON / DEANNA, RUSSELL / RACHEL), so a zip
#' check cannot catch these false matches. On the 2022 example file roughly
#' 20 of 75 Soundex matches that agreed on zip were wrong. Review its matches
#' by hand (`nm_results(m)` filtered to `match_step == "soundex"`) before
#' using them.
#'
#' @param obj An `nm_match` object.
#' @param verify How to use `check_by` variables to verify a match; one of
#'   `"none"`, `"no_conflict"` or `"agree"`.
#' @return The updated `nm_match` object.
#' @name match_steps
#' @examples
#' donors <- data.frame(
#'   id = 1:5,
#'   first = c("Ann", "Bob", "D", "Maria", "Lisa E"),
#'   last = c("Lee", "Smith", "Frankenstein", "Garcia Lopez", "Wong")
#' )
#' voters <- data.frame(
#'   id = c("a", "b", "c", "d", "e"),
#'   first = c("ANN", "ROBERT", "DRACULA", "MARIA", "LISA"),
#'   last = c("LEE", "SMITH", "FRANKENSTEIN", "GARCIA", "WONG")
#' )
#' m <- nm_setup(donors, voters, first = "first", last = "last", id = "id") |>
#'   match_nickname() |>
#'   match_first_word() |>
#'   match_initial() |>
#'   match_compound_last()
#' nm_summary(m)
NULL

#' @rdname match_steps
#' @export
match_exact <- function(obj, verify = "none") {
  check_nm(obj)
  if ("exact" %in% obj$log$step) {
    rlang::inform("Exact matching has already run; skipping.")
    return(obj)
  }
  cands <- dplyr::inner_join(
    pool_names(obj), obj$y[c(".first", ".last", ".y_row")],
    by = c(".first", ".last")
  )
  run_step(obj, cands, "exact", verify, hold_unmatched = TRUE)
}

#' @rdname match_steps
#' @export
match_tokens <- function(obj, verify = "no_conflict") {
  obj <- ensure_exact(obj)
  dn <- pool_names(obj)
  dn$tok <- token_key(dn$.first, dn$.last)
  yt <- tibble::tibble(
    .y_row = obj$y$.y_row, y_first = obj$y$.first, y_last = obj$y$.last,
    tok = token_key(obj$y$.first, obj$y$.last)
  )
  cands <- dplyr::inner_join(dn, yt, by = "tok", relationship = "many-to-many")
  cands <- cands[cands$.first != cands$y_first | cands$.last != cands$y_last, ]
  run_step(obj, cands, "tokens", verify)
}

#' @rdname match_steps
#' @param jw_cutoff Jaro-Winkler similarity (1 - distance) a name pair needs
#'   to be a candidate. Candidates count towards uniqueness even when they
#'   are not close enough to be accepted.
#' @param jw_accept A candidate is accepted only if its Jaro-Winkler distance
#'   is below this.
#' @param min_first,min_last Minimum length of first and last names compared
#'   for typos; shorter names are too easy to confuse.
#' @export
match_typo <- function(obj, jw_cutoff = 0.9, jw_accept = 0.05, min_first = 4,
                       min_last = 5, verify = "none") {
  obj <- ensure_exact(obj)
  dn <- pool_names(obj)
  y <- obj$y
  max_dist <- 1 - jw_cutoff + 1e-9

  fp <- jw_pairs(dn$.first[nchar(dn$.first) >= min_first],
                 y$.first[nchar(y$.first) >= min_first], max_dist)
  lp <- jw_pairs(dn$.last[nchar(dn$.last) >= min_last],
                 y$.last[nchar(y$.last) >= min_last], max_dist)

  # first name typo, last name exact
  c1 <- dplyr::inner_join(dn, fp, by = c(".first" = "a"), relationship = "many-to-many")
  c1 <- dplyr::inner_join(c1, y, by = c("b" = ".first", ".last"), relationship = "many-to-many")
  # last name typo, first name exact
  c2 <- dplyr::inner_join(dn, lp, by = c(".last" = "a"), relationship = "many-to-many")
  c2 <- dplyr::inner_join(c2, y, by = c("b" = ".last", ".first"), relationship = "many-to-many")

  cands <- dplyr::bind_rows(c1, c2)
  cands$keep <- cands$dist < jw_accept
  run_step(obj, cands[c(".first", ".last", ".y_row", "keep")], "typo", verify)
}

#' @rdname match_steps
#' @export
match_soundex <- function(obj, verify = "no_conflict") {
  obj <- ensure_exact(obj)
  dn <- pool_names(obj)
  dn$sx_first <- soundex(dn$.first)
  dn$sx_last <- soundex(dn$.last)
  yt <- tibble::tibble(
    .y_row = obj$y$.y_row,
    sx_first = soundex(obj$y$.first), sx_last = soundex(obj$y$.last)
  )
  cands <- dplyr::inner_join(dn, yt, by = c("sx_first", "sx_last"),
                             relationship = "many-to-many")
  run_step(obj, cands, "soundex", verify)
}

#' @rdname match_steps
#' @param nicknames A data frame of nickname groups with columns `group` and
#'   `name` (normalized, upper case). Defaults to the built-in [nicknames].
#' @export
match_nickname <- function(obj, nicknames = namematch::nicknames, verify = "none") {
  obj <- ensure_exact(obj)
  if (!all(c("group", "name") %in% names(nicknames))) {
    rlang::abort("`nicknames` needs columns `group` and `name`.")
  }
  nk <- tibble::tibble(group = nicknames$group, name = normalize_name(nicknames$name))
  pairs <- dplyr::inner_join(nk, nk, by = "group", suffix = c("_a", "_b"),
                             relationship = "many-to-many")
  pairs <- dplyr::distinct(pairs[pairs$name_a != pairs$name_b, c("name_a", "name_b")])

  cands <- dplyr::inner_join(pool_names(obj), pairs, by = c(".first" = "name_a"),
                             relationship = "many-to-many")
  cands <- dplyr::inner_join(cands, obj$y, by = c("name_b" = ".first", ".last"),
                             relationship = "many-to-many")
  run_step(obj, cands, "nickname", verify)
}

#' @rdname match_steps
#' @export
match_first_word <- function(obj, verify = "no_conflict") {
  obj <- ensure_exact(obj)
  dn <- pool_names(obj)
  dn$first_w <- first_word(dn$.first)
  yt <- tibble::tibble(
    .y_row = obj$y$.y_row, y_first = obj$y$.first, .last = obj$y$.last,
    first_w = first_word(obj$y$.first)
  )
  cands <- dplyr::inner_join(dn, yt, by = c("first_w", ".last"),
                             relationship = "many-to-many")
  cands <- cands[cands$.first != cands$y_first, ]
  run_step(obj, cands, "first_word", verify)
}

#' @rdname match_steps
#' @export
match_initial <- function(obj, verify = "no_conflict") {
  obj <- ensure_exact(obj)
  dn <- pool_names(obj)
  dn$init <- substr(dn$.first, 1, 1)
  dn$x_only <- initials_only(dn$.first)
  yt <- tibble::tibble(
    .y_row = obj$y$.y_row, .last = obj$y$.last,
    init = substr(obj$y$.first, 1, 1), y_only = initials_only(obj$y$.first)
  )
  cands <- dplyr::inner_join(dn, yt, by = c("init", ".last"),
                             relationship = "many-to-many")
  # exactly one side gives only initials; "S RENEE" is not an initial match
  # for STEVEN, since RENEE is the name the person goes by
  cands <- cands[cands$x_only != cands$y_only, ]
  run_step(obj, cands, "initial", verify)
}

#' @rdname match_steps
#' @param min_token Words shorter than this are ignored when comparing
#'   compound last names.
#' @export
match_compound_last <- function(obj, min_token = 2, verify = "no_conflict") {
  obj <- ensure_exact(obj)
  dn <- pool_names(obj)
  xt <- explode_tokens(dn$.first, dn$.last, min_token)
  yt <- explode_tokens(obj$y$.first, obj$y$.last, min_token, id = obj$y$.y_row)

  pairs <- dplyr::inner_join(xt, yt, by = c("first", "token"),
                             suffix = c("_x", "_y"), relationship = "many-to-many")
  pairs <- dplyr::distinct(pairs, .data$first, .data$last_x, .data$last_y, .data$id)
  pairs <- pairs[pairs$last_x != pairs$last_y, ]

  tx <- token_sets(pairs$last_x, min_token)
  ty <- token_sets(pairs$last_y, min_token)
  subset <- mapply(function(a, b) all(a %in% b) || all(b %in% a), tx, ty)
  pairs <- pairs[subset, ]

  cands <- tibble::tibble(.first = pairs$first, .last = pairs$last_x, .y_row = pairs$id)
  run_step(obj, cands, "compound_last", verify)
}

# ---- helpers ----------------------------------------------------------------

token_key <- function(first, last) {
  vapply(strsplit(paste(first, last), " ", fixed = TRUE),
         function(t) paste(sort(t), collapse = " "), character(1))
}

first_word <- function(x) sub(" .*$", "", x)

initials_only <- function(x) grepl("^[A-Z0-9]( [A-Z0-9])*$", x)

soundex <- function(x) {
  if (length(x) == 0) return(character())
  stringdist::phonetic(gsub(" ", "", x, fixed = TRUE), method = "soundex")
}

token_sets <- function(x, min_token) {
  lapply(strsplit(x, " ", fixed = TRUE), function(t) unique(t[nchar(t) >= min_token]))
}

# One row per (name, last-name word) for names whose last name has a usable word.
explode_tokens <- function(first, last, min_token, id = NULL) {
  toks <- token_sets(last, min_token)
  n <- lengths(toks)
  out <- tibble::tibble(
    first = rep(first, n), last = rep(last, n), token = unlist(toks, use.names = FALSE)
  )
  if (!is.null(id)) out$id <- rep(id, n)
  out
}
