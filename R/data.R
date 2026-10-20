#' Nickname groups
#'
#' Groups of first names that are treated as interchangeable by
#' [match_nickname()]. Every pair of names within a group is a candidate
#' pair (in both directions). Groups only contain names that are not shared
#' across genders, so ALEXANDER includes AL but not ALEX (which could be
#' ALEXANDRA).
#'
#' @format A tibble with one row per name in a group:
#' \describe{
#'   \item{group}{Integer group id.}
#'   \item{name}{Upper-case first name, already normalized.}
#' }
#' @source Hand-built list from the original `fuzzy_name_match.R` script.
"nicknames"
