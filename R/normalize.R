#' Normalize names for matching
#'
#' Converts names to a canonical form before matching: accents are
#' transliterated to ASCII, letters are upper-cased, hyphens become spaces,
#' other punctuation is removed (O'BRIEN becomes OBRIEN), suffixes and titles
#' (JR, SR, II, III, IV, DR, MR, MRS, MS) are dropped and white space is
#' squished. Empty results become `NA`.
#'
#' @param x A character vector of names.
#' @return A character vector the same length as `x`.
#' @export
#' @examples
#' normalize_name(c("O'Brien-Smith Jr.", "  José ", "Mrs."))
normalize_name <- function(x) {
  suffix <- "(^| )(JR|SR|II|III|IV|DR|MR|MRS|MS)( |$)"
  x <- stringi::stri_trans_general(as.character(x), "Latin-ASCII")
  x <- toupper(x)
  x <- gsub("-", " ", x, fixed = TRUE)               # hyphens act as spaces
  x <- gsub("[^A-Z0-9 ]", "", x)                     # drop punctuation
  x <- gsub(suffix, " ", x)                          # remove suffixes
  x <- gsub(suffix, " ", x)                          # twice for adjacent tokens
  x <- trimws(gsub("[[:space:]]+", " ", x))
  x[!is.na(x) & x == ""] <- NA_character_
  x
}
