#' Read a file of names
#'
#' A thin wrapper around [readr::read_csv()] that reads every column as
#' character, so ids and zip codes keep their leading zeros and are compared
#' as text.
#'
#' @param path Path to a delimited file.
#' @param ... Passed on to [readr::read_csv()].
#' @return A tibble.
#' @export
#' @examples
#' path <- tempfile(fileext = ".csv")
#' writeLines(c("id,first,last,zip", "1,Ann,Lee,02134"), path)
#' read_names(path)
read_names <- function(path, ...) {
  readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    progress = FALSE,
    ...
  )
}
