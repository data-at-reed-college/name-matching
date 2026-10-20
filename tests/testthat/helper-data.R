# Build a small x / y pair; extra columns are passed as vectors.
make_x <- function(first, last, ...) {
  data.frame(id = seq_along(first), first = first, last = last, ...,
             stringsAsFactors = FALSE)
}
make_y <- function(first, last, ...) {
  data.frame(rid = paste0("r", seq_along(first)), first = first, last = last, ...,
             stringsAsFactors = FALSE)
}
setup_xy <- function(x, y, ...) {
  nm_setup(x, y, first = "first", last = "last", id = c("id" = "rid"), ...)
}
matched_to <- function(obj) {
  r <- nm_results(obj, "all")
  stats::setNames(r$rid, r$id)
}
