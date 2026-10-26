# All pairs (a, b) of distinct names with Jaro-Winkler distance <= max_dist.
# Only names sharing a first letter are compared, and the distance matrix is
# built in chunks so large blocks stay within memory.
jw_pairs <- function(a, b, max_dist, chunk = 1000) {
  a <- unique(a[!is.na(a)])
  b <- unique(b[!is.na(b)])
  out <- list()
  for (l in intersect(unique(substr(a, 1, 1)), unique(substr(b, 1, 1)))) {
    al <- a[substr(a, 1, 1) == l]
    bl <- b[substr(b, 1, 1) == l]
    for (s in seq(1, length(al), by = chunk)) {
      ai <- al[s:min(s + chunk - 1, length(al))]
      d <- stringdist::stringdistmatrix(ai, bl, method = "jw", p = 0.1)
      hit <- which(d <= max_dist & d > 0, arr.ind = TRUE)
      if (nrow(hit) > 0) {
        out[[length(out) + 1]] <- tibble::tibble(
          a = ai[hit[, 1]], b = bl[hit[, 2]], dist = d[hit]
        )
      }
    }
  }
  if (length(out) == 0) {
    return(tibble::tibble(a = character(), b = character(), dist = numeric()))
  }
  dplyr::bind_rows(out)
}
