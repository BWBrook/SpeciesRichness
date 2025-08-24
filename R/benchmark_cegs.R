# Benchmark utilities for CEGS models (internal)

#' Scale-invariant top-k fingerprint
#' @keywords internal
topk_fingerprint <- function(counts, k = 10L) {
  counts <- as.integer(counts)
  S <- length(counts); N <- sum(counts)
  if (S == 0L || N == 0L) return(rep(NA_real_, k))
  v <- sort(counts, decreasing = TRUE)
  v <- utils::head(v, k)
  v * (S / N)
}

#' Euclidean nearest neighbour (on complete rows only)
#' @keywords internal
nearest_neighbour <- function(M) {
  # M: matrix rows = inventories, cols = k; rownames = sample_id
  ok <- rowSums(is.na(M)) == 0
  ids <- rownames(M)[ok]
  nn <- stats::setNames(rep(NA_character_, length(ids)), ids)
  if (length(ids) < 2L) return(nn)
  D <- as.matrix(stats::dist(M[ok, , drop = FALSE], method = "euclidean"))
  diag(D) <- Inf
  for (i in seq_along(ids)) nn[ids[i]] <- ids[which.min(D[i, ])]
  nn
}

#' Build tail frequency-of-frequencies for a neighbour j
#' @keywords internal
tail_histogram <- function(counts, k = 10L, max_x = 4096L) {
  counts <- as.integer(counts)
  rare <- sort(counts, decreasing = TRUE)
  rare <- if (length(rare) > k) rare[-seq_len(k)] else integer()
  rare <- rare[rare > 0L & rare <= max_x]
  if (!length(rare)) return(list(u = integer(), s = integer(max_x)))
  t <- tabulate(rare, nbins = max_x)
  list(u = which(t > 0L), s = t)
}

#' Negative log-likelihood of neighbour tail under fitted SAD p (conditional on presence)
#' @keywords internal
tail_nll <- function(s, u, p) {
  u <- u[u <= length(p)]
  if (!length(u)) return(NA_real_)
  -sum(s[u] * log(p[u]))
}
