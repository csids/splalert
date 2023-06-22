row_mean <- rowMeans

row_sd <- function(x, na.rm=F) {
  # Vectorised version of variance filter
  sqrt(rowSums((x - rowMeans(x, na.rm=na.rm))^2, na.rm=na.rm) / (ncol(x) - 1))
}
