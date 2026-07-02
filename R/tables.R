# =============================================================================
# Ready-made APA table builders for common MCA/HCPC appendix tables.
# Each returns a data.frame; compose with apa_table() / apa_bundle().
# Self-contained (only read the mca_fit object's fields).
# =============================================================================

#' Eigenvalues and retained dimensions (scree table).
#' @param fit an mca_fit; @param show number of dimensions to list.
#' @export
apa_eigenvalues <- function(fit, show = 8) {
  i <- utils::head(fit$inertia, show)
  data.frame(Dimension = i$dim, Eigenvalue = round(i$raw_eig, 4),
             "Variance %" = i$pct_raw, "Benzecri %" = i$pct_benzecri,
             "Cumulative %" = round(cumsum(i$pct_benzecri), 1), check.names = FALSE)
}

#' MCA dimension poles: characteristic low/high-pole codes + inertia per dimension.
#' @param fit an mca_fit; @param k codes per pole; @param dims dimensions to include.
#' @export
apa_dimension_poles <- function(fit, k = 5, dims = 1:3) {
  do.call(rbind, lapply(dims, function(d) {
    m <- fit$master
    o   <- order(m[[paste0("ctr_D", d)]], decreasing = TRUE)
    lab <- sub("^[^=]+=", "", m$category[o]); co <- m[[paste0("coord_D", d)]][o]
    data.frame(Dimension = paste0("D", d),
               "Inertia %" = fit$inertia$pct_benzecri[d],
               Eigenvalue  = round(fit$inertia$raw_eig[d], 4),
               "Low pole"  = paste(utils::head(lab[co < 0], k), collapse = "; "),
               "High pole" = paste(utils::head(lab[co > 0], k), collapse = "; "),
               check.names = FALSE)
  }))
}

#' Hierarchical cluster profiles: label, n, %, characteristic period, top codes.
#' @param fit an mca_fit; @param k top codes per cluster.
#' @export
apa_cluster_profiles <- function(fit, k = 5) {
  lv <- levels(fit$clusters); sz <- as.integer(table(fit$clusters))
  charp <- rep(NA_character_, length(lv))
  if (!is.null(fit$group)) {
    pc <- table(fit$clusters, fit$group); E <- outer(rowSums(pc), colSums(pc)) / sum(pc)
    r <- (pc - E) / sqrt(E); charp <- colnames(r)[apply(r, 1, which.max)]
  }
  adj  <- fit$master[, paste0("adjC_", lv), drop = FALSE]
  topc <- vapply(seq_along(lv), function(ci) {
    o <- order(adj[[ci]], decreasing = TRUE)[seq_len(k)]
    paste(sub("^[^=]+=", "", fit$master$category[o]), collapse = "; ") }, character(1))
  data.frame(Cluster = lv, Label = unname(fit$cluster_pretty[lv]), n = sz,
             "%" = round(100 * sz / fit$n, 1), "Characteristic period" = charp,
             "Top characteristic codes" = topc, check.names = FALSE)
}

#' Cluster-by-group cross-tabulation (with margins).
#' @param fit an mca_fit with a group.
#' @export
apa_cluster_by_period <- function(fit) {
  if (is.null(fit$group)) stop("no grouping available (fit was built without `group`)")
  tb <- addmargins(table(Cluster = fit$clusters, Period = fit$group))
  data.frame(Cluster = rownames(tb), as.data.frame.matrix(tb), check.names = FALSE, row.names = NULL)
}
