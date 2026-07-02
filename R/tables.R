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

#' Category contributions to each dimension (%, sum to 100 per dimension).
#' @param fit an mca_fit; @param digits rounding; @param sort by peak contribution.
#' @export
apa_contributions <- function(fit, digits = 1, sort = TRUE) {
  m <- fit$master
  d <- data.frame(Variable = sub("=.*$", "", m$category), Category = sub("^[^=]+=", "", m$category),
                  D1 = round(100 * m$ctr_D1, digits), D2 = round(100 * m$ctr_D2, digits),
                  D3 = round(100 * m$ctr_D3, digits), check.names = FALSE)
  if (sort) d <- d[order(-pmax(d$D1, d$D2, d$D3)), ]
  rownames(d) <- NULL; d
}

#' Squared cosines (cos2, quality of representation) per dimension.
#' @param fit an mca_fit; @param digits rounding.
#' @export
apa_cos2 <- function(fit, digits = 3) {
  m <- fit$master
  data.frame(Variable = sub("=.*$", "", m$category), Category = sub("^[^=]+=", "", m$category),
             D1 = round(m$cos2_D1, digits), D2 = round(m$cos2_D2, digits), D3 = round(m$cos2_D3, digits),
             Total = round(m$cos2_D1 + m$cos2_D2 + m$cos2_D3, digits),
             check.names = FALSE, row.names = NULL)
}

#' Category principal coordinates per dimension.
#' @param fit an mca_fit; @param digits rounding.
#' @export
apa_coordinates <- function(fit, digits = 3) {
  m <- fit$master
  data.frame(Variable = sub("=.*$", "", m$category), Category = sub("^[^=]+=", "", m$category),
             D1 = round(m$coord_D1, digits), D2 = round(m$coord_D2, digits), D3 = round(m$coord_D3, digits),
             check.names = FALSE, row.names = NULL)
}

#' Geometric typicality Z by dimension x group (|Z| > 1.96 is atypical).
#' @export
apa_typicality <- function(fit, digits = 2) {
  m <- categorical::mca_typicality(fit)
  data.frame(Dimension = rownames(m), round(as.data.frame.matrix(m), digits),
             check.names = FALSE, row.names = NULL)
}

#' Correlation ratio (eta^2), F, and permutation p by space.
#' @export
apa_eta <- function(fit, digits = 4) {
  e <- categorical::mca_eta(fit)
  data.frame(Space = e$space, "eta^2" = round(e$eta2, digits), F = round(e$F, 2),
             "Permutation p" = signif(e$perm_p, 3), check.names = FALSE, row.names = NULL)
}

#' Pairwise overlap of 95% bootstrap confidence ellipses for group means.
#' @export
apa_ellipse_overlap <- function(fit, dims = c(1, 2)) {
  o <- categorical::mca_ellipses(fit, dims)$overlap
  data.frame("Group 1" = o$g1, "Group 2" = o$g2,
             "95% ellipses overlap" = ifelse(o$overlap, "yes", "no"),
             check.names = FALSE, row.names = NULL)
}

#' Adjusted Rand Index matrix across a named list of fits (or cluster vectors).
#' @param fits named list of mca_fit objects or integer cluster vectors.
#' @export
apa_ari_matrix <- function(fits, digits = 3) {
  cl <- function(x) if (is.list(x) && !is.null(x$clusters)) x$clusters else x
  k <- names(fits)
  M <- outer(k, k, Vectorize(function(a, b) round(categorical::mca_ari(cl(fits[[a]]), cl(fits[[b]])), digits)))
  df <- as.data.frame(M, stringsAsFactors = FALSE); names(df) <- k
  cbind(Fit = k, df)
}

#' Cluster-count selection by between-inertia gain (HCPC diagnostic).
#' @export
apa_inertia_gain <- function(fit, digits = 1) {
  g <- fit$gain_tab
  data.frame("k (clusters)" = g$k, "Between-inertia %" = round(g$between_pct, digits),
             "Gain vs k-1 (pp)" = round(g$gain, digits), check.names = FALSE, row.names = NULL)
}
