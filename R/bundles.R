# =============================================================================
# One-shot report helpers: bundle all standard MCA/HCPC appendix tables into one
# Word document, all figures into editable files, and the colour-3D map (bundled
# matplotlib renderer via reticulate).
# =============================================================================

#' All standard MCA/HCPC appendix tables in ONE Word document.
#'
#' Assembles dimension poles, cluster profiles, eigenvalues, and (if the fit has a
#' grouping) frequency-by-group, cluster-by-group, group signatures, and the
#' row-level segment distribution -- each a native, editable APA table.
#'
#' @param fit  an mca_fit (from `categorical::mca_run`).
#' @param file output `.docx` path.
#' @param prefix table-number prefix, e.g. "Table ".
#' @return (invisibly) the file path.
#' @export
mca_appendix <- function(fit, file = "MCA_appendix_tables.docx", prefix = "Table ") {
  tabs <- list(
    list(x = apa_dimension_poles(fit),  title = "MCA dimension poles and Benzecri-adjusted variance explained"),
    list(x = apa_cluster_profiles(fit), title = "Hierarchical cluster profiles and characteristic codes",
         note = sprintf("N = %d segments.", fit$n)),
    list(x = apa_eigenvalues(fit),      title = "Eigenvalues and retained dimensions (Benzecri-adjusted)"),
    list(x = apa_inertia_gain(fit),     title = "Cluster-count selection by between-inertia gain (HCPC)",
         note = "k is chosen at the last large gain before diminishing returns."),
    list(x = apa_contributions(fit),    title = "Category contributions to each dimension (%)",
         note = "Contributions sum to 100% within a dimension; sorted by peak contribution."),
    list(x = apa_cos2(fit),             title = "Squared cosines (cos2): quality of representation by dimension",
         note = "cos2 is the share of a category's inertia captured by a dimension (row Total across the 3 retained dims)."),
    list(x = apa_coordinates(fit),      title = "Category principal coordinates by dimension"))
  if (!is.null(fit$group)) tabs <- c(tabs, list(
    list(x = categorical::mca_frequencies(fit, long = TRUE), title = "Frequency of coded categories by group (% within group)"),
    list(x = apa_cluster_by_period(fit),                     title = "Cluster by group distribution"),
    list(x = categorical::mca_top_group(fit),                title = "Top over-represented categories per group (adjusted residuals)"),
    list(x = categorical::mca_distribution(fit),             title = "Segment distribution across group and cluster (row-level hierarchical shares)"),
    list(x = apa_typicality(fit),      title = "Geometric typicality Z by dimension and group",
         note = "|Z| > 1.96 flags an atypical group mean on that dimension."),
    list(x = apa_eta(fit),             title = "Correlation ratio (eta^2), F, and permutation p by space",
         note = "eta^2 = share of inertia explained by the grouping; p from permuted labels."),
    list(x = apa_ellipse_overlap(fit), title = "Overlap of 95% bootstrap confidence ellipses (D1 x D2)",
         note = "Non-overlapping ellipses indicate distinguishable group mean positions.")))
  for (i in seq_along(tabs)) tabs[[i]]$number <- paste0(prefix, i)   # generic sequential numbering
  apa_bundle(tabs, file); invisible(file)
}

#' All standard figures as Word/PowerPoint-editable files.
#'
#' Writes scree, the D1xD2 and D1xD3 cluster maps, and the Ward dendrogram as
#' editable files (EMF for Word, or PPTX). Requires the `categorical` base-R plots.
#'
#' @param fit an mca_fit; @param dir output directory; @param format "emf" or "pptx".
#' @return (invisibly) the file paths written.
#' @export
mca_figures <- function(fit, dir = ".", format = c("emf", "pptx")) {
  format <- match.arg(format); ext <- format; f <- character()
  f <- c(f, save_editable(categorical::plot_scree(fit),
                          file.path(dir, paste0("Fig_scree.", ext)),      format, 6.6, 4.6))
  f <- c(f, save_editable(categorical::plot_map(fit, c(1,2), bw = TRUE, ellipse = "centroid", legend = FALSE),
                          file.path(dir, paste0("Fig_map_D1D2.", ext)),   format, 7, 6.6))
  f <- c(f, save_editable(categorical::plot_map(fit, c(1,3), bw = TRUE, ellipse = "centroid", legend = FALSE),
                          file.path(dir, paste0("Fig_map_D1D3.", ext)),   format, 7, 6.6))
  f <- c(f, save_editable(categorical::plot_dendrogram(fit, bw = TRUE),
                          file.path(dir, paste0("Fig_dendrogram.", ext)), format, 7, 5))
  invisible(f)
}

#' Colour 3D MCA cluster map + 2D projections (bundled matplotlib renderer).
#'
#' Exports the fit's coordinates and runs the packaged Python renderer through
#' reticulate, producing `FIG_cluster_map_3d_color.{pdf,png}` and the three 2D
#' projection panels in `dir`.
#'
#' @param fit an mca_fit with a grouping; @param dir output directory.
#' @return (invisibly) the PNG paths written.
#' @export
plot_map_3d <- function(fit, dir = ".") {
  if (!requireNamespace("reticulate", quietly = TRUE)) stop("install.packages('reticulate')")
  categorical::mca_export_fig3d(fit, dir = dir)
  py <- system.file("python", "mca_map3d.py", package = "categoricalReport")
  if (!nzchar(py)) stop("bundled renderer not found")
  old <- Sys.getenv("FIGDIR"); Sys.setenv(FIGDIR = dir); on.exit(Sys.setenv(FIGDIR = old))
  reticulate::py_run_file(py)
  invisible(list.files(dir, pattern = "^FIG_(cluster_map_3d_color|map_2d).*\\.png$", full.names = TRUE))
}

#' One call: write ALL outputs into an organized folder and zip it.
#'
#' Layout: <dir>/data (CSV masters, inertia, frequencies), <dir>/tables (appendix
#' .docx), <dir>/figures (scree/maps/dendrogram/3D as PNG+PDF, plus editable EMF),
#' <dir>/methods (methods tables + handbook + bibliography). Returns the .zip path.
#'
#' @param fit an mca_fit; @param dir output folder; @param methods include methods docs;
#' @param zip also produce <dir>.zip.
#' @return (invisibly) the zip path (or the directory if zip=FALSE).
#' @export
export_all <- function(fit, dir = "mca_outputs", methods = TRUE, zip = TRUE) {
  mk <- function(x) { d <- file.path(dir, x); dir.create(d, recursive = TRUE, showWarnings = FALSE); d }
  Dd <- mk("data"); Td <- mk("tables"); Fd <- mk("figures"); Md <- if (methods) mk("methods") else NULL

  ## data (CSV)
  utils::write.csv(categorical::mca_master(fit),      file.path(Dd, "master_columns.csv"), row.names = FALSE)
  utils::write.csv(categorical::mca_master_rows(fit), file.path(Dd, "master_rows.csv"),    row.names = FALSE)
  utils::write.csv(fit$inertia,                       file.path(Dd, "inertia.csv"),        row.names = FALSE)
  utils::write.csv(fit$gain_tab,                      file.path(Dd, "inertia_gain.csv"),   row.names = FALSE)
  if (!is.null(fit$group)) {
    utils::write.csv(categorical::mca_frequencies(fit, long = TRUE), file.path(Dd, "frequency_by_group.csv"), row.names = FALSE)
    utils::write.csv(categorical::mca_distribution(fit),            file.path(Dd, "segment_distribution.csv"), row.names = FALSE)
  }

  ## tables (Word)
  try(mca_appendix(fit, file.path(Td, "appendix_tables.docx")), silent = TRUE)

  ## figures: base-R PNG + PDF
  gs <- function(name, fn, w = 7, h = 6) {
    grDevices::png(file.path(Fd, paste0(name, ".png")), width = w, height = h, units = "in", res = 200); fn(); grDevices::dev.off()
    grDevices::pdf(file.path(Fd, paste0(name, ".pdf")), width = w, height = h); fn(); grDevices::dev.off()
  }
  gs("scree",      function() categorical::plot_scree(fit), 6.6, 4.6)
  gs("map_D1D2",   function() categorical::plot_map(fit, c(1,2), bw = TRUE, ellipse = "centroid", legend = FALSE))
  gs("map_D1D3",   function() categorical::plot_map(fit, c(1,3), bw = TRUE, ellipse = "centroid", legend = FALSE))
  gs("dendrogram", function() categorical::plot_dendrogram(fit, bw = TRUE), 8, 5)
  try(mca_figures(fit, Fd), silent = TRUE)                 # editable EMF versions
  try(plot_map_3d(fit, Fd), silent = TRUE)                 # colour 3D + 2D projections

  ## methods documentation
  if (methods) try(methods_document(Md), silent = TRUE)

  if (zip) { zf <- paste0(dir, ".zip"); if (file.exists(zf)) unlink(zf)
    try(utils::zip(zf, dir, flags = "-r9Xq"), silent = TRUE); return(invisible(zf)) }
  invisible(dir)
}
