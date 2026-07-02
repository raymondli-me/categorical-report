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
    list(x = apa_eigenvalues(fit),      title = "Eigenvalues and retained dimensions (Benzecri-adjusted)"))
  if (!is.null(fit$group)) tabs <- c(tabs, list(
    list(x = categorical::mca_frequencies(fit, long = TRUE), title = "Frequency of coded categories by group (% within group)"),
    list(x = apa_cluster_by_period(fit),                     title = "Cluster by group distribution"),
    list(x = categorical::mca_top_group(fit),                title = "Top over-represented categories per group (adjusted residuals)"),
    list(x = categorical::mca_distribution(fit),             title = "Segment distribution across group and cluster (row-level hierarchical shares)")))
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
  f <- c(f, save_editable(categorical::plot_dendrogram(fit),
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
