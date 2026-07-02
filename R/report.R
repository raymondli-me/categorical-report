# =============================================================================
# categoricalReport : one-line APA Word (.docx) tables + Word/PowerPoint-editable
# figures for outputs of the MIT 'categorical' package.
#
# GPL-3 (renders via flextable + devEMF/rvg, which are GPL). 'categorical' is MIT.
# =============================================================================

#' One-line, natively editable APA-style Word table.
#'
#' Writes a `.docx` containing a genuine (editable) Word table in APA format:
#' three horizontal rules, Times New Roman 11, right-aligned numbers, a bold
#' table number, an italic title, and an italic "Note." footer.
#'
#' @param x       a data.frame (e.g. any `categorical` table such as
#'                `mca_master(fit)` or `mca_top_cluster(fit)`).
#' @param file    output `.docx` path.
#' @param title   table title (italic, under the number).
#' @param number  table number, e.g. "Table 1".
#' @param note    general note; "Note. " is prefixed automatically.
#' @param cols    columns to keep (names or indices) -- the filter.
#' @param digits  round numeric columns to this many places.
#' @param headers named vector to rename headers, `c(old = "New")`.
#' @param landscape put the table on a landscape page.
#' @return (invisibly) the file path.
#' @export
apa_table <- function(x, file, title = "", number = "Table 1", note = NULL,
                      cols = NULL, digits = 2, headers = NULL, landscape = FALSE) {
  x <- as.data.frame(x, check.names = FALSE)
  if (!is.null(cols))    x <- x[, cols, drop = FALSE]
  isnum <- vapply(x, is.numeric, logical(1)); x[isnum] <- lapply(x[isnum], round, digits)
  if (!is.null(headers)) names(x)[match(names(headers), names(x))] <- unname(headers)
  ff <- "Times New Roman"; bd <- officer::fp_border(color = "black", width = 1)
  ft <- flextable::flextable(x)
  ft <- flextable::font(ft, part = "all", fontname = ff)
  ft <- flextable::fontsize(ft, part = "all", size = 11)
  ft <- flextable::border_remove(ft)
  ft <- flextable::hline_top(ft, part = "header", border = bd)
  ft <- flextable::hline_bottom(ft, part = "header", border = bd)
  ft <- flextable::hline_bottom(ft, part = "body", border = bd)
  ft <- flextable::align(ft, part = "header", align = "left")
  if (any(isnum)) ft <- flextable::align(ft, j = which(isnum), align = "right", part = "body")
  if (!is.null(note))
    ft <- flextable::add_footer_lines(ft, values = flextable::as_paragraph(flextable::as_i("Note. "), note))
  ft <- flextable::set_table_properties(ft, layout = "autofit")
  doc <- officer::read_docx()
  doc <- officer::body_add_fpar(doc, officer::fpar(
    officer::ftext(number, officer::fp_text(font.family = ff, font.size = 11, bold = TRUE))))
  if (nzchar(title)) doc <- officer::body_add_fpar(doc, officer::fpar(
    officer::ftext(title, officer::fp_text(font.family = ff, font.size = 11, italic = TRUE))))
  doc <- flextable::body_add_flextable(doc, ft)
  if (landscape) doc <- officer::body_end_section_landscape(doc)
  print(doc, target = file); invisible(file)
}

#' Save a plot as a Word/Office-native EDITABLE figure.
#'
#' `format = "emf"` writes an Enhanced Metafile: insert it into Word, right-click
#' and choose *Ungroup*, and every label, axis tick and legend line becomes an
#' individually editable object. `format = "pptx"` writes a PowerPoint slide with
#' the plot as fully editable DrawingML (via rvg); copy it into Word if needed.
#'
#' @param expr   a plotting expression, e.g. `plot_map(fit, c(1,2))`.
#' @param file   output `.emf` or `.pptx` path.
#' @param format "emf" (Word) or "pptx" (PowerPoint).
#' @param width,height inches.
#' @return (invisibly) the file path.
#' @export
save_editable <- function(expr, file, format = c("emf", "pptx"), width = 7, height = 6) {
  format <- match.arg(format); pe <- parent.frame()
  if (format == "emf") {
    if (!requireNamespace("devEMF", quietly = TRUE)) stop("install.packages('devEMF')")
    devEMF::emf(file, width = width, height = height, emfPlus = TRUE)
    eval(substitute(expr), pe); grDevices::dev.off()
  } else {
    if (!requireNamespace("rvg", quietly = TRUE)) stop("install.packages('rvg')")
    doc <- officer::read_pptx(); doc <- officer::add_slide(doc)
    doc <- officer::ph_with(doc, rvg::dml(code = eval(substitute(expr), pe)),
                            location = officer::ph_location_fullsize())
    print(doc, target = file)
  }
  invisible(file)
}
