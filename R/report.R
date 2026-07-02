# =============================================================================
# categoricalReport : one-line APA Word (.docx) tables + Word/PowerPoint-editable
# figures for outputs of the MIT 'categorical' package.
#
# GPL-3 (renders via flextable + devEMF/rvg, which are GPL). 'categorical' is MIT.
# =============================================================================

# internal: build one APA-styled flextable from a data.frame
.apa_ft <- function(x, cols = NULL, digits = 2, headers = NULL, note = NULL) {
  x <- as.data.frame(x, check.names = FALSE)
  if (!is.null(cols))    x <- x[, cols, drop = FALSE]
  isnum <- vapply(x, is.numeric, logical(1)); x[isnum] <- lapply(x[isnum], round, digits)
  if (!is.null(headers)) names(x)[match(names(headers), names(x))] <- unname(headers)
  bd <- officer::fp_border(color = "black", width = 1)
  ft <- flextable::flextable(x)
  ft <- flextable::font(ft, part = "all", fontname = "Times New Roman")
  ft <- flextable::fontsize(ft, part = "all", size = 11)
  ft <- flextable::border_remove(ft)
  ft <- flextable::hline_top(ft, part = "header", border = bd)
  ft <- flextable::hline_bottom(ft, part = "header", border = bd)
  ft <- flextable::hline_bottom(ft, part = "body", border = bd)
  ft <- flextable::align(ft, part = "header", align = "left")
  if (any(isnum)) ft <- flextable::align(ft, j = which(isnum), align = "right", part = "body")
  if (!is.null(note))
    ft <- flextable::add_footer_lines(ft, values = flextable::as_paragraph(flextable::as_i("Note. "), note))
  flextable::set_table_properties(ft, layout = "autofit")
}

# internal: add a numbered + titled table block to an officer doc
.add_block <- function(doc, ft, number, title) {
  ff <- "Times New Roman"
  doc <- officer::body_add_fpar(doc, officer::fpar(
    officer::ftext(number, officer::fp_text(font.family = ff, font.size = 11, bold = TRUE))))
  if (nzchar(title)) doc <- officer::body_add_fpar(doc, officer::fpar(
    officer::ftext(title, officer::fp_text(font.family = ff, font.size = 11, italic = TRUE))))
  doc <- flextable::body_add_flextable(doc, ft)
  officer::body_add_par(doc, "")
}

#' One-line, natively editable APA-style Word table.
#'
#' @param x       a data.frame (e.g. any `categorical` table).
#' @param file    output `.docx` path.
#' @param title   table title (italic, under the number).
#' @param number  table number, e.g. "Table 1".
#' @param note    general note; "Note. " is prefixed automatically.
#' @param cols    columns to keep (names or indices) -- the filter.
#' @param digits  round numeric columns.
#' @param headers named vector to rename headers, `c(old = "New")`.
#' @param landscape put the table on a landscape page.
#' @return (invisibly) the file path.
#' @export
apa_table <- function(x, file, title = "", number = "Table 1", note = NULL,
                      cols = NULL, digits = 2, headers = NULL, landscape = FALSE) {
  ft  <- .apa_ft(x, cols = cols, digits = digits, headers = headers, note = note)
  doc <- .add_block(officer::read_docx(), ft, number, title)
  if (landscape) doc <- officer::body_end_section_landscape(doc)
  print(doc, target = file); invisible(file)
}

#' Bundle several APA tables into ONE Word document.
#'
#' @param tables a list of lists; each entry may contain `x` (required), `number`,
#'   `title`, `note`, `cols`, `digits`, `headers`.
#' @param file   output `.docx` path.
#' @param landscape landscape pages.
#' @return (invisibly) the file path.
#' @export
apa_bundle <- function(tables, file, landscape = FALSE) {
  doc <- officer::read_docx()
  for (t in tables) {
    ft  <- .apa_ft(t$x, cols = t$cols, digits = if (is.null(t$digits)) 2 else t$digits,
                   headers = t$headers, note = t$note)
    doc <- .add_block(doc, ft,
                      number = if (is.null(t$number)) "Table" else t$number,
                      title  = if (is.null(t$title))  ""      else t$title)
  }
  if (landscape) doc <- officer::body_end_section_landscape(doc)
  print(doc, target = file); invisible(file)
}

#' Save a plot as a Word/Office-native EDITABLE figure.
#'
#' `format = "emf"` -> insert into Word, right-click > Ungroup: every label / axis /
#' legend line becomes individually editable. `format = "pptx"` -> fully editable
#' DrawingML on a PowerPoint slide (rvg).
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
