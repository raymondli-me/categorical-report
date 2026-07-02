# =============================================================================
# Programmatic methodology export: turn the `categorical` technique registry into
# Word-friendly documentation -- brief spec table, aspect/page-level citations,
# symbol glossary, code table, a full handbook (native Word equations via pandoc),
# and a bibliography (.bib / APA7).
# =============================================================================

.mkeys <- function(keys) if (is.null(keys)) names(categorical::mca_techniques()) else keys

#' Brief methods specification table -> editable Word.
#' @export
methods_brief <- function(file = "methods_brief.docx", keys = NULL)
  apa_table(categorical::mca_techniques_table(.mkeys(keys)), file, number = "Table",
            title = "Methods: technique specifications",
            note = "Formulas are shown in LaTeX; see the handbook for typeset equations.")

#' Aspect-level citation table: which source (and page) grounds which part -> Word.
#' @export
methods_citations <- function(file = "methods_citations.docx", keys = NULL)
  apa_table(categorical::mca_citations(.mkeys(keys)), file, number = "Table",
            title = "Citations by technique and aspect",
            note = "Each row links a specific claim of a technique to its source and locator (page/chapter).")

#' Symbol glossary -> Word.
#' @export
methods_glossary <- function(file = "methods_glossary.docx", keys = NULL)
  apa_table(categorical::mca_glossary(.mkeys(keys)), file, number = "Table",
            title = "Glossary of symbols")

#' Implementation table: package function + example call per technique -> Word.
#' @export
methods_code <- function(file = "methods_code.docx", keys = NULL)
  apa_table(categorical::mca_code_table(.mkeys(keys)), file, number = "Table",
            title = "Implementation: functions and example calls")

#' All four methods tables in ONE Word document.
#' @export
methods_tables <- function(file = "methods_tables.docx", keys = NULL) {
  k <- .mkeys(keys)
  apa_bundle(list(
    list(x = categorical::mca_techniques_table(k), number = "Table 1", title = "Technique specifications"),
    list(x = categorical::mca_citations(k),        number = "Table 2", title = "Citations by technique and aspect"),
    list(x = categorical::mca_glossary(k),         number = "Table 3", title = "Glossary of symbols"),
    list(x = categorical::mca_code_table(k),       number = "Table 4", title = "Implementation functions and example calls")
  ), file)
}

#' Bibliography for the techniques used -> .bib or APA7 text.
#' @param format "bibtex" or "apa".
#' @export
write_bibliography <- function(file = "references.bib", format = c("bibtex", "apa"), keys = NULL) {
  format <- match.arg(format)
  writeLines(categorical::mca_bibliography(.mkeys(keys), format = format), file); invisible(file)
}

# assemble the handbook as (pandoc) markdown, one section per technique
.handbook_md <- function(keys) {
  na0 <- function(x) if (is.null(x) || is.na(x)) "" else x
  out <- c("---", "title: 'Methods Handbook'", "output: word_document", "---", "")
  for (k in keys) {
    t <- categorical::mca_technique(k); ci <- categorical::mca_citations(k)
    loc <- ifelse(nzchar(ci$Locator), paste0(" (", ci$Locator, ")"), "")
    out <- c(out,
      paste("##", t$name), "",
      t$brief, "",
      paste0("**Formula.** $", t$formula, "$"), "",
      "**Algorithm.**", "", paste0(seq_along(t$algorithm), ". ", t$algorithm), "",
      paste0("**Inputs.** ", t$inputs,
             if (nzchar(na0(t$optional_inputs))) paste0("  *Optional:* ", t$optional_inputs) else ""), "",
      paste0("**Outputs.** ", t$outputs,
             if (nzchar(na0(t$optional_outputs))) paste0("  *Optional:* ", t$optional_outputs) else ""), "",
      "**Code.**", "", "```r", t$code, "```", "",
      paste0("**Symbols.** ", paste(paste0("`", names(t$glossary), "` = ", unname(t$glossary)), collapse = "; ")), "",
      "**Citations.**", "",
      paste0("- ", ci$Source, loc, " — ", ci$Aspect), "")
  }
  out
}

#' Full methods handbook: one section per technique, with NATIVE Word equations.
#'
#' Renders through pandoc (via rmarkdown) so `$...$` LaTeX becomes editable Word
#' equations. Falls back to a plain-text officer document if rmarkdown/pandoc is
#' unavailable.
#' @export
methods_handbook <- function(file = "methods_handbook.docx", keys = NULL, native = TRUE) {
  keys <- .mkeys(keys); md <- .handbook_md(keys)
  if (native && requireNamespace("rmarkdown", quietly = TRUE) && nzchar(Sys.which("pandoc"))) {
    mdf <- tempfile(fileext = ".Rmd"); writeLines(md, mdf)
    rmarkdown::render(mdf, output_format = "word_document",
                      output_file = normalizePath(file, mustWork = FALSE), quiet = TRUE)
  } else {                                                     # text fallback (no typeset math)
    doc <- officer::read_docx()
    for (ln in md[-(1:5)]) doc <- officer::body_add_par(doc, ln)
    print(doc, target = file)
  }
  invisible(file)
}

#' One-shot: write the whole methodology documentation set to a directory.
#'
#' Produces methods_tables.docx (brief + citations + glossary + code), the
#' handbook (native equations), and references.bib + references_apa.txt.
#' @export
methods_document <- function(dir = ".", keys = NULL) {
  k <- .mkeys(keys); f <- c()
  f <- c(f, methods_tables(file.path(dir, "methods_tables.docx"), k))
  f <- c(f, methods_handbook(file.path(dir, "methods_handbook.docx"), k))
  f <- c(f, write_bibliography(file.path(dir, "references.bib"), "bibtex", k))
  f <- c(f, write_bibliography(file.path(dir, "references_apa.txt"), "apa", k))
  invisible(f)
}
