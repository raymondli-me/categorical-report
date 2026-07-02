# =============================================================================
# Programmatic methodology export: turn the `categorical` technique registry into
# Word-friendly documentation -- brief spec table, aspect/page-level citations,
# symbol glossary, code table, a full handbook (native Word equations via pandoc),
# and a bibliography (.bib / APA7).
# =============================================================================

.mkeys <- function(keys) if (is.null(keys)) names(categorical::mca_techniques()) else keys

#' Brief methods specification table -> editable Word (raw formula dropped; see handbook).
#' @export
methods_brief <- function(file = "methods_brief.docx", keys = NULL) {
  tab <- categorical::mca_techniques_table(.mkeys(keys)); tab$Formula <- NULL
  apa_table(tab, file, number = "Table", title = "Methods: technique specifications",
            note = "Typeset equations are in the methods handbook; this table omits raw LaTeX.")
}

#' Aspect-level citation table with clickable, descriptive links -> Word.
#' @export
methods_citations <- function(file = "methods_citations.docx", keys = NULL) {
  ci <- categorical::mca_citations(.mkeys(keys))
  ci$Citation <- paste0(ci$Source, ifelse(nzchar(ci$Locator), paste0(", ", ci$Locator), ""))
  apa_table(ci[, c("Technique", "Aspect", "Citation", "Link")], file, number = "Table",
            title = "Citations by technique and aspect",
            note = "Each citation is a clickable link to the source (DOI, or a Google Scholar search).",
            link = list(text = "Citation", url = "Link"))
}

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
  spec <- categorical::mca_techniques_table(k); spec$Formula <- NULL
  ci <- categorical::mca_citations(k)
  ci$Citation <- paste0(ci$Source, ifelse(nzchar(ci$Locator), paste0(", ", ci$Locator), ""))
  ci <- ci[, c("Technique", "Aspect", "Citation", "Link")]
  apa_bundle(list(
    list(x = spec, number = "Table 1", title = "Technique specifications",
         note = "Typeset equations: see the methods handbook."),
    list(x = ci,   number = "Table 2", title = "Citations by technique and aspect",
         link = list(text = "Citation", url = "Link")),
    list(x = categorical::mca_glossary(k),   number = "Table 3", title = "Glossary of symbols"),
    list(x = categorical::mca_code_table(k), number = "Table 4", title = "Implementation functions and example calls")
  ), file)
}

#' Bibliography for the techniques used -> .bib or APA7 text.
#' @param format "bibtex" or "apa".
#' @export
write_bibliography <- function(file = "references.bib", format = c("bibtex", "apa"), keys = NULL) {
  format <- match.arg(format)
  writeLines(categorical::mca_bibliography(.mkeys(keys), format = format), file); invisible(file)
}

#' Curated methods overview (one row per technique) -> Word.
#' @export
methods_overview <- function(file = "methods_overview.docx", keys = NULL)
  apa_table(categorical::mca_overview(.mkeys(keys)), file, number = "Table",
            title = "Methods overview: technique, what it answers, function, and citation")

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
      if (!is.null(t$interpretation)) paste0("*Interpretation.* ", t$interpretation) else "", "",
      paste0("**Formula.** $", t$formula, "$"), "",
      "**Algorithm.**", "", paste0(seq_along(t$algorithm), ". ", t$algorithm), "",
      paste0("**Inputs.** ", t$inputs,
             if (nzchar(na0(t$optional_inputs))) paste0("  *Optional:* ", t$optional_inputs) else ""), "",
      paste0("**Outputs.** ", t$outputs,
             if (nzchar(na0(t$optional_outputs))) paste0("  *Optional:* ", t$optional_outputs) else ""), "",
      "**Code.**", "", "```r", t$code, "```", "",
      paste0("**Symbols.** ", paste(paste0("`", names(t$glossary), "` = ", unname(t$glossary)), collapse = "; ")), "",
      "**Citations.**", "",
      paste0("- ", ci$Reading, "  <", ci$Link, ">"), "")
  }
  out
}

#' Full methods handbook: one section per technique, with NATIVE Word equations.
#'
#' Renders through pandoc (via rmarkdown) so `$...$` LaTeX becomes editable Word
#' equations. Falls back to a plain-text officer document if rmarkdown/pandoc is
#' unavailable.
#' @export
# shared renderer: a markdown string -> docx / html / pdf via pandoc, with the
# cwd-safe output path, a LaTeX guard for pdf, and a plain-text officer fallback.
.render_md <- function(md, file, formats = "docx", native = TRUE, label = "document") {
  of <- c(docx = "word_document", html = "html_document", pdf = "pdf_document")
  ex <- c(word_document = "docx", html_document = "html", pdf_document = "pdf")
  if (native && requireNamespace("rmarkdown", quietly = TRUE) && rmarkdown::pandoc_available()) {
    mdf <- tempfile(fileext = ".Rmd"); writeLines(md, mdf)
    outdir <- dirname(file); if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
    base <- sub("\\.[^.]+$", "", basename(file)); adir <- normalizePath(outdir)
    for (ff in formats) {
      fmt <- of[[ff]]; if (is.null(fmt)) next
      if (ff == "pdf" && !nzchar(Sys.which("pdflatex")) &&
          !(requireNamespace("tinytex", quietly = TRUE) && tinytex::is_tinytex())) {
        message("  [note] PDF ", label, " skipped -- no LaTeX engine (run tinytex::install_tinytex())."); next }
      tryCatch(
        rmarkdown::render(mdf, output_format = fmt, output_file = paste0(base, ".", ex[[fmt]]),
                          output_dir = adir, quiet = TRUE),
        error = function(e) message("  [note] ", ff, " ", label, " failed -- ", conditionMessage(e)))
    }
  } else {                                                     # plain-text officer fallback
    message("  [note] ", label, " is PLAIN TEXT -- rmarkdown/pandoc not found.")
    doc <- officer::read_docx(); for (ln in md[!grepl("^---", md)]) doc <- officer::body_add_par(doc, ln)
    print(doc, target = file)
  }
  invisible(file)
}

#' Full methods handbook (one section per technique, native equations) -> docx/html/pdf.
#' @export
methods_handbook <- function(file = "methods_handbook.docx", keys = NULL, native = TRUE,
                             formats = "docx")
  .render_md(.handbook_md(.mkeys(keys)), file, formats, native, label = "handbook")

#' Formatted APA reference list -> Word/HTML/PDF (hanging indent, clickable links).
#' @param file output path; @param formats any of "docx","html","pdf".
#' @export
methods_bibliography <- function(file = "references.docx", keys = NULL, formats = "docx") {
  refs <- categorical::mca_bibliography(.mkeys(keys), format = "apa")   # APA strings (with DOI/Scholar URL)
  linked <- gsub("(https?://[^ ]+)", "[\\1](\\1)", refs)                # make the trailing URL clickable
  md <- c("---", "title: 'References'", "---", "",
          "# References", "",
          "::: {style=\"padding-left:2em; text-indent:-2em;\"}", "",   # hanging indent (html)
          paste0(linked, "\n"), ":::")
  .render_md(md, file, formats, native = TRUE, label = "bibliography")
}

#' One-shot: write the whole methodology documentation set to a directory.
#'
#' methods_overview/tables (docx), handbook (docx+html+pdf, native equations),
#' formatted APA references (docx+html+pdf), and references.bib (for managers).
#' @export
methods_document <- function(dir = ".", keys = NULL) {
  k <- .mkeys(keys); f <- c()
  f <- c(f, methods_overview(file.path(dir, "methods_overview.docx"), k))
  f <- c(f, methods_tables(file.path(dir, "methods_tables.docx"), k))
  f <- c(f, methods_handbook(file.path(dir, "methods_handbook.docx"), k, formats = c("docx", "html", "pdf")))
  f <- c(f, methods_bibliography(file.path(dir, "references.docx"), k, formats = c("docx", "html", "pdf")))
  f <- c(f, write_bibliography(file.path(dir, "references.bib"), "bibtex", k))   # machine-readable for Zotero/BibTeX
  invisible(f)
}
