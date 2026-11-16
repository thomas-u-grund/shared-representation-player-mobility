# Post-process a pandoc .docx: fonts, page set-up and table formatting (same layout as the Python/python-docx version).
suppressPackageStartupMessages(library(xml2))
W <- "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
wq <- function(x) xml_find_all  # placeholder to keep lint quiet
style_docx <- function(target, title, widthf) {
  target <- normalizePath(target); td <- tempfile("docx_"); dir.create(td); zip::unzip(target, exdir = td)
  ns <- c(w = W)
  nd <- function(parent, name, ...) { a <- list(...); n <- xml_add_child(parent, paste0("w:", name)); for (k in names(a)) xml_set_attr(n, paste0("w:", k), a[[k]]); n }
  first <- function(parent, xp) { r <- xml_find_first(parent, xp, ns); if (inherits(r, "xml_missing")) NULL else r }
  ensure <- function(parent, name, after = NULL) { r <- first(parent, paste0("w:", name)); if (!is.null(r)) return(r); if (is.null(after)) nd(parent, name) else { n <- xml_add_sibling(after, paste0("w:", name), .where = "after"); n } }
  setfont <- function(pr_parent, font = "Cambria", size = NULL, color = "000000", bold = NULL) {
    rpr <- first(pr_parent, "w:rPr"); if (is.null(rpr)) rpr <- nd(pr_parent, "rPr")
    for (nm in c("rFonts", "color", if (!is.null(size)) c("sz", "szCs"))) { x <- first(rpr, paste0("w:", nm)); if (!is.null(x)) xml_remove(x) }
    rf <- nd(rpr, "rFonts", ascii = font, hAnsi = font, cs = font, eastAsia = font)
    if (!is.null(color)) nd(rpr, "color", val = color)
    if (!is.null(size)) { nd(rpr, "sz", val = as.character(size * 2)); nd(rpr, "szCs", val = as.character(size * 2)) }
    if (!is.null(bold)) { b <- first(rpr, "w:b"); if (!is.null(b)) xml_remove(b); if (bold) nd(rpr, "b") }
    rpr
  }
  ppr_add <- function(ppr, name, ...) {   # insert in schema order: pStyle, keepNext, keepLines, ..., spacing, ind, jc
    order_ <- c("pStyle", "keepNext", "keepLines", "pageBreakBefore", "framePr", "widowControl", "numPr", "suppressLineNumbers", "pBdr", "shd", "tabs", "suppressAutoHyphens", "kinsoku", "wordWrap", "overflowPunct",
                "topLinePunct", "autoSpaceDE", "autoSpaceDN", "bidi", "adjustRightInd", "snapToGrid", "spacing", "ind", "contextualSpacing", "mirrorIndents", "suppressOverlap", "jc")
    pos <- match(name, order_); kids <- xml_children(ppr); idx <- match(xml_name(kids), order_)
    n <- xml_add_child(ppr, paste0("w:", name), .where = 0)
    prev <- kids[!is.na(idx) & idx < pos]
    if (length(prev)) { xml_remove(n); n <- xml_add_sibling(prev[[length(prev)]], paste0("w:", name), .where = "after") }
    a <- list(...); for (k in names(a)) xml_set_attr(n, paste0("w:", k), a[[k]]); n
  }
  # ---- styles
  sf <- file.path(td, "word", "styles.xml"); st <- read_xml(sf)
  for (id in c("Normal", "BodyText", "FirstParagraph", "Compact", "Title", "Heading1", "Heading2", "Heading3")) {
    s <- first(st, sprintf("//w:style[@w:styleId='%s']", id)); if (is.null(s)) next
    sz <- if (id %in% c("Normal", "BodyText", "FirstParagraph", "Compact")) 11.5 else if (id == "Title") 23 else NULL
    setfont(s, size = sz)
    ppr <- first(s, "w:pPr"); if (is.null(ppr)) ppr <- nd(s, "pPr")
    sp <- first(ppr, "w:spacing"); if (!is.null(sp)) xml_remove(sp)
    if (id %in% c("Normal", "BodyText", "FirstParagraph", "Compact")) ppr_add(ppr, "spacing", after = "140", line = "276", lineRule = "auto")
    if (id == "Title") ppr_add(ppr, "spacing", after = "300")
    if (startsWith(id, "Heading")) ppr_add(ppr, "spacing", before = "240", after = "80")
  }
  write_xml(st, sf)
  # ---- document
  df <- file.path(td, "word", "document.xml"); doc <- read_xml(df)
  sect <- xml_find_last <- NULL
  sp <- xml_find_all(doc, "//w:sectPr", ns); sp <- sp[[length(sp)]]
  for (nm in c("pgSz", "pgMar")) { x <- first(sp, paste0("w:", nm)); if (!is.null(x)) xml_remove(x) }
  nd(sp, "pgSz", w = "12240", h = "15840"); nd(sp, "pgMar", top = "1238", right = "1440", bottom = "1238", left = "1440", header = "720", footer = "720", gutter = "0")
  for (p in xml_find_all(doc, "//w:body/w:p", ns)) {
    sty <- first(p, "w:pPr/w:pStyle"); sid <- if (is.null(sty)) "" else xml_attr(sty, "w:val", ns)
    txt <- paste(xml_text(xml_find_all(p, ".//w:t", ns)), collapse = "")
    if (startsWith(sid, "Heading")) for (r in xml_find_all(p, "w:r", ns)) setfont(r, color = "000000", font = "Cambria")
    if (startsWith(txt, "Table ")) { ppr <- first(p, "w:pPr"); if (is.null(ppr)) { ppr <- xml_add_child(p, "w:pPr", .where = 0) }; if (is.null(first(ppr, "w:keepNext"))) ppr_add(ppr, "keepNext") }
    if (startsWith(txt, "Note.")) for (r in xml_find_all(p, "w:r", ns)) { rpr <- first(r, "w:rPr"); if (is.null(rpr)) rpr <- xml_add_child(r, "w:rPr", .where = 0)
      for (nm in c("sz", "szCs")) { x <- first(rpr, paste0("w:", nm)); if (!is.null(x)) xml_remove(x) }; nd(rpr, "sz", val = "20"); nd(rpr, "szCs", val = "20") }
  }
  tbls <- xml_find_all(doc, "//w:body/w:tbl", ns)
  for (i in seq_along(tbls)) {
    t <- tbls[[i]]; rows <- xml_find_all(t, "w:tr", ns); ncol <- length(xml_find_all(rows[[1]], "w:tc", ns))
    widths <- widthf(i - 1, ncol); tw <- as.character(round(widths * 1440))
    tp <- first(t, "w:tblPr"); for (nm in c("tblW", "tblLayout", "tblBorders")) { x <- first(tp, paste0("w:", nm)); if (!is.null(x)) xml_remove(x) }
    look <- first(tp, "w:tblLook")
    mk <- function(name, ...) { n <- if (is.null(look)) xml_add_child(tp, paste0("w:", name)) else xml_add_sibling(look, paste0("w:", name), .where = "before"); for (k in names(list(...))) xml_set_attr(n, paste0("w:", k), list(...)[[k]]); n }
    mk("tblW", w = as.character(sum(as.integer(tw))), type = "dxa")
    b <- mk("tblBorders"); for (e in c("top", "left", "bottom", "right", "insideH", "insideV")) nd(b, e, val = "single", sz = "4", color = "D9D9D9")
    mk("tblLayout", type = "fixed")
    grid <- first(t, "w:tblGrid"); if (!is.null(grid)) { gc <- xml_find_all(grid, "w:gridCol", ns); for (k in seq_along(gc)) xml_set_attr(gc[[k]], "w:w", tw[k]) }
    for (j in seq_along(rows)) {
      tr <- rows[[j]]; trpr <- first(tr, "w:trPr"); if (is.null(trpr)) trpr <- xml_add_child(tr, "w:trPr", .where = 0)
      if (j == 1) nd(trpr, "tblHeader"); nd(trpr, "cantSplit")
      for (k in seq_len(ncol)) {
        tc <- xml_find_all(tr, "w:tc", ns)[[k]]; tcpr <- first(tc, "w:tcPr"); if (is.null(tcpr)) tcpr <- xml_add_child(tc, "w:tcPr", .where = 0)
        for (nm in c("tcW", "shd")) { x <- first(tcpr, paste0("w:", nm)); if (!is.null(x)) xml_remove(x) }
        nd(tcpr, "tcW", w = tw[k], type = "dxa"); nd(tcpr, "shd", val = "clear", color = "auto", fill = if (j == 1) "E7EAED" else if (j %% 2 == 1) "F5F6F7" else "FFFFFF")
        for (p in xml_find_all(tc, "w:p", ns)) {
          ppr <- first(p, "w:pPr"); if (is.null(ppr)) ppr <- xml_add_child(p, "w:pPr", .where = 0)
          for (nm in c("spacing", "jc")) { x <- first(ppr, paste0("w:", nm)); if (!is.null(x)) xml_remove(x) }
          ppr_add(ppr, "spacing", before = "60", after = "60", line = "240", lineRule = "auto"); ppr_add(ppr, "jc", val = if (k == 1) "left" else "right")
          for (r in xml_find_all(p, "w:r", ns)) { rpr <- first(r, "w:rPr"); if (is.null(rpr)) rpr <- xml_add_child(r, "w:rPr", .where = 0)
            for (nm in c("sz", "szCs", "b")) { x <- first(rpr, paste0("w:", nm)); if (!is.null(x)) xml_remove(x) }
            if (j == 1) nd(rpr, "b"); nd(rpr, "sz", val = "20"); nd(rpr, "szCs", val = "20") }
        }
      }
    }
  }
  write_xml(doc, df)
  cp <- file.path(td, "docProps", "core.xml")
  if (file.exists(cp)) { core <- read_xml(cp); cns <- xml_ns(core); tt <- xml_find_first(core, "//dc:title", cns)
    if (inherits(tt, "xml_missing")) xml_add_child(core, "dc:title", title) else xml_set_text(tt, title)
    for (a in c("//dc:creator", "//cp:lastModifiedBy")) { x <- xml_find_first(core, a, cns); if (!inherits(x, "xml_missing")) xml_set_text(x, "") }
    write_xml(core, cp) }
  fl <- list.files(td, recursive = TRUE, all.files = TRUE); fl <- c("[Content_Types].xml", setdiff(fl, "[Content_Types].xml"))
  unlink(target); old <- setwd(td); on.exit(setwd(old), add = TRUE); utils::zip(target, fl, flags = "-X -q")
  invisible(target)
}
