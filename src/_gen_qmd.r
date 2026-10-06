# src/_gen_qmd.R
# Generates, from src/papers.csv:
#   - research.qmd               : the research page (all papers, abstracts expand in place)
#   - research/<jmp slug>.qmd    : a dedicated page for the job market paper only
#   - _jmp-card.qmd              : the JMP card shown on the About page
# Run from the project root with: Rscript src/_gen_qmd.R
# DO NOT hand-edit the generated files — edit papers.csv or this script instead.

suppressPackageStartupMessages({
library(readr)
library(dplyr)
library(stringr)
library(glue)
})

script_dir   <- "src"
project_root <- normalizePath(file.path(script_dir, ".."))
papers_dir   <- file.path(project_root, "research")
site_url     <- "https://pedrotol.github.io/Pedro-Torres/"   # used for full links in citations


papers <- read_tsv(
  file.path(script_dir, "papers.csv"),
  show_col_types = FALSE,
  col_types = cols(.default = col_character()),
  locale = locale(encoding = "UTF-8")
)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------
safe <- function(x) ifelse(is.na(x) | str_trim(x) == "", "", str_trim(x))
esc  <- function(x) {                       # HTML-escape text
  x <- str_replace_all(x, "&", "&amp;"); x <- str_replace_all(x, "<", "&lt;")
  x <- str_replace_all(x, ">", "&gt;");  str_replace_all(x, '"', "&quot;")
}
yq   <- function(x) paste0("'", str_replace_all(x, "'", "''"), "'")   # YAML single-quote
link <- function(url, text) glue('<a href="{esc(url)}">{text}</a>')
# TRUE for a file in this project (e.g. files/paper.pdf), FALSE for a web address
is_local_file <- function(x) x != "" && !str_detect(x, "^(https?:|mailto:)")

join_names <- function(names) {
  n <- length(names)
  if (n == 0) return("")
  if (n == 1) return(names)
  if (n == 2) return(paste(names, collapse = " and "))
  paste0(paste(names[1:(n - 1)], collapse = ", "), " and ", names[n])
}
split_list <- function(x, sep = ",\\s*") {
  x <- safe(x); if (x == "") return(character(0))
  out <- str_trim(str_split(x, sep)[[1]]); out[out != ""]
}
truncate_words <- function(x, n = 280) {
  x <- str_squish(x)
  if (nchar(x) <= n) return(x)
  cut <- str_sub(x, 1, n)
  # Prefer ending at a full sentence, if one ends in the second half of the cut
  sentence_ends <- str_locate_all(cut, "[.!?](?=\\s)")[[1]][, "end"]
  if (length(sentence_ends) > 0 && max(sentence_ends) > n / 2) {
    return(str_sub(cut, 1, max(sentence_ends)))
  }
  cut <- str_replace(cut, "\\s+\\S*$", "")
  paste0(str_replace(cut, "[[:punct:]]+$", ""), "\u2026")
}

# --- Sections, in page order ----------------------------------------------
sections <- tibble(
  type  = c("job market paper", "published", "working paper", "book chapter", "work in progress"),
  title = c("Job Market Paper", "Publications", "Working Papers", "Book Chapters", "Work in Progress"),
  id    = c("jmp", "publications", "working-papers", "book-chapters", "work-in-progress")
)

papers <- papers %>%
  mutate(type = str_trim(type),
         order_sec = match(type, sections$type),
         year_num = suppressWarnings(parse_number(date))) %>%
  filter(!is.na(order_sec)) %>%
  arrange(order_sec, desc(year_num))

# ---------------------------------------------------------------------------
# Citations: "authors" column is "Last, First; Last, First" in publication order
# ---------------------------------------------------------------------------
parse_authors <- function(p) {
  a <- split_list(p$authors, ";\\s*")
  if (length(a) == 0) {
    warning("No 'authors' for ", p$slug, ": citation uses Torres López first, then co-authors.", call. = FALSE)
    co <- split_list(p$`co-authors`)
    a <- c("Torres López, Pedro J.",
           vapply(co, function(n) {w <- str_split(n, " ")[[1]]; paste0(tail(w, 1), ", ", paste(head(w, -1), collapse = " "))}, ""))
  }
  tibble(last = str_trim(str_extract(a, "^[^,]+")), first = str_trim(str_replace(a, "^[^,]+,?", "")))
}
initials <- function(first) {
  l <- str_extract_all(first, "(?<!\\p{L})\\p{L}")[[1]]
  if (length(l) == 0) "" else paste0(l, ".", collapse = " ")
}
cite_authors_text <- function(au) {
  names <- ifelse(au$first == "", au$last, paste0(au$last, ", ", vapply(au$first, initials, "")))
  n <- length(names)
  if (n > 6) return(paste0(names[1], " et al."))
  if (n == 1) return(names)
  if (n == 2) return(paste(names, collapse = " and "))
  paste0(paste(names[1:(n - 1)], collapse = ", "), ", and ", names[n])
}
bib_key <- function(au, year, title) {
  last <- tolower(iconv(str_extract(au$last[1], "^[^ ]+"), "UTF-8", "ASCII//TRANSLIT"))
  stop_w <- c("a", "an", "the", "of", "in", "on", "and", "to", "for", "new")
  words <- tolower(str_extract_all(iconv(title, "UTF-8", "ASCII//TRANSLIT"), "[A-Za-z]+")[[1]])
  paste0(str_replace_all(last, "[^a-z]", ""), year, setdiff(words, stop_w)[1])
}
make_citation <- function(p) {
  au <- parse_authors(p)
  year <- safe(p$date); title <- safe(p$title); venue <- safe(p$journal)
  vol <- safe(p$volume); iss <- safe(p$issue); pg <- safe(p$pages)
  doi <- safe(p$doi); url <- if (doi != "") paste0("https://doi.org/", doi) else safe(p$link)
  if (is_local_file(url)) url <- paste0(site_url, url)
  editors <- split_list(p$`other info`)
  pg_txt <- str_replace(pg, "--", "\u2013")

  text <- glue("{cite_authors_text(au)} ({year}). {title}.")
  text <- switch(p$type,
    "published" = paste0(text, " <em>", venue, "</em>",
                         if (vol != "") paste0(", ", vol) else "",
                         if (iss != "") paste0("(", iss, ")") else "",
                         if (pg != "") paste0(", ", pg_txt) else "", "."),
    "book chapter" = paste0(text, " In ", join_names(editors),
                            if (length(editors) > 1) " (Eds.), " else " (Ed.), ",
                            "<em>", venue, "</em>."),
    "working paper" = paste0(text, " ", venue, "."),
    "job market paper" = paste0(text, " Job market paper, London School of Economics."),
    text)
  if (doi != "") text <- paste0(text, " ", link(url, url))

  f <- c(author = paste(ifelse(au$first == "", au$last, paste0(au$last, ", ", au$first)), collapse = " and "),
         title = title)
  bibtype <- switch(p$type, "published" = "article", "book chapter" = "incollection",
                    "working paper" = "techreport", "unpublished")
  if (p$type == "published")     f <- c(f, journal = venue, volume = vol, number = iss, pages = pg)
  if (p$type == "book chapter")  f <- c(f, booktitle = venue, editor = paste(editors, collapse = " and "))
  if (p$type == "working paper") f <- c(f, institution = venue, type = "Working Paper")
  if (p$type == "job market paper") f <- c(f, note = "Job market paper, London School of Economics")
  f <- c(f, year = year, doi = doi, url = if (doi == "") url else "")
  f <- f[f != ""]
  width <- max(nchar(names(f)))
  body <- paste0("  ", str_pad(names(f), width, "right"), " = {", f, "}", collapse = ",\n")
  bib <- glue("@{bibtype}{{{bib_key(au, year, title)},\n{body}\n}}")
  list(text = text, bib = bib)
}

# ---------------------------------------------------------------------------
# Venue line (shared by the card and the paper page) and buttons
# ---------------------------------------------------------------------------
venue_html <- function(p) {
  date <- safe(p$date); journal <- safe(p$journal)
  doi <- safe(p$doi); url <- if (doi != "") paste0("https://doi.org/", doi) else safe(p$link)
  label <- switch(p$type,
    "job market paper" = if (safe(p$latest_version) != "") glue("Latest version: {safe(p$latest_version)}") else glue("Job market paper ({date})"),
    "work in progress" = "",
    "book chapter"     = glue("in <em>{esc(journal)}</em> ({date})"),
    glue("{esc(journal)} ({date})"))
  if (label != "" && url != "" && p$type != "job market paper") label <- link(url, label)

  co <- split_list(p$`co-authors`)
  co_txt <- if (length(co) == 0) "" else if (length(co) <= 5) paste("with", join_names(co)) else glue("with {length(co)} co-authors")
  line1 <- paste(c(label, if (co_txt != "") glue("<em>{esc(co_txt)}</em>")), collapse = ", ")
  line1 <- str_replace(line1, "^, ", "")

  other <- split_list(p$`other info`)
  line2 <- if (length(other) == 0) "" else if (p$type == "book chapter") glue("<em>Edited by {esc(join_names(other))}</em>") else if (length(co) > 5) glue("<em>Coordinated by {esc(join_names(other))}</em>") else ""
  paste(c(line1, line2)[c(line1, line2) != ""], collapse = "<br>")
}

pill <- function(url, label, icon, extra = "") glue('<a class="pill" href="{esc(url)}" {extra}><i class="bi bi-{icon}"></i>{label}</a>')


# Buttons for one paper. "prefix" is how to reach the project root from the
# page being written: "" for research.qmd, "../" for research/<slug>.qmd.
buttons_html <- function(p, prefix) {
  doi <- safe(p$doi)
  main <- if (doi != "") paste0("https://doi.org/", doi) else safe(p$link)
  if (is_local_file(main)) {
    if (file.exists(file.path(project_root, main))) {
      main <- paste0(prefix, main)
    } else {
      warning("Paper file not found, button skipped: ", main, call. = FALSE)
      main <- ""
    }
  }

  slides <- safe(p$slides)
  if (slides != "" && !file.exists(file.path(project_root, slides))) {
    warning("Slides file not found, buttons skipped: ", slides, call. = FALSE)
    slides <- ""
  }

  b <- character(0)
  if (main != "") {
    if (p$type == "job market paper") b <- c(b, pill(main, "PDF", "file-earmark-pdf", 'target="_blank"'))
    if (p$type == "published")        b <- c(b, pill(main, "Journal", "journal-text", 'target="_blank"'))
    if (p$type == "working paper")    b <- c(b, pill(main, "Paper", "file-earmark-text", 'target="_blank"'))
    if (p$type == "book chapter")     b <- c(b, pill(main, "Paper", "file-earmark-text", 'target="_blank"'))
  }
  if (safe(p$wp_link) != "") {
    b <- c(b, pill(p$wp_link, "Working paper", "file-earmark-text", 'target="_blank"'))
  }
  if (slides != "") {
    s <- paste0(prefix, slides)
    b <- c(b, pill(s, "Slides", "easel", 'target="_blank"'))
    b <- c(b, pill(s, "Slides (HTML)", "download",
                   glue('download="{basename(slides)}" title="Self-contained file: opens offline in any browser, interactive elements included"')))
  }
  if (safe(p$code_link) != "") {
    b <- c(b, pill(p$code_link, "Code", "github", 'target="_blank"'))
  }
  b <- c(b, '<button class="pill" type="button" data-toggle-cite aria-expanded="false"><i class="bi bi-quote"></i>Cite</button>')
  paste0('<div class="paper-buttons">', paste(b, collapse = "\n"), "</div>")
}

cite_html <- function(cit) glue('
<div class="cite-panel" hidden>
<div class="cite-tabs"><button type="button" class="cite-tab active" data-cite-tab="text">Text</button><button type="button" class="cite-tab" data-cite-tab="bibtex">BibTeX</button><button type="button" class="cite-copy"><i class="bi bi-clipboard"></i> Copy</button></div>
<p class="cite-text" data-cite-body="text">{cit$text}</p>
<pre class="cite-bibtex" data-cite-body="bibtex" hidden>{esc(cit$bib)}</pre>
</div>')

# Abstract that shows a teaser and expands in place ("more" / "less").
abstract_html <- function(abstract) {
  full <- str_squish(safe(abstract))
  if (full == "") return("")
  short <- truncate_words(full, 280)
  if (short == full) return(glue('<p class="paper-abstract">{esc(full)}</p>'))
  glue('<p class="paper-abstract"><span data-abstract-short>{esc(short)}</span>',
       '<span data-abstract-full hidden>{esc(full)}</span> ',
       '<button type="button" class="abstract-more" data-toggle-abstract aria-expanded="false">Read more <i class="bi bi-chevron-down"></i></button></p>')
}

# ---------------------------------------------------------------------------
# The research page: every paper, grouped by section
# ---------------------------------------------------------------------------
jmp_slug <- papers$slug[papers$type == "job market paper"][1]
jmp_page <- if (is.na(jmp_slug)) "" else paste0("research/", jmp_slug, ".html")

html <- character(0)
for (s in seq_len(nrow(sections))) {
  section_papers <- papers %>% filter(type == sections$type[s])
  if (nrow(section_papers) == 0) next
  html <- c(html, glue('<h2 class="research-section" id="{sections$id[s]}">{sections$title[s]}</h2>'))

  for (i in seq_len(nrow(section_papers))) {
    p <- section_papers[i, ]
    is_jmp <- p$type == "job market paper"
    is_wip <- p$type == "work in progress"

    image <- safe(p$image)
    if (image != "" && !file.exists(file.path(project_root, image))) {
      warning("Image not found, entry shown without it: ", image, call. = FALSE)
      image <- ""
    }

    # Only the JMP links to its own page
    title <- esc(p$title)
    if (is_jmp) title <- glue('<a href="{jmp_page}">{title}</a>')

    entry_class <- if (image == "") "paper-entry no-image" else "paper-entry"
    html <- c(html, glue('<div class="{entry_class}" data-paper>'))
    if (image != "") {
      img <- glue('<img src="{image}" alt="" loading="lazy">')
      if (is_jmp) img <- glue('<a href="{jmp_page}" tabindex="-1" aria-hidden="true">{img}</a>')
      html <- c(html, glue('<div class="paper-thumb">{img}</div>'))
    }
    html <- c(html, '<div class="paper-body">', glue('<h3 class="paper-title">{title}</h3>'))
    # A translated title (e.g. English for a paper published in Spanish)
    if (safe(p$subtitle) != "") html <- c(html, glue('<div class="other-en">{esc(p$subtitle)}</div>'))
    venue <- venue_html(p)
    if (venue != "") html <- c(html, glue('<div class="paper-venue">{venue}</div>'))
    html <- c(html, abstract_html(p$abstract))
    if (!is_wip) {
      html <- c(html, buttons_html(p, prefix = ""), cite_html(make_citation(p)))
    }
    html <- c(html, "</div>", "</div>")
  }
}

page <- c("---", 'pagetitle: "Research"', "---", "",
          "<!-- GENERATED FILE — do not edit directly. Edit src/papers.csv or",
          "     src/_gen_qmd.R instead, then re-run Rscript src/_gen_qmd.R -->", "",
          "```{=html}", html, '<script src="files/research.js"></script>', "```")
writeLines(enc2utf8(page), file.path(project_root, "research.qmd"), useBytes = TRUE)
cat("research.qmd written\n")

# ---------------------------------------------------------------------------
# The JMP page: research/<jmp slug>.qmd
# ---------------------------------------------------------------------------
dir.create(papers_dir, showWarnings = FALSE)

# Remove previously generated paper pages (e.g. from an older version of this
# script). Hand-written files in research/ are never touched.
old <- list.files(papers_dir, "\\.qmd$", full.names = TRUE)
for (f in old) {
  if (any(grepl("GENERATED FILE", readLines(f, n = 20, warn = FALSE)))) file.remove(f)
}

if (!is.na(jmp_slug)) {
  p <- papers %>% filter(slug == jmp_slug)
  p <- p[1, ]

  image <- safe(p$image)
  if (image != "" && !file.exists(file.path(project_root, image))) image <- ""

  fm <- c("---", paste("title:", yq(p$title)))
  if (safe(p$subtitle) != "") fm <- c(fm, paste("subtitle:", yq(p$subtitle)))
  if (image != "") fm <- c(fm, paste("image:", yq(paste0("../", image))))
  fm <- c(fm, "open-graph:",
          paste("  description:", yq(truncate_words(safe(p$abstract), 160))),
          "lightbox: true", "---", "",
          "<!-- GENERATED FILE — do not edit directly. Edit src/papers.csv or",
          "     src/_gen_qmd.R instead, then re-run Rscript src/_gen_qmd.R -->", "")

  body <- c("```{=html}", "<div data-paper>")
  venue <- venue_html(p)
  if (venue != "") body <- c(body, glue('<div class="paper-venue">{venue}</div>'))
  body <- c(body, buttons_html(p, prefix = "../"), cite_html(make_citation(p)), "</div>", "```", "")

  if (safe(p$abstract) != "") {
    body <- c(body, "## Abstract {.paper-heading}", "", str_squish(p$abstract), "")
  }

  figs <- split_list(p$figures, ";\\s*")
  if (length(figs) > 0) {
    body <- c(body, "## Figures {.paper-heading}", "", glue("::: {{layout-ncol={min(3, length(figs))}}}"))
    for (f in figs) {
      parts <- str_split(f, "\\|")[[1]]
      caption <- if (length(parts) > 1) str_trim(parts[2]) else ""
      body <- c(body, glue('![{caption}](../{str_trim(parts[1])}){{group="figures"}}'), "")
    }
    body <- c(body, ":::", "")
  }
  if (safe(p$extra) != "") body <- c(body, safe(p$extra), "")

  body <- c(body, "```{=html}",
            '<nav class="paper-nav"><a href="../research.html"><i class="bi bi-arrow-left"></i> All research</a></nav>',
            '<script src="../files/research.js"></script>', "```")
  writeLines(enc2utf8(c(fm, body)), file.path(papers_dir, paste0(jmp_slug, ".qmd")), useBytes = TRUE)
  cat("JMP page written:", file.path("research", paste0(jmp_slug, ".qmd")), "\n")
}

# ---------------------------------------------------------------------------
# Job market paper card for the About page: written to _jmp-card.qmd and
# pulled into index.qmd with {{< include _jmp-card.qmd >}}.
# Uses the "pitch" column (one sentence); falls back to a short teaser.
# ---------------------------------------------------------------------------
jmp <- papers %>% filter(type == "job market paper")
card <- character(0)
if (nrow(jmp) > 0) {
  p <- jmp[1, ]
  pitch <- safe(p$pitch)
  if (pitch == "") pitch <- truncate_words(safe(p$abstract), 200)

  buttons <- character(0)
  pdf <- safe(p$link)
  if (is_local_file(pdf) && !file.exists(file.path(project_root, pdf))) pdf <- ""
  if (pdf != "") {
    buttons <- c(buttons, pill(pdf, "PDF", "file-earmark-pdf", 'target="_blank"'))
  }
  slides <- safe(p$slides)
  if (slides != "" && file.exists(file.path(project_root, slides))) {
    buttons <- c(buttons, pill(slides, "Slides", "easel", 'target="_blank"'))
  }
  if (safe(p$code_link) != "") {
    buttons <- c(buttons, pill(p$code_link, "Code", "github", 'target="_blank"'))
  }

  card <- c("```{=html}",
            '<div class="jmp-card">',
            '<div class="jmp-text">',
            glue('<p class="jmp-title">{esc(p$title)}</p>'),
            glue('<p class="jmp-pitch">{esc(pitch)}</p>'),
            "</div>",
            glue('<div class="jmp-buttons">{paste(buttons, collapse = "")}</div>'),
            "</div>",
            "```")
}
writeLines(enc2utf8(c("<!-- GENERATED FILE by src/_gen_qmd.R from papers.csv. Do not edit. -->", "", card)),
           file.path(project_root, "_jmp-card.qmd"), useBytes = TRUE)
cat("_jmp-card.qmd written\n")
