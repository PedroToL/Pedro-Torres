# src/_gen_other.R
# Generates the top-level other.qmd from src/other.csv.
# Run with: Rscript src/_gen_other.R
# DO NOT hand-edit other.qmd — edit other.csv or this script instead.

library(readr)
library(dplyr)
library(stringr)
library(glue)

script_dir <- "src"
project_root <- normalizePath(file.path(script_dir, ".."))

items <- read_tsv(
  file.path(script_dir, "other.csv"),
  show_col_types = FALSE,
  locale = locale(encoding = "UTF-8")
)

safe <- function(x) ifelse(is.na(x) | str_trim(x) == "", "", str_trim(x))

# "A" / "A and B" / "A, B and C" — no Oxford comma (same convention as research/teaching)
join_names <- function(names) {
  n <- length(names)
  if (n == 0) return("")
  if (n == 1) return(names)
  if (n == 2) return(paste(names, collapse = " and "))
  paste0(paste(names[1:(n - 1)], collapse = ", "), " and ", names[n])
}

escape_html_attr <- function(x) {
  x <- str_replace_all(x, "&", "&amp;")
  x <- str_replace_all(x, '"', "&quot;")
  x <- str_replace_all(x, "<", "&lt;")
  x <- str_replace_all(x, ">", "&gt;")
  x
}

format_teaser_block <- function(text, teaser_id) {
  text <- safe(text)
  if (text == "") return(NULL)
  glue(
    '<p id="{teaser_id}" class="abstract-teaser" ',
    'onclick="toggleAbstract(\'{teaser_id}\')">{escape_html_attr(text)}</p>'
  )
}

body <- character(0)

# ---------------------------------------------------------------------------
# NON-ACADEMIC ARTICLES
# ---------------------------------------------------------------------------
articles <- items %>% filter(str_trim(type) == "article")
if (nrow(articles) > 0) {
  body <- c(body, "# Non-academic Articles", "")

  for (r in seq_len(nrow(articles))) {
    p <- articles[r, ]
    title <- safe(p$title); title_en <- safe(p$title_en); publication <- safe(p$publication)
    date <- safe(p$date); link <- safe(p$link)
    teaser_id <- glue("desc-article-{r}")
    has_desc <- safe(p$description) != ""

    # Title: plain heading, clickable to expand its description if one exists
    heading <- if (has_desc) {
      glue('<h2 class="entry-title expandable-title" onclick="toggleAbstract(\'{teaser_id}\')">{title}</h2>')
    } else {
      glue('<h2 class="entry-title">{title}</h2>')
    }
    body <- c(body, heading, "")

    # English gloss + venue link, joined tight into one paragraph
    info_lines <- character(0)
    if (title_en != "") info_lines <- c(info_lines, glue("*{title_en}*"))

    venue_label <- if (publication != "" && date != "") glue("{publication} ({date})")
                    else if (publication != "") publication
                    else date
    venue_line <- if (link != "" && venue_label != "") glue("[{venue_label}]({link})") else venue_label
    if (venue_line != "") info_lines <- c(info_lines, venue_line)

    if (length(info_lines) > 0) {
      body <- c(body, paste(info_lines, collapse = "<br>"), "")
    }

    # Description (click-to-expand)
    teaser <- format_teaser_block(p$description, teaser_id)
    if (!is.null(teaser)) body <- c(body, teaser, "")
  }
}

# ---------------------------------------------------------------------------
# CODE & DATA
# ---------------------------------------------------------------------------
code_items <- items %>% filter(str_trim(type) == "code")
if (nrow(code_items) > 0) {
  body <- c(body, "# Code & Data", "")

  for (r in seq_len(nrow(code_items))) {
    p <- code_items[r, ]
    title <- safe(p$title); link <- safe(p$link); coauthors <- safe(p$coauthors)
    teaser_id <- glue("desc-code-{r}")
    has_desc <- safe(p$description) != ""

    heading <- if (has_desc) {
      glue('<h2 class="entry-title expandable-title" onclick="toggleAbstract(\'{teaser_id}\')">{title}</h2>')
    } else {
      glue('<h2 class="entry-title">{title}</h2>')
    }
    body <- c(body, heading, "")

    info_lines <- character(0)
    if (coauthors != "") {
      names <- str_split(coauthors, ",\\s*")[[1]]
      names <- names[names != ""]
      info_lines <- c(info_lines, glue("with {join_names(names)}"))
    }
    if (link != "") info_lines <- c(info_lines, glue("[GitHub repository]({link})"))
    if (length(info_lines) > 0) {
      body <- c(body, paste(info_lines, collapse = "<br>"), "")
    }

    teaser <- format_teaser_block(p$description, teaser_id)
    if (!is.null(teaser)) body <- c(body, teaser, "")
  }
}

# ---------------------------------------------------------------------------
# MUSIC & AUDIO
# ---------------------------------------------------------------------------
media_items <- items %>% filter(str_trim(type) == "media")
if (nrow(media_items) > 0) {
  body <- c(body, "# Music & Audio", "")

  for (r in seq_len(nrow(media_items))) {
    p <- media_items[r, ]
    title <- safe(p$title); link <- safe(p$link); role <- safe(p$role)
    episodes_raw <- safe(p$episodes)
    teaser_id <- glue("desc-media-{r}")
    has_desc <- safe(p$description) != ""

    heading <- if (has_desc) {
      glue('<h2 class="entry-title expandable-title" onclick="toggleAbstract(\'{teaser_id}\')">{title}</h2>')
    } else {
      glue('<h2 class="entry-title">{title}</h2>')
    }
    body <- c(body, heading, "")

    info_lines <- character(0)
    if (role != "") info_lines <- c(info_lines, role)
    if (link != "") info_lines <- c(info_lines, glue("[Instagram]({link})"))

    # Episodes: "Label|url;Label|url" -> linked list, joined into the same tight block
    if (episodes_raw != "") {
      ep_pairs <- str_split(episodes_raw, ";\\s*")[[1]]
      ep_lines <- sapply(ep_pairs, function(ep) {
        parts <- str_split(ep, "\\|")[[1]]
        if (length(parts) == 2) glue("[{parts[1]}]({parts[2]})") else ep
      })
      info_lines <- c(info_lines, ep_lines)
    }

    if (length(info_lines) > 0) {
      body <- c(body, paste(info_lines, collapse = "<br>"), "")
    }

    teaser <- format_teaser_block(p$description, teaser_id)
    if (!is.null(teaser)) body <- c(body, teaser, "")
  }
}

header <- c(
  "---",
  "---",
  "",
  "<!-- GENERATED FILE — do not edit directly. Edit src/other.csv or ",
  "     src/_gen_other.R instead, then re-run Rscript src/_gen_other.R -->",
  "",
  "<script>",
  "function toggleAbstract(id) {",
  "  var el = document.getElementById(id);",
  "  if (el) el.classList.toggle('expanded');",
  "}",
  "</script>",
  ""
)

writeLines(c(header, body), file.path(project_root, "other.qmd"))
cat("other.qmd written to", file.path(project_root, "other.qmd"), "\n")
