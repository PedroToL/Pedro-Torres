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

    # English gloss, italic, right under the title
    if (title_en != "") body <- c(body, glue("*{title_en}*"), "")

    # Info line: publication (date), linked
    venue_label <- if (publication != "" && date != "") glue("{publication} ({date})")
                    else if (publication != "") publication
                    else date
    venue_line <- if (link != "" && venue_label != "") glue("[{venue_label}]({link})") else venue_label
    if (venue_line != "") body <- c(body, venue_line, "")

    # Description (click-to-expand)
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
