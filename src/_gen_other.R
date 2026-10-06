# src/_gen_other.R
# Generates other.qmd from src/other.csv.
# Run from the project root with: Rscript src/_gen_other.R
# DO NOT hand-edit other.qmd — edit other.csv or this script instead.
#
# Row types in other.csv (rows appear in the order they are in the file):
#   article : title (original language), title_en (optional translation),
#             publication, date ("Month Year"), link, description
#   code    : title, link, description, coauthors (separated by commas)
#   media   : title, role, description, link, and episodes as
#             "Episode 017 – Mumbai|url;Episode 008 – London|url"

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(glue)
})

script_dir   <- "src"
project_root <- normalizePath(file.path(script_dir, ".."))

other <- read_tsv(
  file.path(script_dir, "other.csv"),
  show_col_types = FALSE,
  col_types = cols(.default = col_character()),
  locale = locale(encoding = "UTF-8")
)

safe <- function(x) ifelse(is.na(x) | str_trim(x) == "", "", str_trim(x))
esc  <- function(x) {
  x <- str_replace_all(x, "&", "&amp;"); x <- str_replace_all(x, "<", "&lt;")
  x <- str_replace_all(x, ">", "&gt;");  str_replace_all(x, '"', "&quot;")
}
join_names <- function(names) {
  n <- length(names)
  if (n == 0) return("")
  if (n == 1) return(names)
  if (n == 2) return(paste(names, collapse = " and "))
  paste0(paste(names[1:(n - 1)], collapse = ", "), " and ", names[n])
}
pill <- function(url, label, icon) {
  glue('<a class="pill" href="{esc(url)}" target="_blank"><i class="bi bi-{icon}"></i>{esc(label)}</a>')
}

html <- character(0)

# ---------------------------------------------------------------------------
# Writing
# ---------------------------------------------------------------------------
articles <- other %>% filter(str_trim(type) == "article")
if (nrow(articles) > 0) {
  html <- c(html, '<h2 class="research-section">Writing</h2>')
  for (i in seq_len(nrow(articles))) {
    p <- articles[i, ]

    # "August 2026" becomes "Aug<br>2026" in the left column
    date_parts <- str_split(safe(p$date), " ")[[1]]
    date_col <- if (length(date_parts) == 2) glue("{str_sub(date_parts[1], 1, 3)}<br>{date_parts[2]}") else esc(safe(p$date))

    venue <- glue("<em>{esc(safe(p$publication))}</em>")
    if (safe(p$title_en) != "") venue <- glue("{venue} · in Spanish")

    html <- c(html, '<div class="teach-entry">',
              glue('<div class="teach-code">{date_col}</div>'),
              "<div>",
              glue('<h3 class="paper-title">{esc(safe(p$title))}</h3>'))
    if (safe(p$title_en) != "") html <- c(html, glue('<div class="other-en">{esc(p$title_en)}</div>'))
    html <- c(html, glue('<div class="paper-venue">{venue}</div>'))
    if (safe(p$description) != "") html <- c(html, glue('<p class="teach-desc">{esc(p$description)}</p>'))
    if (safe(p$link) != "") html <- c(html, glue('<div class="paper-buttons">{pill(p$link, "Read", "box-arrow-up-right")}</div>'))
    html <- c(html, "</div>", "</div>")
  }
}

# ---------------------------------------------------------------------------
# Code & data
# ---------------------------------------------------------------------------
code <- other %>% filter(str_trim(type) == "code")
if (nrow(code) > 0) {
  html <- c(html, '<h2 class="research-section">Code &amp; data</h2>')
  for (i in seq_len(nrow(code))) {
    p <- code[i, ]
    on_github <- str_detect(safe(p$link), "github\\.com")
    icon <- if (on_github) "github" else "code-slash"

    coauthors <- str_trim(str_split(safe(p$coauthors), ",")[[1]])
    coauthors <- coauthors[coauthors != ""]

    html <- c(html, '<div class="teach-entry">',
              glue('<div class="teach-code"><i class="bi bi-{icon}"></i></div>'),
              "<div>",
              glue('<h3 class="paper-title">{esc(safe(p$title))}</h3>'))
    if (length(coauthors) > 0) html <- c(html, glue('<div class="paper-venue"><em>With {esc(join_names(coauthors))}</em></div>'))
    if (safe(p$description) != "") html <- c(html, glue('<p class="teach-desc">{esc(p$description)}</p>'))
    if (safe(p$link) != "") {
      label <- if (on_github) "GitHub" else "Code"
      html <- c(html, glue('<div class="paper-buttons">{pill(p$link, label, icon)}</div>'))
    }
    html <- c(html, "</div>", "</div>")
  }
}

# ---------------------------------------------------------------------------
# Music & audio
# ---------------------------------------------------------------------------
media <- other %>% filter(str_trim(type) == "media")
if (nrow(media) > 0) {
  html <- c(html, '<h2 class="research-section">Music &amp; audio</h2>')
  for (i in seq_len(nrow(media))) {
    p <- media[i, ]

    buttons <- character(0)
    episodes <- str_trim(str_split(safe(p$episodes), ";")[[1]])
    for (e in episodes[episodes != ""]) {
      parts <- str_split(e, "\\|")[[1]]
      if (length(parts) < 2) next
      label <- str_trim(parts[1])
      # "Episode 017 – Mumbai" becomes "017 · Mumbai"
      label <- str_replace(label, "^Episode\\s+(\\S+)\\s*[–-]\\s*", "\\1 · ")
      buttons <- c(buttons, pill(str_trim(parts[2]), label, "play-circle"))
    }
    link <- safe(p$link)
    if (link != "") {
      if (str_detect(link, "instagram\\.com")) {
        buttons <- c(buttons, pill(link, "Instagram", "instagram"))
      } else {
        buttons <- c(buttons, pill(link, "Website", "box-arrow-up-right"))
      }
    }

    html <- c(html, '<div class="teach-entry">',
              '<div class="teach-code"><i class="bi bi-broadcast"></i></div>',
              "<div>",
              glue('<h3 class="paper-title">{esc(safe(p$title))}</h3>'))
    if (safe(p$role) != "") html <- c(html, glue('<div class="paper-venue">{esc(p$role)}</div>'))
    if (safe(p$description) != "") html <- c(html, glue('<p class="teach-desc">{esc(p$description)}</p>'))
    if (length(buttons) > 0) html <- c(html, glue('<div class="paper-buttons">{paste(buttons, collapse = "")}</div>'))
    html <- c(html, "</div>", "</div>")
  }
}

page <- c("---", 'pagetitle: "Other"', "---", "",
          "<!-- GENERATED FILE — do not edit directly. Edit src/other.csv or",
          "     src/_gen_other.R instead, then re-run Rscript src/_gen_other.R -->", "",
          "```{=html}", html, "```")
writeLines(enc2utf8(page), file.path(project_root, "other.qmd"), useBytes = TRUE)
cat("other.qmd written\n")
