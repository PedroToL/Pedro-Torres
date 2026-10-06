# src/_gen_teaching.R
# Generates teaching.qmd from src/teaching.csv.
# Run from the project root with: Rscript src/_gen_teaching.R
# DO NOT hand-edit teaching.qmd — edit teaching.csv or this script instead.
#
# Row types in teaching.csv:
#   statement : a button in the "Teaching statement" block at the top
#               (name = button label, link = file, description = optional sentence).
#               The block only appears once at least one of its files exists.
#   course    : a course you taught (code, name, role, term, institution,
#               instructor = lecturers separated by commas, link = course page)
#   material  : teaching materials (name, link = file, description)

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(glue)
})

script_dir   <- "src"
project_root <- normalizePath(file.path(script_dir, ".."))

teaching <- read_tsv(
  file.path(script_dir, "teaching.csv"),
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
is_local_file <- function(link) !str_detect(link, "^(https?:|mailto:)")

html <- character(0)

# ---------------------------------------------------------------------------
# Teaching statement (only shown once a file exists)
# ---------------------------------------------------------------------------
statements <- teaching %>% filter(str_trim(type) == "statement")
buttons <- character(0)
sentence <- ""
for (i in seq_len(nrow(statements))) {
  s <- statements[i, ]
  link <- safe(s$link)
  if (link == "") next
  if (is_local_file(link) && !file.exists(file.path(project_root, link))) {
    message("Teaching statement file not found yet, button skipped: ", link)
    next
  }
  icon <- if (str_detect(tolower(s$name), "evaluation")) "bar-chart" else "file-earmark-text"
  buttons <- c(buttons, glue('<a class="pill" href="{esc(link)}" target="_blank"><i class="bi bi-{icon}"></i>{esc(s$name)}</a>'))
  if (sentence == "" && safe(s$description) != "") sentence <- safe(s$description)
}
if (length(buttons) > 0) {
  html <- c(html,
            '<h2 class="research-section">Teaching statement</h2>',
            '<div class="teach-statement">',
            if (sentence != "") glue('<p>{esc(sentence)}</p>') else "<p></p>",
            glue('<div class="paper-buttons">{paste(buttons, collapse = "")}</div>'),
            "</div>")
}

# ---------------------------------------------------------------------------
# Courses
# ---------------------------------------------------------------------------
courses <- teaching %>% filter(str_trim(type) == "course")
if (nrow(courses) > 0) {
  html <- c(html, '<h2 class="research-section">Courses</h2>')
  for (i in seq_len(nrow(courses))) {
    p <- courses[i, ]

    meta <- c(safe(p$role), safe(p$institution), safe(p$term))
    meta <- paste(meta[meta != ""], collapse = " · ")

    lecturers <- str_trim(str_split(safe(p$instructor), ",")[[1]])
    lecturers <- lecturers[lecturers != ""]
    lect_line <- ""
    if (length(lecturers) == 1) lect_line <- glue("Lecturer: {join_names(lecturers)}")
    if (length(lecturers) > 1)  lect_line <- glue("Lecturers: {join_names(lecturers)}")

    html <- c(html, '<div class="teach-entry">',
              glue('<div class="teach-code">{esc(safe(p$code))}</div>'),
              '<div>',
              glue('<h3 class="paper-title">{esc(safe(p$name))}</h3>'))
    if (meta != "")      html <- c(html, glue('<div class="paper-venue">{esc(meta)}</div>'))
    if (lect_line != "") html <- c(html, glue('<div class="paper-venue"><em>{esc(lect_line)}</em></div>'))
    if (safe(p$description) != "") html <- c(html, glue('<p class="teach-desc">{esc(p$description)}</p>'))
    if (safe(p$link) != "") {
      html <- c(html, glue('<div class="paper-buttons"><a class="pill" href="{esc(p$link)}" target="_blank"><i class="bi bi-box-arrow-up-right"></i>Course page</a></div>'))
    }
    html <- c(html, "</div>", "</div>")
  }
}

# ---------------------------------------------------------------------------
# Teaching materials
# ---------------------------------------------------------------------------
materials <- teaching %>% filter(str_trim(type) == "material")
if (nrow(materials) > 0) {
  html <- c(html, '<h2 class="research-section">Teaching materials</h2>')
  for (i in seq_len(nrow(materials))) {
    p <- materials[i, ]
    link <- safe(p$link)
    if (link != "" && is_local_file(link) && !file.exists(file.path(project_root, link))) {
      warning("Material file not found, buttons skipped: ", link, call. = FALSE)
      link <- ""
    }

    html <- c(html, '<div class="teach-entry">',
              '<div class="teach-code"><i class="bi bi-easel"></i></div>',
              '<div>',
              glue('<h3 class="paper-title">{esc(safe(p$name))}</h3>'))
    if (safe(p$description) != "") html <- c(html, glue('<p class="teach-desc">{esc(p$description)}</p>'))
    if (link != "") {
      open_btn <- glue('<a class="pill" href="{esc(link)}" target="_blank"><i class="bi bi-play-circle"></i>Open slides</a>')
      html <- c(html, glue('<div class="paper-buttons">{open_btn}'))
      if (is_local_file(link)) {
        html <- c(html, glue('<a class="pill" href="{esc(link)}" download="{basename(link)}" title="Self-contained file: opens offline in any browser, interactive elements included"><i class="bi bi-download"></i>Download (HTML)</a>'))
      }
      html <- c(html, "</div>")
    }
    html <- c(html, "</div>", "</div>")
  }
}

page <- c("---", 'pagetitle: "Teaching"', "---", "",
          "<!-- GENERATED FILE — do not edit directly. Edit src/teaching.csv or",
          "     src/_gen_teaching.R instead, then re-run Rscript src/_gen_teaching.R -->", "",
          "```{=html}", html, "```")
writeLines(enc2utf8(page), file.path(project_root, "teaching.qmd"), useBytes = TRUE)
cat("teaching.qmd written\n")
