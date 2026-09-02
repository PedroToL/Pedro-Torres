# src/_gen_teaching.R
# Generates the top-level teaching.qmd from src/teaching.csv.
# Run with: Rscript src/_gen_teaching.R
# DO NOT hand-edit teaching.qmd — edit teaching.csv or this script instead.

library(readr)
library(dplyr)
library(stringr)
library(glue)

script_dir <- "src"
project_root <- normalizePath(file.path(script_dir, ".."))

teaching <- read_tsv(
  file.path(script_dir, "teaching.csv"),
  show_col_types = FALSE,
  locale = locale(encoding = "UTF-8")
)

safe <- function(x) ifelse(is.na(x) | str_trim(x) == "", "", str_trim(x))

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

# Description block now carries an explicit id, so the title's click can target it
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
# COURSE TEACHING
# ---------------------------------------------------------------------------
courses <- teaching %>% filter(str_trim(type) == "course")
if (nrow(courses) > 0) {
  body <- c(body, "# Course Teaching", "")

  for (r in seq_len(nrow(courses))) {
    p <- courses[r, ]
    code <- safe(p$code); name <- safe(p$name); role <- safe(p$role)
    term <- safe(p$term); institution <- safe(p$institution)
    instructor <- safe(p$instructor); link <- safe(p$link)
    teaser_id <- glue("desc-{if (code != '') tolower(code) else make.names(name)}")
    has_desc <- safe(p$description) != ""

    # Title: plain heading, clickable to expand its description if one exists
    title_text <- if (code != "") glue("{code} \u2013 {name}") else name
    heading <- if (has_desc) {
      glue('<h2 class="expandable-title" onclick="toggleAbstract(\'{teaser_id}\')">{title_text}</h2>')
    } else {
      glue("## {title_text}")
    }
    body <- c(body, heading, "")

    # Info block: role, term, class teacher, institution course-page link
    info_lines <- character(0)
    if (role != "") info_lines <- c(info_lines, role)
    if (term != "") info_lines <- c(info_lines, term)
    if (instructor != "") {
      instr_names <- str_split(instructor, ",\\s*")[[1]]
      instr_names <- instr_names[instr_names != ""]
      info_lines <- c(info_lines, glue("Class teacher: {join_names(instr_names)}"))
    }
    if (institution != "" && link != "") {
      info_lines <- c(info_lines, glue("[{institution} course page]({link})"))
    }
    if (length(info_lines) > 0) {
      body <- c(body, paste(info_lines, collapse = "<br>"), "")
    }

    # Description (click-to-expand)
    teaser <- format_teaser_block(p$description, teaser_id)
    if (!is.null(teaser)) body <- c(body, teaser, "")
  }
}

# ---------------------------------------------------------------------------
# TEACHING MATERIALS
# ---------------------------------------------------------------------------
materials <- teaching %>% filter(str_trim(type) == "material")
if (nrow(materials) > 0) {
  body <- c(body, "# Teaching Materials", "")

  for (r in seq_len(nrow(materials))) {
    p <- materials[r, ]
    name <- safe(p$name); link <- safe(p$link)
    teaser_id <- glue("desc-{make.names(name)}")
    has_desc <- safe(p$description) != ""

    # Title: plain heading, clickable to expand its description if one exists
    heading <- if (has_desc) {
      glue('<h2 class="expandable-title" onclick="toggleAbstract(\'{teaser_id}\')">{name}</h2>')
    } else {
      glue("## {name}")
    }
    body <- c(body, heading, "")

    # Description (click-to-expand)
    teaser <- format_teaser_block(p$description, teaser_id)
    if (!is.null(teaser)) body <- c(body, teaser, "")

    # Two buttons: view in-browser vs. force a download of the same file
    if (link != "") {
      body <- c(
        body,
        glue(
          '<a href="{link}" target="_blank" class="btn btn-outline-secondary btn-sm">Open in Browser</a> ',
          '<a href="{link}" download class="btn btn-outline-secondary btn-sm">Download</a>'
        ),
        ""
      )
    }
  }
}

header <- c(
  "---",
  "---",
  "",
  "<!-- GENERATED FILE — do not edit directly. Edit src/teaching.csv or ",
  "     src/_gen_teaching.R instead, then re-run Rscript src/_gen_teaching.R -->",
  "",
  "<script>",
  "function toggleAbstract(id) {",
  "  var el = document.getElementById(id);",
  "  if (el) el.classList.toggle('expanded');",
  "}",
  "</script>",
  ""
)

writeLines(c(header, body), file.path(project_root, "teaching.qmd"))
cat("teaching.qmd written to", file.path(project_root, "teaching.qmd"), "\n")
