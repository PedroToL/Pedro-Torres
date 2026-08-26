# src/_gen_teaching.R
# Generates the top-level teaching.qmd from src/teaching.csv.
# Run with: Rscript src/_gen_teaching.R
# DO NOT hand-edit teaching.qmd — edit teaching.csv or this script instead.

library(readr)
library(dplyr)
library(stringr)
library(glue)

script_dir <- "./src/"
project_root <- normalizePath(file.path(script_dir, ".."))

teaching <- read_tsv(
  file.path(script_dir, "teaching.csv"),
  show_col_types = FALSE,
  locale = locale(encoding = "UTF-8")
)

# --- NA-safe accessor: turns NA/blank cells into "" ---
safe <- function(x) ifelse(is.na(x) | str_trim(x) == "", "", str_trim(x))

# --- "A" / "A and B" / "A, B and C" — no Oxford comma ---
join_names <- function(names) {
  n <- length(names)
  if (n == 0) return("")
  if (n == 1) return(names)
  if (n == 2) return(paste(names, collapse = " and "))
  paste0(paste(names[1:(n - 1)], collapse = ", "), " and ", names[n])
}

# --- Escapes text for safe use inside HTML attributes ---
escape_html_attr <- function(x) {
  x <- str_replace_all(x, "&", "&amp;")
  x <- str_replace_all(x, '"', "&quot;")
  x <- str_replace_all(x, "<", "&lt;")
  x <- str_replace_all(x, ">", "&gt;")
  x
}

# --- Click-to-expand, one-line-ellipsis teaser (same style as research.qmd) ---
format_teaser_block <- function(text) {
  text <- safe(text)
  if (text == "") return(NULL)
  glue('<p class="abstract-teaser" onclick="toggleAbstract(this)">{escape_html_attr(text)}</p>')
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

    # Title (h2), clickable to the course page if a link exists
    title_text <- if (code != "") glue("{code} \u2013 {name}") else name
    heading <- if (link != "") glue("## [{title_text}]({link})") else glue("## {title_text}")
    body <- c(body, heading, "")

    # Info block: role, term, class teacher, institution course page link
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

    # Description teaser (click-to-expand)
    teaser <- format_teaser_block(p$description)
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

    # Title (h2), clickable to the slides/material link if it exists
    heading <- if (link != "") glue("## [{name}]({link})") else glue("## {name}")
    body <- c(body, heading, "")

    # Description teaser (click-to-expand)
    teaser <- format_teaser_block(p$description)
    if (!is.null(teaser)) body <- c(body, teaser, "")
  }
}

# ---------------------------------------------------------------------------
# HEADER (includes the toggle-abstract JS, same pattern as research.qmd)
# ---------------------------------------------------------------------------
header <- c(
  "---",
  "---",
  "",
  "<!-- GENERATED FILE — do not edit directly. Edit src/teaching.csv or ",
  "     src/_gen_teaching.R instead, then re-run Rscript src/_gen_teaching.R -->",
  "",
  "<script>",
  "function toggleAbstract(el) {",
  "  el.classList.toggle('expanded');",
  "}",
  "</script>",
  ""
)

writeLines(c(header, body), file.path(project_root, "teaching.qmd"))
cat("teaching.qmd written to", file.path(project_root, "teaching.qmd"), "\n")
