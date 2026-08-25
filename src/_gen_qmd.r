# src/_gen_qmd.R
# Generates the top-level research.qmd from src/papers.csv.
# Run with: Rscript src/_gen_qmd.R
# DO NOT hand-edit research.qmd — edit papers.csv or this script instead.

library(readr)
library(dplyr)
library(stringr)
library(glue)

# ---------------------------------------------------------------------------
# PATH HANDLING
# Resolves the script's own folder, then treats its parent as the project
# root — so this works whether you run it from the project root or from
# inside src/.
# ---------------------------------------------------------------------------
script_dir <- "./src/"
project_root <- normalizePath(file.path(script_dir, ".."))

papers <- read_tsv(
  file.path(script_dir, "papers.csv"),
  show_col_types = FALSE,
  locale = locale(encoding = "UTF-8")
)

# ---------------------------------------------------------------------------
# HELPERS
# ---------------------------------------------------------------------------

# Turns NA / blank cells into "" so glue() never prints literal "NA"
safe <- function(x) ifelse(is.na(x) | str_trim(x) == "", "", str_trim(x))

# "A" / "A and B" / "A, B and C" — no Oxford comma
join_names <- function(names) {
  n <- length(names)
  if (n == 0) return("")
  if (n == 1) return(names)
  if (n == 2) return(paste(names, collapse = " and "))
  paste0(paste(names[1:(n - 1)], collapse = ", "), " and ", names[n])
}

# Co-author fragment: "with A and B" (<=5 names) or "with N co-authors"
format_coauthor_frag <- function(co_authors) {
  co_authors <- safe(co_authors)
  if (co_authors == "") return(NULL)
  names <- str_split(co_authors, ",\\s*")[[1]]
  n <- length(names)
  if (n <= 5) glue("with {join_names(names)}") else glue("with {n} co-authors")
}

# Venue + year as one clickable fragment (or plain text if no URL exists)
format_venue_line <- function(type, journal, link, doi, date) {
  journal <- safe(journal); link <- safe(link); doi <- safe(doi); date <- safe(date)
  url <- if (doi != "") glue("https://doi.org/{doi}") else if (link != "") link else ""

  if (type == "work in progress") return("Work in Progress")

  if (type == "job market paper") {
    return(if (url != "") glue("[Latest version]({url})") else "Latest version")
  }

  label_text <- case_when(
    type == "published"     ~ journal,
    type == "book chapter"  ~ glue("in *{journal}*"),
    type == "working paper" ~ journal,
    TRUE ~ journal
  )
  label <- if (label_text != "" && date != "") glue("{label_text} ({date})")
            else if (label_text != "") label_text
            else glue("({date})")

  if (url != "") glue("[{label}]({url})") else label
}

# Escapes text going into HTML data-* attributes
escape_html_attr <- function(x) {
  x <- str_replace_all(x, "&", "&amp;")
  x <- str_replace_all(x, '"', "&quot;")
  x <- str_replace_all(x, "<", "&lt;")
  x <- str_replace_all(x, ">", "&gt;")
  x
}

# Click-to-expand abstract teaser. Short abstracts render as a plain <p>;
# long ones render as a clickable <p> that swaps teaser <-> full text via JS.
format_abstract_block <- function(abstract) {
  abstract <- safe(abstract)
  if (abstract == "") return(NULL)
  glue('<p class="abstract-teaser" onclick="toggleAbstract(this)">{escape_html_attr(abstract)}</p>')
}

# ---------------------------------------------------------------------------
# SECTION SETUP
# ---------------------------------------------------------------------------
sections <- c("job market paper", "published", "working paper",
              "book chapter", "work in progress")
section_titles <- c("Job Market Paper", "Publications", "Working Papers",
                     "Book Chapters", "Work in Progress")

body <- character(0)

for (i in seq_along(sections)) {
  sec_papers <- papers %>%
    filter(str_trim(type) == sections[i]) %>%
    arrange(desc(suppressWarnings(parse_number(as.character(date)))))

  if (nrow(sec_papers) == 0) next

  body <- c(body, glue("# {section_titles[i]}"), "")

  for (r in seq_len(nrow(sec_papers))) {
    p <- sec_papers[r, ]
    ptype <- str_trim(p$type)

    # --- H2 title, linked unless work in progress ---
    title_txt <- if (ptype == "work in progress") {
      glue("## {p$title}")
    } else {
      glue("## [{p$title}]({p$slug}.html)")
    }
    body <- c(body, title_txt, "")

    # --- Info block: subtitle + venue/authors + coordination/editor,
    #     joined with <br> into ONE tight paragraph ---
    info_lines <- character(0)

    subtitle <- safe(p$subtitle)
    if (subtitle != "") info_lines <- c(info_lines, glue("*{subtitle}*"))

    venue_frag <- format_venue_line(ptype, p$journal, p$link, p$doi, p$date)
    coauthor_frag <- format_coauthor_frag(p$`co-authors`)
    line2 <- if (!is.null(coauthor_frag)) glue("{venue_frag}, *{coauthor_frag}*") else venue_frag
    info_lines <- c(info_lines, line2)

    other_info <- safe(p$`other info`)
    co_list <- str_split(safe(p$`co-authors`), ",\\s*")[[1]]
    n_coauthors <- length(co_list[co_list != ""])

    if (other_info != "") {
      other_names <- str_split(other_info, ",\\s*")[[1]]
      other_names <- other_names[other_names != ""]
      other_joined <- join_names(other_names)

      if (ptype == "book chapter") {
        info_lines <- c(info_lines, glue("*Edited by {other_joined}*"))
      } else if (n_coauthors > 5) {
        info_lines <- c(info_lines, glue("*Coordination: {other_joined}*"))
      }
    }

    body <- c(body, paste(info_lines, collapse = "<br>"), "")

    # --- Abstract teaser (click-to-expand) ---
    abstract_block <- format_abstract_block(p$abstract)
    if (!is.null(abstract_block)) body <- c(body, abstract_block, "")
  }
}

# ---------------------------------------------------------------------------
# HEADER (includes the toggle-abstract JS)
# ---------------------------------------------------------------------------
header <- c(
  "---",
  "---",
  "",
  "<!-- GENERATED FILE — do not edit directly. Edit src/papers.csv or ",
  "     src/_gen_qmd.R instead, then re-run Rscript src/_gen_qmd.R -->",
  "",
  "<script>",
  "function toggleAbstract(el) {",
  "  el.classList.toggle('expanded');",
  "}",
  "</script>",
  ""
)
writeLines(c(header, body), file.path(project_root, "research.qmd"))
cat("research.qmd written to", file.path(project_root, "research.qmd"), "\n")