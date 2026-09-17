# functions to run publication workflow for publications/index.qmd 

library(tidyverse)
library(RefManageR)
library(htmltools)
library(yaml)
library(httr2)
library(jsonlite)
library(dplyr)

#helper functions 
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
nz_chr <- function(x) ifelse(is.na(x) | trimws(x) == "", NA_character_, x)

#remove braces from bibtext
strip_braces <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  y <- as.character(x)
  gsub("[{}]", "", y, perl = TRUE)
}

#create nice author text
fmt_authors_card <- function(entry) {
  au <- entry$author
  if (is.null(au) || length(au) == 0) au <- entry$editor
  if (is.null(au) || length(au) == 0) return(NA_character_)
  
  fmt_one <- function(a) {
    fam_vec <- a$family
    if (length(fam_vec)) paste(as.character(fam_vec), collapse = " ") else ""
  }
  pieces <- vapply(au, fmt_one, character(1))
  pieces <- pieces[nzchar(pieces)]
  if (!length(pieces)) return(NA_character_)
  if (length(pieces) == 1) pieces
  else paste(paste(pieces[-length(pieces)], collapse = ", "), "&", pieces[length(pieces)])
}

#nicely get abstract
get_abstract <- function(entry) {
  abs <- entry$abstract %||% entry$annotation %||% NA_character_
  if (isTRUE(is.na(abs)) || !nzchar(abs)) NA_character_ else abs
}

#get bibtex
get_bibtex <- function(entry) {
  out <- tryCatch({
    e <- entry
    if (!is.null(e$note)) e$note <- NULL
    if (!is.null(e$Note)) e$Note <- NULL
    txt <- paste0(utils::toBibtex(e), collapse = "\n")
    txt <- sub(",\\s*\\n?\\s*\\}$", "\n}", txt, perl = TRUE)
    txt <- gsub("\n{3,}", "\n\n", txt)
    txt
  }, error = function(err) NA_character_)
  if (is.na(out) || !nzchar(out)) NA_character_ else out
}

#normalize a link
normalize_link <- function(x) {
  y <- as.character(x)
  y[is.na(y)] <- NA_character_
  y <- trimws(y)
  
  y <- gsub("\\\\_", "_", y)
  
  y[y == "" | tolower(y) == "na"] <- NA_character_
  
  ok <- !is.na(y)
  
  doi_pref <- ok & grepl("^doi:\\s*", y, ignore.case = TRUE)
  y[doi_pref] <- sub("^doi:\\s*", "", y[doi_pref], ignore.case = TRUE)
  bare_doi <- ok & grepl("^10\\.", y)
  y[bare_doi] <- paste0("https://doi.org/", y[bare_doi])
  
  arxiv_pref <- ok & grepl("^arxiv:\\s*", y, ignore.case = TRUE)
  if (any(arxiv_pref)) {
    ids <- sub("^arxiv:\\s*", "", y[arxiv_pref], ignore.case = TRUE)
    ids <- sub("^([^\\s]+).*", "\\1", ids)
    y[arxiv_pref] <- paste0("https://arxiv.org/abs/", ids)
  }
  
  domain_like <- ok & grepl("^(www\\.|[A-Za-z0-9.-]+\\.[A-Za-z]{2,})(/|$)", y)
  no_scheme   <- ok & !grepl("^https?://", y, ignore.case = TRUE)
  needs_scheme <- domain_like & no_scheme
  y[needs_scheme] <- paste0("https://", y[needs_scheme])
  
  y
} 

#nicely get the publication date
get_date <- function(entry){
  date <- entry$date 
  if(nchar(date) > 4){
    date <- year(as.Date(date))
  }
  
  return(date)
}

#get library location
zotero_library <- function(){
  return(paste0(fs::path_home(),"/Zotero/Storage"))
}

#get a df of all the collection ID's 
collection_ids <- function(user, collection){
  collections <- request(paste0("http://localhost:23119/api/users/", user, "/groups")) %>%
    req_perform() %>%
    resp_body_json()
  
  group_ids <- lapply(collections, function(x){
    data.frame(name = x$data$name, id = x$id)
  })%>% bind_rows()
  
  id <- group_ids$id[group_ids$name == collection]
  return(id)
}

#get bib info from zotero 
zbib <- function(col_id, format=c("json", "bibtex"), itemType=NULL){
  format <- match.arg(format)
  
  #build request 
  req <- request(paste0("http://localhost:23119/api/groups/", col_id, "/items"))
  query <- list()
  
  if(!is.null(itemType)){query$itemType <- itemType} 
  if(format == "bibtex"){query$format <- "bibtex"}
  
  #get bib info
  bib <- req %>% req_url_query(!!!query) %>%
    req_perform() 
  
  if(format == "json"){
    bib <- bib %>% resp_body_json()
  }else{
    bib <- bib %>% resp_body_string()  
  } 
  
  return(bib)
}

#move pdfs from zotero to website folder 
move_pdfs <- function(bib_list){
  dir.create("articles", showWarnings = FALSE)
  paths <- lapply(bib_list, function(entry){
    if(!is.null(entry$links$attachment)){ 
      id <- basename(entry$links$attachment$href) 
      article <- list.files(file.path(zotero_library(), id), pattern = ".pdf", full.names = TRUE) 
    }else{article <- NA_character_}
    key <- entry$data$citationKey
    data.frame(key = key, path=article)
  }) %>% bind_rows()
  
  invisible(file.copy(paths$path, file.path("articles", paste0(paths$key, ".", tools::file_ext(paths$path))), overwrite = FALSE))
}

#create rows to populate cards 
create_rows <- function(bib, bib_extra){ 
  keys <- names(bib) 

  #reorder the extra data so it's in the same order as bib  
  extra_order <- match(keys,sapply(bib_extra, "[[", "key"))
  bib_extra <- bib_extra[extra_order]
  
  rows <- tibble(
    key   = keys,
    entry = lapply(keys, function(k) bib[k])
  ) |>
    mutate(
      title        = map_chr(entry, ~ strip_braces(.x$title %||% "")),
      authors_card = map_chr(entry, fmt_authors_card),
      year         = map_chr(entry, ~ {
        y <- .x$year %||% .x$date %||% ""
        if (is.list(y)) y$year %||% "" else as.character(y)
      }),
      venue        = map_chr(entry, ~ strip_braces(.x$journal %||% .x$booktitle %||% .x$publisher %||% .x$number %||% "")),
      abstract     = map_chr(entry, get_abstract),
      bibtex       = map_chr(entry, get_bibtex),
      
      doi          = map_chr(entry, ~ .x$doi %||% NA_character_),
      url_fallback = map_chr(entry, ~ .x$url %||% NA_character_),
      
      preprint_val  = sapply(bib_extra, function(x){x$preprint %||% NA_character_}),
      materials_val = sapply(bib_extra, function(x){x$materials %||% NA_character_}),
      
      has_preprint_key  = !is.na(preprint_val),
      has_materials_key = !is.na(materials_val),
      has_article =  sapply(bib_extra, `[[`, "has_article"),
      
      primary = sapply(bib_extra, `[[`, "primary"),
      inreview = sapply(bib_extra, `[[`, "inreview"),
      informal = sapply(bib_extra, `[[`, "informal")
    ) |>
    mutate(
      preprint_url  = normalize_link(preprint_val),
      materials_url = normalize_link(materials_val), 
      article_url = ifelse(has_article, paste0("articles/", key, ".pdf"),NA_character_),
      doi_url = ifelse(is.na(doi), url_fallback, paste0("https://doi.org/", doi))
    ) |>
    arrange(desc(inreview), desc(year), title)
  
  return(rows)
}

#alter bib to have custom fields for website
pull_fields <- function(entry){
  
  data <- entry$data
  
  tags <- vapply(data$tags, `[[`, character(1), "tag")
  
  out <- list(
    key = data$citationKey,
    title = data$title,
    authors = data$creators,
    date = data$date,
    journal = data$publicationTitle,
    volume = data$volume,
    issue = data$issue,
    pages = data$pages,
    doi = data$DOI,
    abstract = data$abstractNote,
    url = data$url,
    has_article = !is.null(entry$links$attachment),
    primary = any(grepl("^primary$", tags, ignore.case = TRUE)),
    inreview =  any(grepl("^in-review$", tags, ignore.case = TRUE)),
    informal =  any(grepl("^informal$", tags, ignore.case = TRUE))
  )
  
  if("extra" %in% names(data)){
    note <- strsplit(data$extra, "\n", fixed = TRUE)[[1]]
    
    materials <- grep("^Materials:", note, ignore.case = TRUE, value = TRUE)
    preprint <- grep("^Preprint:", note, ignore.case = TRUE, value = TRUE)
    
    if(length(materials) > 0)
      out$materials <- trimws(sub("^Materials:\\s*", "", materials[1], ignore.case = TRUE))
    
    if(length(preprint) > 0)
      out$preprint <- trimws(sub("^Preprint:\\s*", "", preprint[1], ignore.case = TRUE))
  }
  
  out
}

#code to build html for cards
build_card <- function(
    key, entry, has_preprint_key, has_materials_key,
    title, authors_card, year, venue,
    abstract, bibtex,
    preprint_url = NA_character_, materials_url = NA_character_,
    article_url = NA_character_,
    doi_url = NA_character_, primary = FALSE, 
    inreview = FALSE, ...) {
  payloads <- tags$div(
    style = "display:none;",
    tags$div(class = "payload-abstract", abstract %||% ""),
    tags$div(class = "payload-bibtex",  bibtex   %||% "")
  )
  
  ttl_node <- if (isTRUE(nzchar(doi_url %||% "")) & !is.na(doi_url)) {
    tags$div(class = "pub-title", tags$a(title, href = doi_url, target = "_blank", rel = "noopener"))
  } else {
    tags$div(class = "pub-title", title)
  }
  
  meta <- tags$div(
    class = "pub-meta",
    if (nzchar(authors_card %||% "")) {
      tagList(HTML(authors_card), tags$br())
    } else {
      NULL
    },
    tagList(if (nzchar(venue %||% "")) tags$em(venue) else NULL)
  )
  
  actions <- tags$div(
    class = "pub-actions",
    # Standalone star badge (only if primary)
    if (isTRUE(primary)) tags$span(
      class = "lab-star",
      role  = "img",
      title = "This project was led by the FEWS lab",
      `aria-label` = "This project was led by the FEWS lab",
      "★",
      tags$span(class = "sr-only", " This project was led by the FEWS lab")
    ),
    # Action links
    if (!is.na(abstract))
      tags$a("Abstract", href = "#", `data-action`="abstract", role="button"),
    if (!inreview | (inreview & has_preprint_key))
      tags$a("Citation", href = "#", `data-action`="citation", role="button"),
    if (!inreview| (inreview & has_preprint_key))
      tags$a("BibTeX",   href = "#", `data-action`="bib",      role="button"),
    if (isTRUE(nzchar(article_url %||% "")) & !is.na(article_url))
      tags$a("Article", class = "external", href = article_url, target = "_blank", rel="noopener"),
    if (isTRUE(has_preprint_key))
      tags$a("Preprint", class = "external", href = (preprint_url %||% "#"), target = "_blank", rel="noopener"),
    if (isTRUE(has_materials_key))
      tags$a("Materials", class = "external", href = (materials_url %||% "#"), target = "_blank", rel="noopener")
  )
  
  toggle_panel <- tags$div(
    class = "toggle-area",
    `data-current` = "",
    tags$button(class = "copy-btn", "Copy"),
    tags$span(class = "copy-toast", "Copied!"),
    tags$div(class = "panel-label", "—"),
    tags$div(class = "content")
  )
  
  tags$div(class = "pub-card", `data-key` = key, ttl_node, meta, actions, toggle_panel, payloads)
}