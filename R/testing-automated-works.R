## tester code to see if we can automatically generate publications 

  #TODO:
  #start with just Kevin's (then we could add a df with others and date range), make sure we don't have duplicates 
  #separate pubs from data packages (or visually make them different) 
  #link to pdf versions 
  #add abstract and get citation to work

  #we want to be able to link materials, article, preprints, and mark as primary ? 
  #generate and write to csv that is editable to remove duplicates (T/F) and add extra info (pull abstracts though...) then feed to WriteBib


#load libraries 
  library(openalexR)
  library(lubridate)
  library(RefManageR) 
  library(dplyr)
  library(readr)
  library(rcrossref)
  library(pbapply)
  
  people <- read.csv("publications/people_info.csv") %>% filter(!is.na(openalex_id))
  
  #condenses authors and keywords, filters duplicated pre-prints
  tidy_output <- function(works){
    works$first_auth_id <- sapply(1:nrow(works), function(x){
      gsub("https://openalex.org/", "", works$authorships[[x]]$id[1])    })
    
    works$authorships <- sapply(1:nrow(works), function(x){
      paste(works$authorships[[x]]$display_name, collapse = "; ")
    })

    works$keywords <- sapply(1:nrow(works), function(x){
      if(!is.na(works$keywords[x])){
        paste(works$keywords[[x]]$display_name, collapse = "; ")
      }else{NA_character_}
    })
    
    #remove abstract before the abstract 
    if("abstract" %in% colnames(works)){
      works$abstract <- gsub("^Abstract |^abstract: ", "", works$abstract, ignore.case = TRUE)
    }
    
    #only consider preprints within last year
    preprint <- works[works$type == "preprint",] %>% filter(publication_year >= year(Sys.Date()) -1)
    new_pubs <- works[works$type != "preprint",]
    if(nrow(preprint) > 0){
      for(n in 1:nrow(preprint)){
        paper <- preprint[n,]
        published <- ifelse(paper$title %in% new_pubs$title, TRUE, FALSE)
        if(!published){
          new_pubs <- rbind(new_pubs, paper)
        }
      }
    }
    
    works <- new_pubs %>% filter(!(type %in% c("peer-review", "erratum"))) %>% 
      mutate(type = ifelse(is.na(doi), "conference-abstract", type)) %>% 
     
    
    return(works)
  }
  
 
#get publications kevin is associated with (kevin should be on any fews lab associated things) 
  works <- oa_fetch(entity = "works", author.id = "a5028722255;a5129069706")
  works_clean <- works %>% 
    select(any_of(c("id", "title", "doi", "publication_year", "type", "authorships", "keywords", "abstract", "source_display_name"))) %>% 
    tidy_output() 

#add columns to add manually 
  #check if first author on people list, then likely a primary
  works_clean$primary <- sapply(works_clean$first_auth_id, function(x){
    any(grepl(x, people$openalex_id, ignore.case = TRUE))
  }) 
  
  works_clean <- works_clean %>% mutate(primary = ifelse(is.na(primary), FALSE, primary), 
                                  materials="", preprint="", filename="", include=TRUE) %>% 
    arrange(type, desc(publication_year))
  
#try to fill in missing abstracts 
  # missing <- which(is.na(works_clean$abstract))
  # 
  # for(x in missing){
  #   print(x)
  #   works_clean$abstract[x] <- get_abstract(works_clean$doi[x])
  # }
  
#write to csv to allow editing, but only add new things, don't overwrite the info already in there 
  if(file.exists("publications/semiauto_bib.csv")){
    old <- read.csv("publications/semiauto_bib.csv")
    new <- works_clean %>% filter(!(id %in% old$id)) 
    
    works_clean <- old %>% bind_rows(new) %>% arrange(type, desc(publication_year))
    write_excel_csv(works_clean, "publications/semiauto_bib.csv")
    
  }else{
    write_excel_csv(works_clean, "publications/semiauto_bib-edit.csv")
  }
  
#after editing, convert to BibTex format 
  #probably want to write code manually for this...
  
#convert the data frame to BibTeX format
 suppressMessages(bib <- GetBibEntryWithDOI(
    filtered_works$doi,
    temp.file = tempfile(fileext = ".bib"),
    delete.file = TRUE
  )) 
 
 WriteBib(bib, file="publications/publications_new.bib")
 
 
#trying to just add to group library in zotero with custom tags so it's easier to manage 
#need to install Better BibTeX for Zotero (https://retorque.re/zotero-better-bibtex/installation/) must be installed and zotero must be running 
 #also need to The local API must be enabled in Zotero’s preferences (Settings → Advanced → “Allow other applications on this computer to communicate with Zotero”).

library(httr2)
library(jsonlite)
library(dplyr)
 
#get id for group 
  user <- Sys.getenv("ZOTERO_USER_ID")
  collections <- request(paste0("http://localhost:23119/api/users/", user, "/groups")) %>%
    req_perform() %>%
    resp_body_json()
  group_ids <- lapply(collections, function(x){
    data.frame(name = x$data$name, id = x$id)
  })%>% bind_rows()

bib_list <- request(
  paste0("http://localhost:23119/api/groups/", group_ids$id[group_ids$name == "FEWS-publications"], "/items")) %>%
  req_url_query(itemType = "journalArticle") %>% #get just articles not the pubs too
  req_perform() %>%
  resp_body_json() 

bib <- request(
  paste0("http://localhost:23119/api/groups/", group_ids$id[group_ids$name == "FEWS-publications"], "/items")) %>%
  req_url_query(itemType = "journalArticle", format="bibtex") %>% #get just articles not the pubs too
  req_perform() %>%
  resp_body_string() 

#get only articles 

#get library location
  zotero_library <- function(){
  return(paste0(fs::path_home(),"/Zotero/Storage"))
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
      primary = any(grepl("^primary$", tags, ignore.case = TRUE))
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
  
#apply extra info (pull just the things we want)
  bib_extra <- lapply(bib_list, pull_fields)

#copy pdf to website repo folder 
  dir.create("publications/articles", showWarnings = FALSE)
  paths <- lapply(bib_list, function(entry){
    if(!is.null(entry$links$attachment)){ 
      id <- basename(entry$links$attachment$href) 
      article <- list.files(file.path(zotero_library(), id), pattern = ".pdf", full.names = TRUE) 
    }else{article <- NA_character_}
    key <- entry$data$citationKey
    data.frame(key = key, path=article)
    }) %>% bind_rows()
  
  file.copy(paths$path, file.path("publications/articles", paste0(paths$key, ".", tools::file_ext(paths$path))), overwrite = TRUE)

  
#save as yaml to read via quarto code
  yaml::write_yaml(bib_extra, "publications/publications.yml")
  
#write bibtext file
  write.table(bib, "publications/publications.bib", row.names=FALSE, col.names = FALSE, quote = FALSE)

#write bibtext file
