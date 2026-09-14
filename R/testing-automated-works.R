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
  library(knitcitations) 
  
  
  
  #condenses authors and keywords, filters duplicated pre-prints
  tidy_output <- function(works){
    # works$authorships <- sapply(1:nrow(works), function(x){
    #   paste(works$authorships[[x]]$display_name, collapse = "; ")
    # })
    # 
    # works$keywords <- sapply(1:nrow(works), function(x){
    #   if(!is.na(works$keywords[x])){
    #     paste(works$keywords[[x]]$display_name, collapse = "; ")
    #   }else{NA_character_}
    # }) 
    
    #only consider preprints within last year
    preprint <- works[works$type == "preprint",] %>% filter(publication_date >= year(Sys.Date()) -1)
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
    
    works <- new_pubs
    
    return(works)
  }
  
#get publications 
  works <- oa_fetch(entity = "works", author.id = "A5028722255")
 
#remove preprints unless within the last year (we assume it would otherwise be published)
  #works <- works %>% select(title, authorships, doi, publication_date, type, source_display_name, keywords) %>% tidy_output()
  works <- works %>% select(id, title, doi, publication_date, type) %>% tidy_output()

#after filtering get reselected bib 
  filtered_works <- oa_fetch(entity = "works", identifier = works$id)

#convert the data frame to BibTeX format
 suppressMessages(bib <- GetBibEntryWithDOI(
    filtered_works$doi,
    temp.file = tempfile(fileext = ".bib"),
    delete.file = TRUE
  )) 
 
 WriteBib(bib, file="publications/publications_new.bib")
 
 
  