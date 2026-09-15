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
  
  #function to get works from every person on the list
  get_fews_works <- function(id, start, end){
    if(!is.na(start)){start <- as.Date(paste0(start, "-01-01"))}
    if(!is.na(end)){end <- as.Date(paste0(end, "-12-31")) + years(1)}
    
    #get works from starting at lab to 1 year after
    if(is.na(start) & is.na(end)){
      works <- oa_fetch(entity = "works", author.id = id) 
    }else if(is.na(end)){
      works <- oa_fetch(entity = "works", author.id = id,
                        from_publication_date = start) 
    }else{
      works <- oa_fetch(entity = "works", author.id = id,
                        from_publication_date = start,
                        to_publication_date = end) 
    }
    
    
    if(is.null(works)){
      return(NULL)
    }
    
    works <- works %>% 
      select(any_of(c("id", "title", "doi", "publication_year", "type", "authorships", "keywords", "abstract"))) %>% 
      tidy_output() 
    
    #return to bind together 
    return(works)
  }
  
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
  
  # #get missing abstracts 
  # get_abstract <- function(doi){
  #   browser()
  #   abs <- tryCatch({cr_abstract(gsub("https://doi.org/", "", doi))},
  #                   error = function(e){return(NA)})
  #   abs_clean <- gsub("^Abstract |^abstract: ", "", abs, ignore.case = TRUE)
  #   return(abs_clean)
  # }
  
#get publications for each person 
  all_works <- pblapply(1:nrow(people), function(x){
    get_fews_works(people$openalex_id[x], start=people$start[x], end=people$end[x])
  })
  
  works <- all_works %>% bind_rows() %>% distinct()
#
#add columns to add manually 
  #check if first author on people list, then likely a primary
  works$primary <- sapply(works$first_auth_id, function(x){
    any(grepl(x, people$openalex_id, ignore.case = TRUE))
  }) 
  
  works_clean <- works %>% mutate(materials="", preprint="", filename="", include=TRUE) %>% 
    arrange(type, desc(publication_year))
  
#try to fill in missing abstracts 
  # missing <- which(is.na(works_clean$abstract))
  # 
  # for(x in missing){
  #   print(x)
  #   works_clean$abstract[x] <- get_abstract(works_clean$doi[x])
  # }
  
#write to csv to allow editing 
  write_excel_csv(works_clean, "publications/semiauto_bib.csv")
  
#after filtering get reselected bib 
  filtered_works <- oa_fetch(entity = "works", identifier = works$id)

#convert the data frame to BibTeX format
 suppressMessages(bib <- GetBibEntryWithDOI(
    filtered_works$doi,
    temp.file = tempfile(fileext = ".bib"),
    delete.file = TRUE
  )) 
 
 WriteBib(bib, file="publications/publications_new.bib")
 
 
  