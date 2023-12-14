### PURPOSE OF THIS CODE: to take in the XML from the ParlParse
### project, and turn this into R data frames which include
### information on the speaker, the member, the date, the heading, and
### what they actually said

### Load libraries
library(tidyverse)
library(tidytext)
library(rvest)
library(parallel)
library(xml2)
library(furrr)
library(jsonlite)

num_cores_to_use <- 20

here::i_am("R/02_tidy_files.R")

### Get in helper functions
source(here::here("R", "00_helpers.R"), echo = FALSE)







tidy_file <- function(file, overwrite = FALSE, debug = FALSE) {
    require(tidyverse)
    require(tidytext)
    require(stringr)
    require(rvest)
    require(xml2)
    require(arrow)
    require(lubridate)
  

    outfile <- sub("debates", "working", file)
    outfile <- sub(".parquet", ".rds", outfile, fixed = TRUE)

### if overwrite is false, and the file exists
    if (!overwrite) { 
        if (file.exists(outfile)) return(TRUE)
    }
    
    raw <- read_parquet(file)
    
### handle formatting issue for texts before 1981    
    
    
    date <- handle_date(file)
    
    
    raw <- handle_hansard80(raw, date)
    

### extract data
    
    oral_heading <- get_major_heading(raw)

    speech_id <- handle_speech_id(raw)
 
    person <- handle_person(raw, date)
    
    is_president <- person == handle_current_president(date)
    
    words <- handle_words(raw)
    
    dat <- data.frame(person = person,
                      oral_heading = oral_heading,
                      speech_id = speech_id,
                      is_president = is_president,
                      date = date,
                      words = words,
                      row.names = NULL) |>
      filter(!is_president) |>
      filter(!is.na(is_president) | !is.na(is_president)) |>
      dplyr::select(-is_president)
    
    
### Tokenize on sentence
    dat <- dat |>
        unnest_tokens(sents, words,
                      "sentences",
                      to_lower = FALSE,
                      drop = TRUE)

    ### Remove sentences without letters (i.e., purely numeric or punctuation)
    dat <- dat |>
        filter(grepl("[A-Za-z]", sents))

### Omit sentences which begin and end with square brackets
    dat <- dat |>
        filter(!(grepl("^\\[", sents) & grepl("\\[$", sents)))
    
### Replace references to Act [YYYY] with Act_[YYYY]
    dat <- dat |>
        mutate(sents = stringi::stri_replace_all_regex(sents,
                                                       "Act [0-9]{4}",
                                                       replacement = "Act"))

    dat <- dat |>
        mutate(sents = stringi::stri_replace_all_regex(sents,
                                                       "esolution [0-9]{4}",
                                                       replacement = "esolution"))

### We want sentences that have at least four words.
    ## four words => three boundaries
    dat <- dat |>
        mutate(nboundaries = str_count(sents, boundary("word"))) |>
        filter(nboundaries >= 3)    

### select the columns we want
    dat <- dat |>
        dplyr::select(speaker, person, date,
                      heading, oral_heading, speech_id,
                      sents) |>
        as.data.frame()
    
    if (nrow(dat) == 0) {
        warning(paste0("No speeches in file ", file))
        return(FALSE)
    }
    
    if (debug) {
        print(dat |>
              sample_n(10) |>
              pull(sents))
    } else { 
        dat <- dat |>
            mutate(docid = seq_len(n()))
        saveRDS(dat, outfile)
    }
    return(TRUE)
}


infiles <- sort(list.files(here::here("scrapedxml/debates/"),
                           full.names = TRUE))
length(infiles)

### ParlParse will occasionally store duplicates of date entries
### Pick the last of each date (i.e., if we have a, b, and c, pick c
date <- sub("scrapedxml/debates//debates", "", infiles, fixed = TRUE)
date <- gsub("[a-z]", "", date)

infiles <- by(infiles, list(date = date), FUN = tail, n = 1)
infiles <- unlist(unique(infiles))
length(infiles)

### Shuffle the files
infiles <- sample(infiles, length(infiles), replace = FALSE)

            
## ## # Initiate cluster
cl <- makeCluster(num_cores_to_use)
clusterEvalQ(cl, library(jsonlite))
clusterEvalQ(cl, source("R/00_helpers.R"))

Sys.time()
res <- parLapply(cl,
                 X = infiles,
                 fun = tidy_file,
                 overwrite = FALSE,
                 debug = FALSE)
Sys.time()

stopCluster(cl)
