handle_date <- function(x) {
  x <- sub(".*senate_", "", x )
  x <- sub(".parquet", "", x, fixed = TRUE)
  x <- substr(x, 0, 10)
  as.Date(x, format = "%Y-%m-%d")
}


handle_hansard80 <- function(dat, date) {
  if (date < as.Date("1981-01-01")) {
    x<-dat
    x$speech_id<-dat$`Author Id`
    x$`System Id`<-dat$`Author Id`
    x$`Author Id`<-dat$Text
    x$Text<- dat$Filename
    x$Title<- sub(".*.txt,", "",x$Title)
    x<-x[x$Text!="None",]
  }else{x<-dat}
  
  x
}

handle_speech_id <- function(dat) {
  
  if(suppressWarnings(is.null(dat$speech_id))){
    retval <- dat$`System Id`
  }
  
  if(suppressWarnings(!is.null(dat$speech_id))){
    retval <- dat$speech_id
  }
  
  if (length(retval) == 0) {
    retval <- NA_character_
  }
  retval
}


handle_current_president<-function(date){
  
  presidents <- tribble(
    ~person_id, ~startdate, ~stopdate, ~Name,
    "KUH", "8/9/1953", "30/6/1971", "McMULLIN, the Hon. Sir Alister Maxwell, KCMG",
    "JQQ", "17/8/1971", "11/4/1974", "CORMACK, the Hon. Sir Magnus Cameron, KBE",
    "JYA", "9/7/1974", "11/11/1975", "O'BYRNE, the Hon. Justin Hilary, AO",
    "KQN", "17/2/1976", "30/6/1981", "LAUCKE, the Hon. Sir Condor Louis, KCMG",
    "KBY", "18/8/1981", "4/2/1983", "YOUNG, the Hon. Sir Harold William, KCMG",
    "KTA", "21/4/1983", "23/1/1987", "McCLELLAND, the Hon. Douglas, AC",
    "EJ4", "17/2/1987", "5/6/1987", "SIBRAA, the Hon. Kerry Walter, AO",
    "EJ4", "14/9/1987", "31/1/1994", "SIBRAA, the Hon. Kerry Walter, AO",
    "NE4", "1/2/1994", "30/6/1996", "BEAHAN, the Hon. Michael Eamon, AM",
    "VI4", "20/8/1996", "18/8/2002", "REID, the Hon. Margaret Elizabeth, AO",
    "5F4", "19/8/2002", "14/8/2007", "CALVERT, the Hon. Paul Henry, AO",
    "EP5", "14/8/2007", "25/8/2008", "FERGUSON, the Hon. Alan Baird, AM",
    "7L6", "26/8/2008", "30/6/2014", "HOGG, the Hon. John Joseph",
    "E5V", "7/7/2014", "9/5/2016", "PARRY, the Hon. Stephen Shane",
    "E5V", "30/8/2016", "13/11/2017", "PARRY, the Hon. Stephen Shane",
    "I0Q", "13/11/2017", "13/10/2021", "RYAN, the Hon. Scott Michael",
    "30484", "18/10/2021", "25/7/2022", "BROCKMAN, the Hon. William (Slade) Edward Slade"
  )
  
  presidents$startdate<-dmy(presidents$startdate)
  presidents$stopdate<-dmy(presidents$stopdate)
  
  retval<-presidents$person_id[which(presidents$startdate< date & presidents$stopdate>=date)]
  if (length(retval) == 0) {
    retval <- NA_character_
  }
  retval
}



handle_person <- function(dat,date) {
  retval <- dat$`Author Id`
  retval<- sub("Fallmps%2F","",retval, fixed = T)
  retval<- sub("%","",retval, fixed = T)
  
  retval[retval=="None"&grepl("president",dat$Speaker, ignore.case = T)]<-handle_current_president(date)
 
  
  if (length(retval) == 0) {
    retval <- NA_character_
  }
  retval
}

handle_words <- function(dat) {
  spoken_words <- dat$Text
  ## Replace zero length entries
  spoken_words[which(sapply(spoken_words, length) == 0)] <- ""
  spoken_words <- unlist(spoken_words)
  
  if (is.null(spoken_words)) {
    spoken_words <- NA_character_
  }
  ### Remove content before the first hyphen, n or m dash (noting the name of the speaker)
  spoken_words<- gsub("^[^\n]*\n", "", spoken_words)|>trimws()
  spoken_words<- gsub("\n", "", spoken_words) 
  
  ### Remove content between square brackets
  spoken_words <- gsub("\\[.*?)\\]", "", spoken_words)
  
  spoken_words <- gsub("hon.", "Honourable", spoken_words, fixed = TRUE)
  spoken_words <- gsub("Hon.", "Honourable", spoken_words, fixed = TRUE)
  spoken_words <- gsub("HON.", "HONOURABLE", spoken_words, fixed = TRUE)
  spoken_words <- gsub("Prof.", "Professor ", spoken_words, fixed = TRUE)
  spoken_words <- gsub("Dr.", "Doctor ", spoken_words, fixed = TRUE)
  spoken_words <- gsub("Mr.", "Mr", spoken_words, fixed = TRUE)
  spoken_words <- gsub("Ms.", "Ms", spoken_words, fixed = TRUE)
  spoken_words <- gsub("Mrs.", "Mrs", spoken_words, fixed = TRUE)
  
  ## Remove initial punctuation in these spoken_words
  spoken_words <- sub("^[[:punct::]+", "", spoken_words)
  spoken_words <- str_trim(spoken_words)
  spoken_words <- sub("^[[:punct::]+", "", spoken_words)        
  spoken_words <- str_trim(spoken_words)
  spoken_words <- str_squish(spoken_words)
  
  spoken_words
}

verbs <- function(json) {
  x <- jsonlite:::parse_string(json, bigint_as_char=F)
  sentences <- x$sentences[[1]]
  tokens <- sentences$tokens
  pos <- lapply(tokens, function(x)x$pos)
  ### for parts-of-speech, punctuation is returned as-is
  verbs <- sum(sapply(pos, function(x)substr(x, 0, 2) == "VB"))
  return(verbs)
}

extract_timex <- function(response) {
  x <- jsonlite:::parse_string(response, bigint_as_char=F)
  ## x <- parse_json(response, simplifyVector = FALSE)
  sentences <- x$sentences[[1]]
  entities <- sentences[["entitymentions"]]
  entities <- lapply(entities, as.data.frame)
  entities <- bind_rows(entities)
  if ("timex.type" %in% names(entities)) {
    ### return just those entries
    entities <- entities |>
      filter(!is.na(timex.type))
    ### and those entities which are dates
    entities <- entities |>
      filter(timex.type == "DATE")
    if ("timex.value" %in% names(entities)) {
      return(entities |>
               dplyr::select(pattern = text,
                             timex.value))
    } else {
      return(NULL)
    }
  } else {
    ### return null data frame
    return(NULL)
  }  
}

timex2date <- function(x) {
  case_when(nchar(x) == 4 ~ as.Date(paste0(x, "-01-01"),
                                    format = "%Y-%m-%d"),
            nchar(x) == 7 ~ as.Date(paste0(x, "-01"),
                                    format = "%Y-%m-%d"),
            nchar(x) == 10 ~ as.Date(x,
                                     format = "%Y-%m-%d"))
}

difftime_to_text <- function(x, level = 1) {
  ## level 1 = year
  ## level 2 = year and month
  ## print(class(x))
  sign_x <- sign(x)
  if (is.na(sign_x)) {
    return(NA_character_)
  }
  if (sign_x == 0) {
    return(NA_character_)
  }
  x <- abs(x)
  
  years <- floor(as.numeric(x) / 365.25)
  residual <- as.numeric(x) - (365.25 * years)
  months <- floor(residual / 30)
  if (level == 1) {
    r <- case_when(years == 0 ~ "less than a year ",
                   years == 1 ~ "one year ",
                   years > 1 ~ paste0(years, " years "))
  } else if (level == 2) {
    r <- case_when(years == 0 & months == 1 ~ "one month ",
                   years == 0 & months > 1 ~ paste0(months, " months "),
                   years == 1 & months == 0 ~ "one year ",
                   years == 1 & months == 1 ~ "one year and one month ",
                   years == 1 & months > 1 ~ paste0("one year and ",
                                                    months, " months "),
                   years > 1 & months == 0 ~ paste0(years, " years "),
                   years > 1 & months == 1 ~ paste0(years, " year and one month "),
                   years > 1 & months > 1 ~ paste0(years, " years and ",
                                                   months, " months "))
  }
  r <- paste0(r, ifelse(sign_x == 1, "from now", "ago"))
  return(r)
}




get_major_heading <- function(dat) {
  retval <- dat$`Title`
  if (length(retval) == 0) {
    retval <- NA_character_
  }
  retval
}

 