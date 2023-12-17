library(tidyverse)
library(arrow)
library(doParallel)
library(foreach)
here::i_am("R/07_aggregate.R")

infiles <- list.files(here::here("working/distilled"),
                      full.names = TRUE)

parse_file <- function(i) {
    the_date <- sub(".*debates", "", i)
    the_date <- gsub("[^0-9]", "", the_date)
    the_date <- as.Date(the_date, format = "%Y%m%d")
    ### Aggregate to speaker/topic/da
    retval <- read_parquet(i) |>
        mutate(date = the_date) |>
        ### accounting for debates on 2007-09-19 which did not have an oral heading.
        mutate(oral_heading = as.character(oral_heading),
               oral_heading = case_when(is.na(oral_heading) ~ "_NA_Character_",
                                        !is.na(oral_heading) ~ oral_heading)
               ) |>
        mutate(person = as.character(person),
               nchars = nchar(newtext)) |>
        group_by(person, date,
                 oral_heading) |>
        summarize(Present = weighted.mean(Present, nchars, na.rm = TRUE),
                  Past = weighted.mean(Past, nchars, na.rm = TRUE),
                  Future = weighted.mean(Future, nchars, na.rm = TRUE),
                  nchars = sum(nchars, na.rm = TRUE),
                  .groups = "drop") |>
        as.data.frame()
    return(retval)
}

num_cores <- 20
cl <- makeCluster(num_cores)
registerDoParallel(cl)
clusterEvalQ(cl, library(tidyverse))
clusterEvalQ(cl, library(arrow))
Sys.time()
dat <- foreach(i=infiles,.combine = 'bind_rows') %dopar% {
    parse_file(i)
}
Sys.time()

stopCluster(cl)

dat$person <- factor(dat$person)
dat <- as.data.frame(dat)

saveRDS(dat, file = here::here("outputs", "daily_speaker_by_heading.rds"))
