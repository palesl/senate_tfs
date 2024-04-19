### PURPOSE OF THIS CODE: model future focus as a function of age with covariates

here::i_am("R/10_modelling.R")

## packages

library(tidyverse)
library(mgcv)
library(tictoc)
library(sjPlot)

# data in

analysis_data <- readRDS("outputs/analysis_data.rds")

common_oral_headings <- analysis_data |>
    dplyr::select(oral_heading) |>
    mutate(oral_heading = fct_lump_n(tolower(oral_heading), 100)) |>
    pull(oral_heading)

table(common_oral_headings)

future_focus <- analysis_data |> 
    filter(!grepl("QUESTIONS ON NOTICE", oral_heading, ignore.case = TRUE)) |>
    filter(!grepl("ANSWERS TO QUESTIONS", oral_heading, ignore.case = T)) |>
    filter(!grepl("COMMITTEES,Reports: Government Responses", oral_heading, ignore.case = T)) |>
    filter(!grepl("^PETITIONS", oral_heading, ignore.case = TRUE)) |>
    filter(!grepl("tabling of documents", oral_heading, ignore.case = TRUE)) |>
    filter(!grepl("answers to questions,procedural text", oral_heading,
                  ignore.case = TRUE)) |>
    filter(!grepl("documents,tabling", oral_heading, ignore.case = TRUE)) |>
    filter(!grepl("procedural text", oral_heading, ignore.case = TRUE)) |>
    select(person,Future,InGov,Party,
           date,yearsSince1972,month,day,
           in_cohort,Age,nchars)|>
    na.omit() |>
    group_by(person, InGov, Party, yearsSince1972, date,month, day,
             in_cohort, Age) |>
    summarize(Future = weighted.mean(Future, nchars),
              nchars = sum(nchars))


### Set parties only ever represented by one senator to "Other"
party_counts <- future_focus |>
    group_by(Party) |>
    summarize(nuniq = length(unique(person)))
singletons <- party_counts |>
    filter(nuniq == 1) |>
    pull(Party) |>
    as.character()

future_focus$Party2 <- as.character(future_focus$Party)
future_focus$Party2[which(future_focus$Party2 %in% singletons)] <- "Other"
future_focus$Party2 <- factor(future_focus$Party2)
future_focus$InGov<-as.factor(as.numeric(future_focus$InGov))
future_focus$Party<-as.factor(future_focus$Party)
future_focus$person<-as.factor(future_focus$person)
future_focus$month<-as.factor(as.character(future_focus$month))
future_focus$day<-as.factor(as.character(future_focus$day))

future_focus$in_cohort<-as.factor(future_focus$in_cohort)

future_focus$w8 <- future_focus$nchars / mean(future_focus$nchars)

saveRDS(future_focus,
        file = here::here("working",
                          "senate_model_data.rds"))
#debug

debug=FALSE

if(debug==TRUE){
  future_focus<-future_focus[sample(1:nrow(future_focus), 10000, replace = F),]
  
}

# model
tic()
model<-bam(Future ~  
             # persistent individual effects
             s(person, bs="re") + 
             # Political variables
             InGov + s(Party2, bs="re") + 
             #period
             s(yearsSince1972,bs="cr", k=20) + day + month+
             #cohort
             s(in_cohort, bs="re")+
             # age
             s(Age, bs="cr", k=30),
           nthreads=6,
           weights = future_focus$w8,
           family=betar(link="logit"),
           knots=list(yearsSince1972=seq(0,47,length=20),
                      Age=seq(15,85,length=30)),
           data = future_focus)
toc()
summary(model)
plot(model)

weighted.mean(future_focus$Future ,future_focus$w8, na.rm = T)
 
plot_model(model, type = 'pred', terms = 'Age')
 

saveRDS(model, file = here::here("working",
                                 "senate_tfs_bam_model.rds"))




alt <- future_focus |>
    filter(nchars < 100000) |>
    mutate(w8 = nchars / mean(nchars))

alt_model<-bam(Future ~  
             # persistent individual effects
             s(person, bs="re") + 
             # Political variables
             InGov + s(Party2, bs="re") + 
             #period
             s(yearsSince1972,bs="cr", k=20) + day + month+
             #cohort
             s(in_cohort, bs="re")+
             # age
             s(Age, bs="cr", k=30),
           nthreads=6,
           weights = alt$w8,
           family=betar(link="logit"),
           ## knots=list(yearsSince1972=seq(0,47,length=20),
           ##            Age=seq(23,77,length=30)),
           data = alt)
