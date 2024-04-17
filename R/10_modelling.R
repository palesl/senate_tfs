### PURPOSE OF THIS CODE: model future focus as a function of age with covariates

here::i_am("R/10_modelling.R")

## packages

library(tidyverse)
library(mgcv)
library(tictoc)
library(sjPlot)

# data in

analysis_data <- readRDS("outputs/analysis_data.rds")

# filtering out documents read into the record
analysis_data<-analysis_data|>
  filter(!grepl("QUESTIONS ON NOTICE", oral_heading, ignore.case = T)) |>
  filter(!grepl("ANSWERS TO QUESTIONS", oral_heading, ignore.case = T)) |>
  filter(!grepl("COMMITTEES,Reports: Government Responses", oral_heading, ignore.case = T)) |>
  group_by(oral_heading,DisplayName)|>
  arrange(-nchars)

# removing any NAs 
# aggregating to words per person per day

future_focus <- analysis_data|>
  select(person,Future,InGov,Party,
         date,yearsSince1972,month,day,
         in_cohort,Age,nchars)|>
  na.omit() |>
  group_by(person,DisplayName, InGov, Party, yearsSince1972, date,month, day,
           in_cohort, Age) |>
  summarize(Future = weighted.mean(Future, nchars),
            nchars = sum(nchars))




future_focus$InGov<-as.factor(as.numeric(future_focus$InGov))
future_focus$Party<-as.factor(future_focus$Party)
future_focus$person<-as.factor(future_focus$person)
future_focus$month<-as.factor(as.character(future_focus$month))
future_focus$day<-as.factor(as.character(future_focus$day))

future_focus$in_cohort<-as.factor(future_focus$in_cohort)

future_focus$w8 <- future_focus$nchars / mean(future_focus$nchars)

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
             InGov + s(Party, bs="re") + 
             # period
             s(yearsSince1972,bs="cr", k=20)+ day + month+
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


