### PURPOSE OF THIS CODE: model future focus as a function of age with covariates

here::i_am("R/10_modelling.R")

## packages

library(tidyverse)
library(mgcv)
library(tictoc)
library(sjPlot)

# data in

analysis_data <- readRDS("outputs/analysis_data.rds")

# removing any NAs 

future_focus <- analysis_data|>
  select(person,Future,InGov,Party,yearsSince1972,month,day,
         in_cohort,Age)|>
  na.omit()

future_focus$InGov<-as.factor(as.numeric(future_focus$InGov))
future_focus$Party<-as.factor(future_focus$Party)
future_focus$person<-as.factor(future_focus$person)
future_focus$month<-as.factor(as.character(future_focus$month))
future_focus$day<-as.factor(as.character(future_focus$day))

future_focus$in_cohort<-as.factor(future_focus$in_cohort)

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
             #period
             s(yearsSince1972,bs="cr", k=20)+ day + month+
             #cohort
             s(in_cohort, bs="re")+
             # age
             s(Age,bs="cr", k=30),
           nthreads=6,
           family=betar(link="logit"),
           knots=list(yearsSince1972=seq(0,47,length=20),
                      Age=seq(23,77,length=30)),
           data = future_focus)
toc()

saveRDS(model, file = here::here("working",
                                 "senate_tfs_bam_model.rds"))


