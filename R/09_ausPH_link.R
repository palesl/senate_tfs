### PURPOSE OF THIS CODE: to take the aggregated data from the daily speaker by heading
### rds file, clean any bad PHIDS, and link to the ausPH package data, ready for analysis.

library(foreach)
library(ausPH)
library(dplyr)
library(arrow)
library(lubridate)


here::i_am("R/08_aggregate_duckling.R")


# initial link attempt

daily_speaker_by_heading <- readRDS("outputs/daily_speaker_by_heading.rds")

individuals<-ausPH::getIndividuals()|>select(PHID, DisplayName)




# unmatched ids function
unmatched_stats<-function(daily_speaker_by_heading){
  link_attempt <- daily_speaker_by_heading|>left_join(individuals, by=join_by("person"=="PHID"))
  unmatched <- link_attempt |> filter(is.na(DisplayName))|>group_by(person)|>
    summarise(mindate = min(date),
              maxdate =max(date),
              n = n())|>
    arrange(-n)
  View(unmatched)
  sum(unmatched$n)
  return(unmatched)
}


# first unmatched

unmatched_stats(daily_speaker_by_heading)

### simple cleaning.


# removing president (speaker)

daily_speaker_by_heading <- daily_speaker_by_heading |> filter(person!="None")

unmatched_stats(daily_speaker_by_heading)

# capitalise IDs

daily_speaker_by_heading$person<-toupper(daily_speaker_by_heading$person)

unmatched_stats(daily_speaker_by_heading)


# looking into ids that are in the data but are noisy for example EXE in helloEXE

unmatched<-unmatched_stats(daily_speaker_by_heading)

out_mat <- matrix(nrow = length(individuals$PHID),
                  ncol = length(unmatched$person))
colnames(out_mat)<- unmatched$person
rownames(out_mat)<- individuals$PHID

for(i in seq_along(individuals$PHID)){
  out_mat[i,]<-grepl(individuals$PHID[i], unmatched$person)
  
  }

for(i in seq_along(unmatched$person)){
  df<-as.data.frame(out_mat[,i])
  names(df)<-'matched'
  df$id<-rownames(df)
  matches<-df$id[df$matched==T]
  matches <- matches[order(nchar(matches), decreasing = TRUE)]
  
  if(length(matches)==0)(
    unmatched$truePerson[i]<-NA
  )else(
    unmatched$truePerson[i]<-matches[1]
  )
}

# concordance

noise_concordance<-unmatched|>filter(!is.na(truePerson))|>select(person,truePerson)

replace_broken_ids <- function(vector, concordance_df) {
  true_ids <- setNames(concordance_df$truePerson, concordance_df$person)
  replaced_vector <- ifelse(vector %in% names(true_ids), true_ids[vector], vector)
  return(replaced_vector)
}


daily_speaker_by_heading$person <- 
  replace_broken_ids(daily_speaker_by_heading$person,noise_concordance)



# now test again
unmatched<-unmatched_stats(daily_speaker_by_heading) #better! 



# now find unmatched data in monolithic senate...

monolithic_senate<-read_parquet('senate_1112.parquet')


monolithic_senate$`Author Id`<-gsub("Fallmps%2F",'',monolithic_senate$`Author Id`,fixed = T)

monolithic_senate$`Author Id`<-gsub("%",'',monolithic_senate$`Author Id`,fixed = T)

matching_unmatched<-monolithic_senate[monolithic_senate$`Author Id` %in% unmatched$person,]


matching_unmatched_sum<-matching_unmatched|>group_by(`Author Id`)|>
  summarise(mindate = min(as.Date(Date,format="%d-%m-%Y")),
            maxdate = max(as.Date(Date,format="%d-%m-%Y")),
            n=n(),
            name = paste(unique(Speaker),collapse = ';')
            )|>
  arrange(-n)


## manual input of missing IDs

search_people<-getIndividuals()|>select(PHID,DisplayName,Gender,SenateState)

search_people<-search_people|>left_join(getParlService(chamber='senate'))

matching_unmatched_sum$true_id<-NA

matching_unmatched_sum$true_id[matching_unmatched_sum$`Author Id` == "YL7"]<- "KUU"

matching_unmatched_sum$true_id[matching_unmatched_sum$`Author Id` == "6D7"]<- "C7D"

matching_unmatched_sum$true_id[matching_unmatched_sum$`Author Id` == "YJ7"]<- "L8O"

matching_unmatched_sum$true_id[matching_unmatched_sum$`Author Id` == "0P7"]<- "CAK"

matching_unmatched_sum$true_id[matching_unmatched_sum$`Author Id` == "2G7"]<- "KKD"

matching_unmatched_sum$true_id[matching_unmatched_sum$`Author Id` == "O14"]<- "OI4"

matching_unmatched_sum$true_id[matching_unmatched_sum$`Author Id` == "4L7"]<- "KTZ"

matching_unmatched_sum$true_id[matching_unmatched_sum$`Author Id` == "OP7"]<- "K2U"

matching_unmatched_sum$true_id[matching_unmatched_sum$`Author Id` == "OT7"]<- "KA8"

matching_unmatched_sum$true_id[matching_unmatched_sum$`Author Id` == "8T7"]<- "K8R"

matching_unmatched_sum$true_id[matching_unmatched_sum$`Author Id` == "8R7"]<- "EVL"

matching_unmatched_sum$true_id[matching_unmatched_sum$`Author Id` == "O97"]<- "JYI"

matching_unmatched_sum$true_id[matching_unmatched_sum$`Author Id` == "C67"]<- "K7H"

matching_unmatched_sum$true_id[matching_unmatched_sum$`Author Id` == "S97"]<- "JYL"

matching_unmatched_sum$true_id[matching_unmatched_sum$`Author Id` == "GM7"]<- "KVK"

matching_unmatched_sum$true_id[matching_unmatched_sum$`Author Id` == "WK7"]<- "KTJ"

matching_unmatched_sum$true_id[matching_unmatched_sum$`Author Id` == "WJ7"]<- "KNA"

matching_unmatched_sum$true_id[matching_unmatched_sum$`Author Id` == "SG7"]<- "KPG"

matching_unmatched_sum$true_id[matching_unmatched_sum$`Author Id` == "SO7"]<- "K1M"

matching_unmatched_sum$true_id[matching_unmatched_sum$`Author Id` == "MQ7"]<- "K5H"

matching_unmatched_sum$true_id[matching_unmatched_sum$`Author Id` == "EK7"]<- "KTA"

matching_unmatched_sum$true_id[matching_unmatched_sum$`Author Id` == "NH7"]<- "KQD"

matching_unmatched_sum$true_id[matching_unmatched_sum$`Author Id` == "TS7"]<- "K8H"

matching_unmatched_sum$true_id[matching_unmatched_sum$`Author Id` == "757"]<- "K6F"

matching_unmatched_sum$true_id[matching_unmatched_sum$`Author Id` == "8V7"]<- "KBS"

matching_unmatched_sum$true_id[matching_unmatched_sum$`Author Id` == "FE7"]<- "KOM"

matching_unmatched_sum$true_id[matching_unmatched_sum$`Author Id` == "J87"]<- "JUI"

matching_unmatched_sum$true_id[matching_unmatched_sum$`Author Id` == "OL7"]<- "KUJ"

matching_unmatched_sum$true_id[matching_unmatched_sum$`Author Id` == "HV7"]<- "KBY"

matching_unmatched_sum$true_id[matching_unmatched_sum$`Author Id` == "00AF1"]<- "I0V"

matching_unmatched_sum$true_id[matching_unmatched_sum$`Author Id` == "287"]<- "JTT"

matching_unmatched_sum$true_id[matching_unmatched_sum$`Author Id` == "4N7"]<- "JYA"

matching_unmatched_sum$true_id[matching_unmatched_sum$`Author Id` == "NM7"]<- "JXG"

matching_unmatched_sum$true_id[matching_unmatched_sum$`Author Id` == "9K7"]<- "KSY"

matching_unmatched_sum$true_id[matching_unmatched_sum$`Author Id` == "F27"]<- "K1Y"

matching_unmatched_sum$true_id[matching_unmatched_sum$`Author Id` == "IU7"]<- "CJO"

matching_unmatched_sum$true_id[matching_unmatched_sum$`Author Id` == "9R7"]<- "K69"

matching_unmatched_sum$true_id[matching_unmatched_sum$`Author Id` == "YH7"]<- "KQN"

matching_unmatched_sum$true_id[matching_unmatched_sum$`Author Id` == "A9C"]<- "00A9C"

matching_unmatched_sum$true_id[matching_unmatched_sum$`Author Id` == "IH7"]<- "KPV"

matching_unmatched_sum$true_id[matching_unmatched_sum$`Author Id` == "009FX"]<- "281558"

matching_unmatched_sum$true_id[matching_unmatched_sum$`Author Id` == "4"]<- "BH4"

matching_unmatched_sum$true_id[matching_unmatched_sum$`Author Id` == "A"]<- "AW5"

matching_unmatched_sum$true_id[matching_unmatched_sum$`Author Id` == "ZF"]<- "ZF4"

### matching to manual concordance
names(matching_unmatched_sum)[1]<-'person'
names(matching_unmatched_sum)[6]<-'truePerson'

manual_concordance<-matching_unmatched_sum|>filter(!is.na(truePerson))|>select(person,truePerson)


daily_speaker_by_heading$person <- 
  replace_broken_ids(daily_speaker_by_heading$person,manual_concordance)

# now test again
unmatched<-unmatched_stats(daily_speaker_by_heading) 

# everyone now unmatched is either not a senator (foreign entity), or is acting as senate president
# it is not very many observations anymore...

sum(unmatched$n)

working_data <- daily_speaker_by_heading|>left_join(individuals, by=join_by("person"=="PHID"))

# dropping unlinked

working_data<-working_data|>filter(!is.na(DisplayName))


#### linking with analysis covariates... ####


# Persistent individual effects: speaker random effects which allow for 
# idiosyncratic differences between speakers, including the effects of 
# any pre-political education or career. 

working_data$person


# Political variables: party random effects and a dichotomous
# variable with value one if the speaker’s party was in government

partyService<-getPartyService()
partyService$DateEnd[is.na(partyService$DateEnd)]<-as.Date('2025-05-22')

names(partyService)[1]<-'person'

partyService<- partyService |>select('person', 'DateStart','DateEnd','Party')

working_data <-working_data|>left_join(partyService)|>
  filter(date >= DateStart & date <= DateEnd) |>
  select(-DateStart, -DateEnd) # we lost 847 observations here because for some dates party status unknown

# ingov 

ministries<-getMinistries()
ministries$DateEnd[is.na(ministries$DateEnd)]<-as.Date('2025-05-22')

working_data$InGov<-FALSE

for (j in 1:nrow(ministries)) {
  within_period <- working_data$date >= ministries$DateStart[j] & 
    working_data$date <= ministries$DateEnd[j] &
    working_data$Party == ministries$Party[j]
  
  working_data$InGov <- working_data$InGov | within_period
}



# Period variables: years since 1972, together with fixed effects
# for day of the week and month of the year 
working_data$yearsSince1972<-time_length(difftime(working_data$date, as.Date("1972-01-01")), "years")
working_data$month <-month(working_data$date, label=T)
working_data$day <-wday(working_data$date, label=T)

# Cohort variables: the parliamentary session in which the
# speaker first entered parliament: for example, the 
# 46th Parliament (2019-2022)

sen_indates<-getParlService(chamber='senate')|>group_by(PHID)|>
  summarise(indate=min(DateStart))

# function to assign parliament period names to dates  
assign_period_parl <- function(date, periods) {
  period_name <- NA
  for (i in 1:nrow(periods)) {
    if (date >= periods$DateElection[i] && date <= periods$ParliamentEnd[i]) {
      period_name <- periods$Name[i]
      break
    }
  }
  return(period_name)
}

Parls<-getParliaments()
Parls$ParliamentEnd[is.na(Parls$ParliamentEnd)]<-as.Date(2025-05-22)

sen_indates$in_cohort <- sapply(sen_indates$indate, function(x) assign_period_parl(x, Parls))

sen_indates<-sen_indates|>select(PHID,in_cohort)
names(sen_indates)[1]<-'person'

working_data<-working_data|>left_join(sen_indates)

working_data$in_cohort



# Age variable: speaker’s age measured in years

dob<-ausPH::getIndividuals()|>select(PHID,DateOfBirth)

dob$DateOfBirth<-as.Date(dob$DateOfBirth) 
names(dob)[1]<-'person'

working_data<-working_data|>left_join(dob)

working_data$Age<- time_length(difftime(working_data$date, working_data$DateOfBirth), "years")

# output

saveRDS(working_data, file = here::here("outputs", "analysis_data.rds"))


