##### Notes #####

# pp_{year} <- Results by polling place for {year}, including:
#               * Latitutde and longitude of the polling place
#               * Two party preferred (TPP) vote and percent by party, and TPP swing
#                 (note: tpp.total.votes does not equal the actual PP vote total)
#               * Total votes for the polling place
# div_{year} <- Details by divisions, including:
#               * Demographics
#               * Enrolment
#               * Number of nominations
#               * The elected party/candidate
#               * Whether the seat has changed hands and if so the previous party
#               * Whether the seat was decided on first preferences or preferences were counted
#               * Whether the seat was a non-classic contest
# cand_{year} <- Results by candidate by polling place, including:
#               * Candidate name, party and ballot position
#               * Whether the candidate was elected at this or the previous election
#               * Dummy polling places for declaration votes
#               * Indication of whether the vote was an eary vote
#               * Indication of whether the vote was an ordinary or a declaration vote
#               * Two candidate preferred (TCP) vote and swing
#               * Variable identifying informal votes


##### Load Libraries #####
library(dplyr)

##### Source common variables/functions #####
source('common.R')

##### Functions #####
#### Download VTR Files ####

download_vtr_file <- function(file, year) {
  # This could use some more error checking
  tmp_vtr_data <- read.csv(file, sep = ',', header = TRUE, skip = 1, colClasses = "character")
  return(tmp_vtr_data)
}

download_vtr <- function(file, year, do_state = FALSE){
  tmp_event_id <- event_ids$id[event_ids$year == year]
  tmp_basedir <- paste('http://results.aec.gov.au/', tmp_event_id, '/Website/Downloads/', sep = '')
  
  if(do_state){
    tmp_filename_list <- paste(tmp_basedir, file, '-', tmp_event_id, '-', states_list, '.csv', sep = '')
    tmp_vtr_table <- do.call("bind_rows", lapply(tmp_filename_list, download_vtr_file, year = year))
  } else {
    tmp_filename <- paste(tmp_basedir, file, '-', tmp_event_id, '.csv', sep = '')
    tmp_vtr_table <- download_vtr_file(tmp_filename, year)
  }
  return(tmp_vtr_table)
}

#### Polling place table functions ####

build_pp_base <- function(year) {
  tmp_pp_base <- download_vtr('GeneralPollingPlacesDownload', year)
  
  # Two party preferred (TPP)
  tmp_pp_tpp <- download_vtr('HouseTppByPollingPlaceDownload', year) %>% 
    select(PollingPlaceID,
           tpp.coalition.votes = Liberal.National.Coalition.Votes,
           tpp.coalition.percent = Liberal.National.Coalition.Percentage,
           tpp.alp.votes = Australian.Labor.Party.Votes,
           tpp.alp.percent = Australian.Labor.Party.Percentage,
           tpp.total.votes = TotalVotes,
           tpp.swing = Swing)
  tmp_pp_base <- tmp_pp_base %>% left_join(tmp_pp_tpp, by = 'PollingPlaceID')
  
  # Clean up
  tmp_pp_base <- tmp_pp_base %>% 
    mutate_at(vars(State, DivisionNm), funs(as.factor(.))) %>% 
    mutate_at(vars(tpp.coalition.votes, tpp.coalition.percent, tpp.alp.votes, tpp.alp.percent, tpp.total.votes, tpp.swing),
              funs(as.numeric(.)))
  
  return(tmp_pp_base)  
}

#### Candidate table functions ####

build_cand_base <- function(year) {
  tmp_cand_base <- download_vtr('HouseStateFirstPrefsByPollingPlaceDownload', year, TRUE) %>% 
    select(StateAb:PartyNm, votes = OrdinaryVotes, swing = Swing)
  # Two candidate preferred (TCP)
  tmp_cand_tcp <- download_vtr('HouseTcpByCandidateByPollingPlaceDownload', year) %>% 
    select(PollingPlaceID, CandidateID, tcp.votes = OrdinaryVotes, tcp.swing = Swing)
  tmp_cand_base <- tmp_cand_base %>% 
    left_join(tmp_cand_tcp, by = c('CandidateID', 'PollingPlaceID'))
  return(tmp_cand_base)
}

# Add the types of declaration votes as dummy polling places
build_cand_dec <- function(year) {
  tmp_cand_dec <- download_vtr('HouseFirstPrefsByCandidateByVoteTypeDownload', year)
  tmp_cand_dec_tcp <- download_vtr('HouseTcpByCandidateByVoteTypeDownload', year)
  
  # Absent votes
  tmp_cand_dec_absent <- tmp_cand_dec %>% 
    select(StateAb:PartyNm, votes = AbsentVotes) %>% 
    mutate(PollingPlace = 'Absent',
           PollingPlaceID = NA, 
           swing = NA,
           type = 'Declaration',
           early = 0)
  
  tmp_cand_dec_absent <- tmp_cand_dec_absent %>% 
    left_join(tmp_cand_dec_tcp %>% 
                select(DivisionID, CandidateID, tcp.votes = AbsentVotes), 
              by = c('DivisionID', 'CandidateID'))

  # Provisional votes
  tmp_cand_dec_provisional <- tmp_cand_dec %>% 
    select(StateAb:PartyNm, votes = ProvisionalVotes) %>% 
    mutate(PollingPlace = 'Provisional',
           PollingPlaceID = NA, 
           swing = NA,
           type = 'Declaration',
           early = 0)
  
  tmp_cand_dec_provisional <- tmp_cand_dec_provisional %>% 
    left_join(tmp_cand_dec_tcp %>% 
                select(DivisionID, CandidateID, tcp.votes = ProvisionalVotes), 
              by = c('DivisionID', 'CandidateID'))
  
  # Pre-poll declaration votes
  tmp_cand_dec_ppdec <- tmp_cand_dec %>% 
    select(StateAb:PartyNm, votes = PrePollVotes) %>% 
    mutate(PollingPlace = 'PrePollDeclaration',
           PollingPlaceID = NA, 
           swing = NA,
           type = 'Declaration',
           early = 1)
  
  tmp_cand_dec_ppdec <- tmp_cand_dec_ppdec %>% 
    left_join(tmp_cand_dec_tcp %>% 
                select(DivisionID, CandidateID, tcp.votes = PrePollVotes), 
              by = c('DivisionID', 'CandidateID'))
    
  # Potal votes
  tmp_cand_dec_postal <- tmp_cand_dec %>% 
    select(StateAb:PartyNm, votes = PostalVotes) %>% 
    mutate(PollingPlace = 'Postal',
           PollingPlaceID = NA, 
           swing = NA,
           type = 'Declaration',
           early = 1)
  
  tmp_cand_dec_postal <- tmp_cand_dec_postal %>% 
    left_join(tmp_cand_dec_tcp %>% 
                select(DivisionID, CandidateID, tcp.votes = PostalVotes), 
              by = c('DivisionID', 'CandidateID'))
  
  return(bind_rows(tmp_cand_dec_absent, tmp_cand_dec_provisional, tmp_cand_dec_ppdec, tmp_cand_dec_postal))
}

# Build the expanded candidate table - Any extra transformations should happen here
build_cand_extra <- function(year) {
  tmp_cand_extra <- build_cand_base(year)
  tmp_cand_extra <- tmp_cand_extra %>% 
    mutate(type = 'Ordinary')
  
  # Mark PPVCs
  tmp_cand_extra <- tmp_cand_extra %>% 
    left_join(pp_2016 %>% 
                select(PollingPlaceID, early = PollingPlaceTypeID) %>% 
                filter(early == 5) %>% 
                mutate(early = 1), by = 'PollingPlaceID')
  tmp_cand_extra$early[is.na(tmp_cand_extra$early)] <- 0
  
  # Add the declaration votes to the table
  tmp_cand_extra <- tmp_cand_extra %>% 
    bind_rows(build_cand_dec(year))
  
  # Mark informal votes
  tmp_cand_extra <- tmp_cand_extra %>% 
    mutate(informal = ifelse(BallotPosition == 999, 1, 0))
  
  # Correct variables types
  tmp_cand_extra <- tmp_cand_extra %>% 
    mutate_at(vars(BallotPosition, votes, swing, tcp.votes, tcp.swing),
              funs(as.numeric(.)))
  
  return(tmp_cand_extra)
}

#### Division table functions ####

build_div_base <- function(year) {
  tmp_div_base <- download_vtr('HouseNominationsByDivisionDownload', year)
  
  # Members elected
  tmp_div_elected <- download_vtr('HouseMembersElectedDownload', year) %>% 
    select(DivisionId = DivisionID,
           elected.candidateID = CandidateID,
           elected.givenNm = GivenNm,
           elected.surname = Surname,
           elected.party = PartyAb)
  tmp_div_base <- tmp_div_base %>% 
    left_join(tmp_div_elected, by = 'DivisionId')
  
  # Divisions which have changed hands
  tmp_div_changed <- download_vtr('HouseSeatsWhichChangedHandsDownload', year) %>% 
    select(DivisionId = DivisionID,
           previous.party = PreviousPartyAb) %>% 
    mutate(changed = 1)
  tmp_div_base <- tmp_div_base %>% 
    left_join(tmp_div_changed, by = 'DivisionId')
  
  # Seats decided on preferences
  tmp_div_first <- download_vtr('HouseSeatsDecidedOnFirstPrefsDownload', year) %>% 
    select(DivisionId = DivisionID) %>% 
    mutate(preferences = 'First')
  
  tmp_div_preferences <- download_vtr('HouseSeatsDecidedOnPrefsDownload', year) %>% 
    select(DivisionId = DivisionID) %>% 
    mutate(preferences = 'Preferences')
  
  tmp_div_decided <- tmp_div_first %>% 
    bind_rows(tmp_div_preferences)
  
  tmp_div_base <- tmp_div_base %>% 
    left_join(tmp_div_decided, by = 'DivisionId')
           
  # Non-classic divisions
  tmp_div_nonclassic <- download_vtr('HouseNonClassicDivisionsDownload', year) %>% 
    # Not sure if the 2016 'DivisionId' capitalisation is a bug for that year only...
    select(DivisionId,
           nonclassic.party1 = PartyAb1,
           noncalssic.party2 = PartyAb2) %>% 
    mutate(nonclassic = 1)
  tmp_div_base <- tmp_div_base %>% 
    left_join(tmp_div_nonclassic, by = 'DivisionId')
  
  # Clean up the table
  tmp_div_base <- tmp_div_base %>% 
    mutate_at(vars(StateAb, DivisionNm, PollingPlace, PartyAb, PartyNm, Elected, HistoricElected, type), 
              funs(as.factor(.))) %>% 
    mutate_at(vars(votes, swing, tcp.votes, tcp.swing), funs(as.numeric(.))) %>% 
    select(-TransactionId, DivisionID = DivisionId, StateAb:nonclassic)
  
  return(tmp_div_base)
}

##### Build tables #####

pp_2016 <- build_pp_base(2016)
pp_2013 <- build_pp_base(2013)
cand_2016 <- build_cand_extra(2016)
cand_2013 <- build_cand_extra(2013)
cand_2010 <- build_cand_extra(2010)
cand_2007 <- build_cand_extra(2007)
cand_2004 <- build_cand_extra(2004)
div_2016 <- build_div_base(2016)

##### Tests #####

cand_2016 %>% 
  group_by(StateAb, DivisionNm, DivisionID) %>% 
  summarise(total.cand = sum(votes)) %>% 
  left_join(pp_2016 %>% 
              group_by(DivisionID) %>% 
              summarise(total.pp = sum(total.votes)), by = 'DivisionID')

cand_2016 %>% 
  group_by(StateAb, informal) %>% 
  summarise(sum(votes))

cand_2016 %>% 
  filter(DivisionNm == "Barton") %>% 
  select(CandidateID, Surname, PollingPlace, PartyAb, type, votes) %>% 
  group_by(Surname, type) %>% 
  summarise(sum(votes))

cand_2016 %>% 
  filter(DivisionNm == "Barton") %>% 
  select(CandidateID, Surname, PollingPlace, PartyAb, type, votes) %>% 
  filter(type == "Declaration") %>% 
  group_by(Surname, PollingPlace) %>% 
  summarise(sum(votes))

cand_2013 %>% 
  group_by(StateAb, DivisionNm, DivisionID) %>% 
  summarise(total.cand = sum(votes))

pp_2016 %>% 
  group_by(DivisionNm) %>% 
  summarise_at(vars(tpp.coalition.votes, tpp.alp.votes, total.votes), funs(sum(.))) %>% 
  mutate(total = tpp.coalition.votes + tpp.alp.votes)