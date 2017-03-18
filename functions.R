##### Notes #####

# TODO: Mark out-of-division polling places
# TODO: Grouping variable for co-located polling places, maybe by assigning to SA1 and then grouping by SA1

##### Load Libraries #####
library(dplyr)
library(knitr)

rm(list=ls(pattern="^tmp_"))

#### Divisional summary by year ####

# Function to call the correct election year data tables
# TODO: Check to see if the tables are loaded and load them if they aren't.
# TODO: Error checking - make sure the election and table are valid
data_year <- function(table, year) {
  year <- as.character(year)
  if (table == "cand") {
    tmp_table <- switch(year,
                        '2016' = cand_2016,
                        '2013' = cand_2013,
                        '2010' = cand_2010,
                        '2007' = cand_2007,
                        '2004' = cand_2004)
  } else if (table == 'div') {
    tmp_table <- switch(year,
                        '2016' = div_2016,
                        '2013' = div_2013)
  } else if (table == 'pp'){
    tmp_table <- switch(year,
                        '2016' = pp_2016,
                        '2013' = pp_2013)
  }
  return(tmp_table)
}

div_table <- function(division, year) {
  # Note that the Percent variable here includes informals, the AEC calculates vote percent of total formal vote
  tmp_div <- data_year('cand', year)
  tmp_div <- tmp_div[tmp_div$DivisionNm == division,]
  
  # Declaration votes
  tmp_dec <- tmp_div %>% 
    filter(type == 'Declaration') %>% 
    group_by(CandidateID, Surname, GivenNm, PartyAb, Elected, HistoricElected, Type=PollingPlace) %>% 
    summarise(votes = sum(votes)) %>% 
    spread(Type, votes)
  
  # Ordinary votes (Pre-poll ordinary votes did not exist before 2007)
  if(year > 2007) {
    tmp_ord <- tmp_div %>% 
      filter(type == 'Ordinary') %>% 
      mutate(early = ifelse(early == 1, 'PrePollOrdinary', 'Ordinary')) %>% 
      group_by(CandidateID, early)%>% 
      summarise(votes = sum(votes)) %>% 
      spread(early, votes) %>% 
      mutate(TotalOrdinary = Ordinary + PrePollOrdinary)
  } else {
    tmp_ord <- tmp_div %>% 
      filter(type == 'Ordinary') %>% 
      group_by(CandidateID, type) %>% 
      summarise(votes = sum(votes)) %>% 
      spread(type, votes) %>% 
      mutate(TotalOrdinary = Ordinary)
  }
  
  # All votes
  tmp_table <- tmp_dec %>% 
    left_join(tmp_ord, by = 'CandidateID') %>% 
    mutate(TotalVotes = sum(Absent, Postal, PrePollDeclaration, Provisional, TotalOrdinary))
  
  tmp_table <- tmp_table %>%
    ungroup %>% 
    mutate(Percent = TotalVotes/sum(TotalVotes) * 100) %>% 
    bind_rows(tmp_table %>%
                ungroup %>%
                summarise_at(vars(Absent:TotalVotes), funs(sum(.))) %>%
                mutate(Surname = 'Total', GivenNm = '', PartyAb = ''))

  return(tmp_table)
}

div_table_informal <- function(division, year) {
  tmp_div_table <- div_table(division, year)
  tmp_formal <- tmp_div_table %>% 
    ungroup %>% 
    filter(CandidateID != 999 & Surname != 'Total') %>% 
    summarise_at(vars(Absent:TotalVotes), funs(sum(.))) %>% 
    mutate(Surname = 'Formal', GivenNm = '', PartyAb = '')
  
  tmp_informal <- tmp_formal %>% 
    bind_rows(tmp_div_table %>% filter(CandidateID == 999)) %>% 
    mutate(GivenNm = '') %>% 
    mutate(Percent = TotalVotes / sum(TotalVotes) * 100)
  
  tmp_total <- tmp_informal %>% 
    summarise_at(vars(Absent:TotalVotes), funs(sum(.))) %>% 
    mutate(Surname = 'Total', GivenNm = '', PartyAb = '')
  
  tmp_table <- tmp_informal %>% bind_rows(tmp_total)
  
  return(tmp_table)
}

div_tcp <- function(division, year){
  # This is exactly the same as div_table except it uses tcp.vote instead of vote, and filters for !is.na(tcp.vote)
  tmp_div <- data_year('cand', year)
  tmp_div <- tmp_div[tmp_div$DivisionNm == division,]
  
  tmp_div <- tmp_div %>% 
    filter(!is.na(tcp.votes)) 
  
  # Declaration votes
  tmp_dec <- tmp_div %>% 
    filter(type == "Declaration") %>% 
    group_by(CandidateID, Surname, GivenNm, PartyAb, Elected, HistoricElected, Type=PollingPlace) %>% 
    summarise(tcp.votes = sum(tcp.votes)) %>% 
    spread(Type, tcp.votes)

  # Ordinary votes (Pre-poll ordinary votes did not exist before 2007)
  if(year > 2007) {
    tmp_ord <- tmp_div %>% 
      filter(type == 'Ordinary') %>% 
      mutate(early = ifelse(early == 1, 'PrePollOrdinary', 'Ordinary')) %>% 
      group_by(CandidateID, early)%>% 
      summarise(tcp.votes = sum(tcp.votes)) %>% 
      spread(early, tcp.votes) %>% 
      mutate(TotalOrdinary = Ordinary + PrePollOrdinary)
  } else {
    tmp_ord <- tmp_div %>% 
      filter(type == 'Ordinary') %>% 
      group_by(CandidateID, type) %>% 
      summarise(tcp.votes = sum(tcp.votes)) %>% 
      spread(type, tcp.votes) %>% 
      mutate(TotalOrdinary = Ordinary)
  }
  
  # # Ordinary votes
  # tmp_ord <- tmp_div %>% 
  #   filter(type == "Ordinary") %>% 
  #   mutate(early = ifelse(early == 1, 'PrePollOrdinary', 'Ordinary')) %>% 
  #   group_by(CandidateID, early)%>% 
  #   summarise(tcp.votes = sum(tcp.votes)) %>% 
  #   spread(early, tcp.votes) %>% 
  #   mutate(TotalOrdinary = Ordinary + PrePollOrdinary)
  
  # All votes
  tmp_table <- tmp_dec %>% 
    left_join(tmp_ord, by = 'CandidateID') %>% 
    mutate(TotalVotes = sum(Absent, Postal, PrePollDeclaration, Provisional, TotalOrdinary))
  
  tmp_table <- tmp_table %>%
    ungroup %>% 
    mutate(Percent = TotalVotes/sum(TotalVotes) * 100) %>% 
    bind_rows(tmp_table %>%
                ungroup %>%
                summarise_at(vars(Absent:TotalVotes), funs(sum(.))) %>%
                mutate(Surname = 'Total', GivenNm = '', PartyAb = ''))
  
  return(tmp_table)  
}

div_tcp_percent <- function(division, year){
  tmp_tcp <- div_tcp(division, year) %>%
    filter(Surname != "Total") %>% 
    select(-Percent, -CandidateID, -TotalOrdinary) %>% 
    mutate_at(vars(Absent:TotalVotes), funs(./sum(.)*100))
  
  return(tmp_tcp)
}

div_pp_tcp <- function(division, year){
  # List TCP vote and percent for the division by polling place
  tmp_pp_tcp <- data_year('cand', year)
  tmp_pp_tcp <- tmp_pp_tcp[tmp_pp_tcp$DivisionNm == division,]
  
  tmp_pp_tcp <- tmp_pp_tcp %>% 
    filter(!is.na(tcp.votes)) %>% 
    group_by(PollingPlace, PartyAb) %>% 
    summarise(votes = sum(tcp.votes)) %>% 
    spread(PartyAb, votes) %>% 
    ungroup
  
  tmp_pp_tcp$Total <- rowSums(tmp_pp_tcp[2:3])
  tmp_pp_tcp[[gsub('^', 'percent.', names(tmp_pp_tcp[2]))]] <- tmp_pp_tcp[[2]]/tmp_pp_tcp[[4]]*100
  tmp_pp_tcp[[gsub('^', 'percent.', names(tmp_pp_tcp[3]))]] <- tmp_pp_tcp[[3]]/tmp_pp_tcp[[4]]*100
  
  return(tmp_pp_tcp)
}

div_summary <- function(division, year) {
  
  # Replace Percent, which comes from div_table() as percent of all votes, with just percent of formals
  tmp_div_table <- div_table(division, year) %>% 
    filter(CandidateID != 999) %>% 
    ungroup %>% 
    mutate(Percent = TotalVotes/sum(TotalVotes) * 100)
  
  tmp_div_informal <- div_table_informal(division, year) %>% 
    mutate(Elected = '', HistoricElected = '')
  if(year > 2007) {
  tmp_div_complete <- tmp_div_table %>% 
    bind_rows(tmp_div_informal) %>% 
    ungroup %>% 
    select(-CandidateID, -TotalOrdinary, 
           Surname:Elected, Historic = HistoricElected, Absent, Postal,
           PrePollDec = PrePollDeclaration, Provisional, PrePollOrdinary, Ordinary, TotalVotes, Percent)
  } else {
    tmp_div_complete <- tmp_div_table %>% 
      bind_rows(tmp_div_informal) %>% 
      ungroup %>% 
      select(-CandidateID, -TotalOrdinary, 
             Surname:Elected, Historic = HistoricElected, Absent, Postal,
             PrePollDec = PrePollDeclaration, Provisional, Ordinary, TotalVotes, Percent)
  }
  
  print(kable(tmp_div_complete, digits = 2))
  
  # TCP percent
  print(kable(div_tcp_percent(division, year), digits = 2))
}

#### Polling place result tables ####

pp_by_party <- function(year){
  # Table of each party's vote by polling place
  tmp_pp <- data_year('cand', year)
  tmp_pp <- tmp_pp %>% 
    mutate(PartyAb = ifelse(CandidateID == 999, 'Informal', PartyAb)) %>%
    group_by(StateAb, DivisionID, DivisionNm, PollingPlaceID, PollingPlace, PartyAb) %>% 
    summarise(votes = sum(votes, na.rm=TRUE)) %>% 
    spread(PartyAb, votes) %>% 
    select(-Informal, Informal)
  
  # Calculate totals
  tmp_pp$total.pp <- rowSums(tmp_pp[,6:dim(tmp_pp)[2]], na.rm = TRUE)
  tmp_pp$total.formal <- rowSums(tmp_pp[,6:(dim(tmp_pp)[2]-3)], na.rm = TRUE)
  
  # Add a column for the combined Coalition vote, but make sure it isn't added to the total
  # Note the LNP only existed from 2010 onwards, but in 2010 they were LNQ and from 2013 LNP.
  if(year > 2010) {
    tmp_coalition <- c('LP', 'NP', 'LNP', 'CLP')
  } else if(year == 2010) {
    tmp_coalition <- c('LP', 'NP', 'LNQ', 'CLP')
  } else {
    tmp_coalition <- c('LP', 'NP', 'CLP')
  }
  # tmp_coalition <- ifelse(year > 2007, c('LP', 'NP', 'LNP', 'CLP'), c('LP', 'NP', 'CLP'))
  tmp_pp_coaliton <- tmp_pp %>%
    ungroup %>% 
    select(one_of(tmp_coalition))
  
  tmp_pp$group.Coalition <- rowSums(tmp_pp_coaliton, na.rm = TRUE)
  
  # Add a column for non-major parties ('others')
  tmp_majors <- c('ALP', 'GRN', tmp_coalition)
  
  tmp_pp_others <- tmp_pp %>% 
    ungroup %>% 
    select(-one_of(tmp_majors), -group.Coalition, -Informal, -total.pp, -total.formal) %>% 
    select_if(is.numeric)
  
  tmp_pp$group.Others <- rowSums(tmp_pp_others, na.rm = TRUE)
  
  # Change the order to what other functions -- like pp_by_party_percent() -- expect
  tmp_pp <- tmp_pp %>%
    select(-total.formal, -total.pp, -group.Coalition, -group.Others,
           group.Coalition, group.Others, total.formal, total.pp)

  return(tmp_pp)
}

pp_by_party_percent <- function(year){
  # Return pp_by_party as a percentage of the total *formal* vote at the polling place
  # NOTE: This is quite slow - dump it into a variable if you're doing it more than once.
  tmp_pp <- pp_by_party(year)
  
  tmp_pp <- tmp_pp %>% 
    mutate_if(is.numeric, funs(./total.formal * 100)) %>% 
    select(-Informal, -total.pp, -total.formal)
  
  return(tmp_pp)
}

div_by_party <- function(year){
  tmp_table <- pp_by_party(year) %>% 
    group_by(StateAb, DivisionID, DivisionNm) %>% 
    summarise_if(is.numeric, funs(sum(., na.rm = TRUE)))
  
  return(tmp_table)
}

#### Misc polling place functions ####

# Calculates the geodesic distance between two points specified by radian latitude/longitude using the
# Haversine formula (hf)
# From https://www.r-bloggers.com/great-circle-distance-calculations-in-r/

deg2rad <- function(deg) {(deg * pi) / (180)}

gcd.hf <- function(long1, lat1, long2, lat2) {
  R <- 6371 # Earth mean radius [km]
  delta.long <- (deg2rad(long2) - deg2rad(long1))
  delta.lat <- (deg2rad(lat2) - deg2rad(lat1))
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
  c <- 2 * asin(min(1,sqrt(a)))
  d = R * c
  return(d) # Distance in km
}

pp_distance <- function(year1, year2){
  # Using the fairly arbitrary threshold of 200 metres for determining when a polling place had moved.
  # TODO: Try and guestimate by name and address those missing geocodes
  tmp_pp_year1 <- data_year('pp', year1)
  tmp_pp_year2 <- data_year('pp', year2)
  
  tmp_pp_table <- tmp_pp_year1 %>%
    select(State:Longitude) %>% 
    full_join(tmp_pp_year2 %>% 
                select(State:Longitude), 
              by = 'PollingPlaceID') %>% 
    mutate_at(vars(starts_with('L')), funs(as.numeric))
  
  tmp_pp_table$distance <- mapply(gcd.hf, 
                                  tmp_pp_table$Longitude.x, tmp_pp_table$Latitude.x, tmp_pp_table$Longitude.y, tmp_pp_table$Latitude.y)
  
  tmp_pp_table <- tmp_pp_table %>% 
    mutate(status = ifelse(is.na(PollingPlaceNm.y), 'Abolished',
                           ifelse(PollingPlaceTypeID.x == '2' & is.na(distance), 'Mobile',
                           ifelse(is.na(distance), 'MissingGeo',
                           ifelse(distance < 0.2, 'Same', 'Moved')))))
  
  return(tmp_pp_table)
}

div_votes_by_type <- function(year, percentage = FALSE) {
  tmp_div <- data_year('cand', year)

  # Declaration votes
  tmp_dec <- tmp_div %>% 
    filter(type == 'Declaration') %>% 
    group_by(StateAb, DivisionNm, DivisionID, Type=PollingPlace) %>% 
    summarise(votes = sum(votes)) %>% 
    spread(Type, votes)
  
  # Ordinary votes (Pre-poll ordinary votes did not exist before 2007)
  if(year > 2007) {
    tmp_ord <- tmp_div %>% 
      filter(type == 'Ordinary') %>% 
      mutate(early = ifelse(early == 1, 'PrePollOrdinary', 'Ordinary')) %>% 
      group_by(DivisionID, early)%>% 
      summarise(votes = sum(votes)) %>% 
      spread(early, votes) %>% 
      mutate(TotalOrdinary = Ordinary + PrePollOrdinary)
  } else {
    tmp_ord <- tmp_div %>% 
      filter(type == 'Ordinary') %>% 
      group_by(DivisionID, type) %>% 
      summarise(votes = sum(votes)) %>% 
      spread(type, votes) %>% 
      mutate(TotalOrdinary = Ordinary)
  }
  
  # All votes
  tmp_table <- tmp_dec %>% 
    left_join(tmp_ord, by = 'DivisionID') %>% 
    mutate(TotalVotes = sum(Absent, Postal, PrePollDeclaration, Provisional, TotalOrdinary))
  
  if(percentage) {
    tmp_table <- tmp_table %>% 
      mutate_at(vars(Absent:TotalOrdinary), funs(./TotalVotes * 100)) %>% 
      select(-TotalVotes)
  }
  
  return(tmp_table)
}

#### Misc general functions ####

nat_type_by_year <- function(start_year, percentage = FALSE){
  # Table of vote type (including pre-poll ordinary) by year from {start_year}
  
  # Vector of election years since {start_year}
  tmp_years <- rev(event_ids$year)[match(start_year, rev(event_ids$year)):length(rev(event_ids$year))]
  
  tmp_table <- data.frame(NULL)
  
  for (year in tmp_years) {
    tmp_year <- div_votes_by_type(year) %>% 
      ungroup %>% 
      summarise_at(vars(Absent:TotalVotes), funs(sum)) %>% 
      mutate_(election = ~year)
    if(dim(tmp_table)[1] < 1) {
      tmp_table <- tmp_year
    } else {
      tmp_table <- tmp_table %>% 
        bind_rows(tmp_year)
    }
  }

  if(percentage) {
    tmp_table <- tmp_table %>% 
      mutate_at(vars(-election, -TotalVotes), funs(./TotalVotes * 100)) %>% 
      select(-TotalVotes)
  }
  return(tmp_table %>% select(-election, -TotalOrdinary, election))
}

state_primary_by_group <- function(year, national = FALSE, percentage = FALSE) {
  # Note: Party/Group percentages are percentages of *formal* vote, and percentage of formal votes is
  # the percentage of *total* votes. Hence state rows won't equal 100 per cent.
  tmp_state <- pp_by_party(year) %>% 
    group_by(StateAb) %>% 
    summarise_at(vars(ALP, group.Coalition, GRN, group.Others, Informal, total.formal, total.pp), funs(sum(., na.rm = TRUE))) %>% 
    arrange(desc(total.pp))
  
  if(national){
    tmp_state <- tmp_state %>% 
      summarise_at(vars(-StateAb), funs(sum(.))) %>% 
      mutate_(StateAb = ~year)
  }
  
  if(percentage) {
    tmp_state <- tmp_state %>% 
      mutate_at(vars(ALP, group.Coalition, GRN, group.Others), funs(./total.formal * 100)) %>% 
      mutate_at(vars(Informal), funs(./total.pp * 100))
  }
  
  tmp_state <- tmp_state %>% 
    select(StateAb, Coalition = group.Coalition, ALP, GRN, Others = group.Others, Informal, Total = total.pp)
  
  if(percentage) {
    tmp_state <- tmp_state %>% 
      select(-Total)
  }
  
  return(tmp_state)
}

nat_primary_by_year <- function(start_year, percentage = FALSE) {
  tmp_years <- rev(event_ids$year)[match(start_year, rev(event_ids$year)):length(rev(event_ids$year))]
  
  tmp_table <- do.call('bind_rows', lapply(tmp_years, state_primary_by_group, national = TRUE, percentage = percentage))
  
  names(tmp_table)[1] <- 'Election'
  
  return(tmp_table)
}