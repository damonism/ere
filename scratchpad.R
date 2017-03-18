# http://results.aec.gov.au/20499/Website/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-20499-NSW.csv
# 
# http://results.aec.gov.au/20499/Website/Downloads/HouseFirstPrefsByStateByPartyDownload-20499.csv
# 


tmp_x %>% 
  filter(type == "Ordinary") %>% 
  mutate(early = ifelse(early == 1, 'PPO', 'Ordinary')) %>% 
  group_by(CandidateID, early)%>% 
  summarise(votes = sum(votes)) %>% 
  spread(early, votes)

tmp_x <-cand_2016 %>% 
  filter(DivisionNm == 'Melbourne' & !is.na(tcp.votes)) %>% 
  group_by(PollingPlace, PartyAb) %>% 
  summarise(votes = sum(tcp.votes)) %>% 
  spread(PartyAb, votes) %>% 
  ungroup

tmp_x$Total <- rowSums(tmp_x[2:3])
tmp_x[[gsub('^', 'percent.', names(tmp_x[2]))]] <- tmp_x[[2]]/tmp_x[[4]]*100
tmp_x[[gsub('^', 'percent.', names(tmp_x[3]))]] <- tmp_x[[3]]/tmp_x[[4]]*100

tmp_x$percent <- tmp_x[,3]/tmp_x$Total * 100

assign(gsub('^', 'percent.', names(tmp_x[3])), tmp_x[,3]/tmp_x$Total * 100) 
assign(gsub('^', 'percent.', names(tmp_x[2])), as.list(tmp_x[,2]/tmp_x$Total * 100)) 

div_pp_tcp('Perth', 2013) %>% 
  left_join(div_pp_tcp('Perth', 2016), by = 'PollingPlace') %>% 
  mutate(swing.LP = percent.LP.x - percent.LP.y) %>% 
  select(PollingPlace, swing.LP) %>% 
  left_join(cand_2016 %>% filter(Elected == 'Y' & DivisionNm == 'Perth') %>% select(PollingPlace, tcp.swing), by = 'PollingPlace')

tmp_z <- pp_2013 %>% 
  select(PollingPlaceID, PollingPlaceNm, Latitude, Longitude) %>% 
  full_join(pp_2016 %>% select(PollingPlaceID, PollingPlaceNm, Latitude, Longitude), by = 'PollingPlaceID') %>%
  #filter(complete.cases(.)) %>%
  #filter(PollingPlaceID == '64583') %>% 
  filter(Latitude.x != Latitude.y | Longitude.x != Longitude.y) %>%
  mutate_at(vars(starts_with('L')), funs(as.numeric))

tmp_z <- tmp_z[complete.cases(tmp_z),]

tmp_z %>% 
  mutate(distance = gcd.hf(Longitude.x, Latitude.x, Longitude.x, Longitude.y))

gcd.hf(tmp_z$Longitude.x, tmp_z$Latitude.x, tmp_z$Latitude.y, tmp_z$Longitude.y)

  head(100) %>% 
  mutate(distance = gcd.hf(Longitude.x, Latitude.x, Longitude.x, Latitude.y)) %>% View()
  
  filter(!is.na(Latitude.x)|!is.na(Latitude.y)|!is.na(Longitude.x)|!is.na(Longitude.y)) %>% View
  mutate(distance = gcd.hf(Longitude.x, Latitude.x, Longitude.x, Longitude.y))
  
  
apply(tmp_z[1,c(3:4,6:7)], 1, gcd.hf, long1 = 'Longitude.x', lat1 = 'Latitude.x', long2 = 'Longitude.y', lat2 = 'Latitude.y')

tmp_z$distance <- mapply(gcd.hf, tmp_z$Longitude.x, tmp_z$Latitude.x, tmp_z$Longitude.y, tmp_z$Latitude.y)
