library(janitor)
library(tidyverse)
library(pdftools)

voting_tibble<-function(year=2020,award){
  pdf_name<-paste0("voting pdfs/",year," ",award,".pdf")
  a<-pdf_text(pdf_name) %>% read_lines() %>% str_trim(.) %>% 
    str_split(.,'\\s{2,100}')
  a=tibble(a) %>% unnest_wider(col=a)
  if (!(award %in% c("All-NBA","All-Defense")) & year > 2017){
    colnames(a)=a[2,]
  }
  else if (award  %in% c("All-NBA","All-Defense") & year %in% 2016:2017){
    colnames(a)=a[4,]
  }
  else if ((award == "All-Defense" & year==2020)|(award=="All-Rook" & year %in% 2018:2020)){
    colnames(a)=a[2,]
  }
  else{
    colnames(a)=a[3,]
  }
  a=a %>% clean_names()
  colnames(a)[1:2]=c("voter_name","affiliation")
  if (!(award %in% c("MVP","All-Defense","All-NBA","All-Rook"))){
    colnames(a)[3:5]=c("first_place_five_pts","second_place_three_pts","third_place_one_pt")
  }
  else if (award == "All-Defense"){
    colnames(a)[3:12]=c("first_fwd","first_fwd_2","first_cen","first_g","first_g_2",
                        "second_fwd","second_fwd_2","second_cen","second_g","second_g_2")
  }
  else if (award == "All-Rook"){
    colnames(a)[3:12]=c("first","first_2","first_3","first_4","first_5","second","second_2",
                        "second_3","second_4","second_5")
  }
  a=a %>% 
    filter(!(tolower(affiliation) %in% c(NA,"affiliation","affilaition","","media affiliation","2017‐18 all‐defensive second team"))) %>% 
    mutate(year=year,award=award)
  if (award == "All-NBA"){
    a=a %>% filter(tolower(voter_name) != "all-nba first team")
  }
  return(a)
}
all_yrs_voting_tibble<-function(award){
  totals=voting_tibble(award=award)
  sapply(2019:2015,function(x){
    new_seas=voting_tibble(year=x,award=award)
    totals<<-rbind(totals,new_seas)
  })
  return(totals)
}
mvp=all_yrs_voting_tibble("MVP") %>% slice(-c(666:678))
roy=all_yrs_voting_tibble("ROY") %>% slice(-c(662:669))
dpoy=all_yrs_voting_tibble("DPOY") %>% slice(-c(661:680))
mip=all_yrs_voting_tibble("MIP") %>% slice(-c(661:692))
smoy=all_yrs_voting_tibble("SMOY")
coy=all_yrs_voting_tibble("COY") %>% slice(-c(661:676))
all_nba=all_yrs_voting_tibble("All-NBA")
all_def=all_yrs_voting_tibble("All-Defense") #extra row of 2017-18 all d second team?
all_rook=all_yrs_voting_tibble("All-Rook")

#write csvs and edit in excel

#found the 2014's later thru wayback machine, so edited separately
#all except all-nba able to copy directly from pdf to excel to csv
#write_csv(voting_tibble(2014,"All-NBA"),"all_nba_2014.csv")

#read back in edited csv's
mvp<-read_csv("mvp.csv") %>% 
  pivot_longer(cols=x1st_place_10_points:x5th_place_1_point,names_to="points",values_to="player")
roy<-read_csv("roy.csv") %>%
  pivot_longer(cols=first_place_five_pts:third_place_one_pt,names_to="points",values_to="player")
dpoy<-read_csv("dpoy.csv") %>%
  pivot_longer(cols=first_place_five_pts:third_place_one_pt,names_to="points",values_to="player")
mip<-read_csv("mip.csv") %>%
  pivot_longer(cols=first_place_five_pts:third_place_one_pt,names_to="points",values_to="player")
smoy<-read_csv("smoy.csv") %>%
  pivot_longer(cols=first_place_five_pts:third_place_one_pt,names_to="points",values_to="player")
coy<-read_csv("coy.csv") %>%
  pivot_longer(cols=first_place_five_pts:third_place_one_pt,names_to="points",values_to="player")
all_nba<-read_csv("all-nba.csv") %>%
  pivot_longer(cols=forward:guard_6,names_to="points",values_to="player")
all_def<-read_csv("all-def.csv") %>%
  pivot_longer(cols=first_fwd:second_g_2,names_to="points",values_to="player")
all_rook<-read_csv("all-rook.csv") %>%
  pivot_longer(cols=first:second_5,names_to="points",values_to="player")

all_voting=mvp %>% add_row(roy) %>% add_row(dpoy) %>% add_row(mip) %>% 
  add_row(smoy) %>% add_row(coy) %>% add_row(all_nba) %>% add_row(all_def) %>% add_row(all_rook)

rm(mvp,roy,dpoy,mip,smoy,coy,all_nba,all_def,all_rook)

all_voting=all_voting %>% mutate(player=gsub("\\(.*","",player)) %>% 
       mutate(player=gsub("--.*","",player)) %>% mutate(player=str_trim(player)) %>% 
       separate(player,into=c("last","first"),sep=", ",convert = TRUE) %>%
       unite("player",c(first,last),sep=" ",na.rm=TRUE) %>% 
  mutate(points_given=case_when(
    str_detect(points,"x1st")~10,
    str_detect(points,"x2nd")~7,
    str_detect(points,"x3rd|first_place")~5,
    str_detect(points,"x4th|second_place")~3,
    str_detect(points,"x5th|third_place")~1,
    (award %in% c("All-Defense","All-Rook") & str_detect(points,"first"))~2,
    (award %in% c("All-Defense","All-Rook") & str_detect(points,"second"))~1,
    (award == "All-NBA" & points %in% c("forward","forward_2","center","guard","guard_2"))~5,
    (award == "All-NBA" & points %in% c("forward_3","forward_4","center_2","guard_3","guard_4"))~3,
    (award == "All-NBA" & points %in% c("forward_5","forward_6","center_3","guard_5","guard_6"))~1)) %>%
  select(-points)

#voter names standardization
all_voting=all_voting %>% mutate(voter_name=case_when(
  voter_name=="Sherrod Blakely"~"A. Sherrod Blakely",
  voter_name=="Diego Martinez"~"Diego Martínez Cabrera",
  voter_name=="Israel Gutierez"~"Israel Gutierrez",
  voter_name=="Israel Guttierez"~"Israel Gutierrez",
  voter_name=="John Krawczynski"~"Jon Krawczynski",
  voter_name=="KC Johnson"~"K.C. Johnson",
  voter_name=="Manny Navaro"~"Manny Navarro",
  voter_name=="MIchael Wilbon"~"Michael Wilbon",
  voter_name=="Mike Wilbon"~"Michael Wilbon",
  str_detect(voter_name,"NBA")~"NBA Fan Vote",
  str_detect(voter_name,"Rosalyn")~"Rosalyn Gold-Onwude",
  voter_name=="Vince Goodwill"~"Vincent Goodwill",
  !(is.na(voter_name))~voter_name))

ballot_num=all_voting %>% group_by(year,award) %>% tally() %>% mutate(num_ballots=case_when(
  award %in% c("MIP","ROY","SMOY","DPOY","COY")~n/3,
  award=="MVP"~n/5,
  award %in% c("All-Defense","All-Rook")~n/10,
  award=="All-NBA"~n/15)) %>% select(-n)


all_voting=left_join(all_voting,ballot_num) %>% group_by(year,award,player) %>% 
  mutate(tot_pts=sum(points_given),
         avg_pts=(tot_pts-points_given)/(num_ballots-1),
         abs_pt_diff=abs(points_given-avg_pts),
         sq_pt_diff=abs_pt_diff^2) %>% ungroup() %>%
  group_by(year,award,voter_name) %>% mutate(avg_consensus_dev=sqrt(mean(sq_pt_diff)))

write_csv(all_voting,"All NBA Voting Ballots.csv")

write_csv(all_voting %>% group_by(award) %>% slice_max(sq_pt_diff) %>% 
            select(voter_name:points_given,tot_pts,sq_pt_diff) %>% 
            arrange(desc(sq_pt_diff),desc(year)),"Most Contrary Single Picks for Each Award.csv")

most_contrary_vote=all_voting %>% group_by(year,award,voter_name) %>% 
  slice_max(sq_pt_diff,with_ties=FALSE) %>% select(voter_name:sq_pt_diff)

write_csv(most_contrary_vote,"Most Contrary Vote on Each Voter's Ballot.csv")

yearly_contrary=all_voting %>% group_by(year,award,voter_name) %>% slice_max(avg_consensus_dev,with_ties=FALSE) %>%
  group_by(year,voter_name) %>% arrange(desc(avg_consensus_dev)) %>%
  mutate(yr_contrary=sum(avg_consensus_dev),num_awards_voted=n()) %>% slice(1) %>%
  select(voter_name:year,yr_contrary:num_awards_voted)

write_csv(yearly_contrary,"Yearly Contrary Scores.csv")

write_csv(yearly_contrary %>% group_by(year) %>% slice_max(yr_contrary),"Most Contrary Voter by Year.csv")

career_contrary=all_voting %>% group_by(voter_name) %>% 
  mutate(num_picks_made=n(),first_vote=min(year),last_vote=max(year)) %>%
  group_by(year,award,voter_name) %>% slice_max(avg_consensus_dev,with_ties=FALSE) %>%
  group_by(voter_name) %>% mutate(career_contrary_sc=sum(avg_consensus_dev),num_awards_voted=n(),
                                  contrary_per_ballot=career_contrary_sc/num_awards_voted,
                                  contrary_per_pick=career_contrary_sc/num_picks_made) %>%
  slice(1) %>% 
  select(voter_name,first_vote:last_vote,career_contrary_sc:contrary_per_pick,num_picks_made)

write_csv(career_contrary,"Career Contrary Scores.csv")
