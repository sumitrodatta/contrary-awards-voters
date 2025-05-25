library(janitor)
library(tidyverse)

all_voting=read_csv("All NBA Voting Ballots.csv")

ballot_num=all_voting %>% 
  #greg anthony abstained from all-rookie & roy voting in 2021, as his son cole anthony joined the league
  filter(player != "Abstained") %>% 
  group_by(year,award) %>% tally() %>%
  #divide tally by number of places on ballot
  mutate(num_ballots=case_when(
    award %in% c("MIP","ROY","SMOY","DPOY","COY","CPOY")~n/3,
    award=="MVP"~n/5,
    award %in% c("All-Defense","All-Rook")~n/10,
    award=="All-NBA"~n/15)) %>% select(-n) %>% ungroup()

all_candidates=all_voting %>% 
  #2019 jason anderson missed a player on second-team all-nba, hence incomplete ballot
  filter(!(player %in% c("Abstained","Incomplete Ballot"))) %>%
  #get all candidates of a year's awards, assign 0 points if voter did not vote for them
  group_by(year,award) %>% complete(nesting(voter_name,affiliation),player,fill=list(points_given=0)) %>% ungroup()

contrary_scores=left_join(all_candidates,ballot_num) %>% 
  group_by(year,award,player) %>% 
  #get average points w/o including current voter's ballot
  mutate(tot_pts=sum(points_given),
         avg_pts=(tot_pts-points_given)/(num_ballots-1),
         abs_pt_diff=abs(points_given-avg_pts),
         sq_pt_diff=abs_pt_diff^2) %>% ungroup() %>%
  group_by(year,award,voter_name) %>% mutate(avg_consensus_dev=sqrt(mean(sq_pt_diff))) %>% ungroup()

write_csv(contrary_scores,"Contrary Outputs/Contrary Scores for All Picks.csv")

contrary_scores_by_award=contrary_scores %>% group_by(award) %>% slice_max(sq_pt_diff) %>% 
  select(year:points_given,tot_pts,sq_pt_diff) %>% 
  arrange(desc(sq_pt_diff),desc(year))

write_csv(contrary_scores_by_award,"Contrary Outputs/Most Contrary Single Picks for Each Award.csv")

most_contrary_vote=contrary_scores %>% group_by(year,award,voter_name) %>% 
  slice_max(sq_pt_diff,with_ties=FALSE) %>% select(year:sq_pt_diff)

write_csv(most_contrary_vote,"Contrary Outputs/Most Contrary Vote on Each Voter's Ballot.csv")

yearly_contrary=contrary_scores %>% group_by(year,award,voter_name) %>% slice_max(avg_consensus_dev,with_ties=FALSE) %>%
  group_by(year,voter_name) %>% arrange(desc(avg_consensus_dev)) %>%
  mutate(yr_contrary=sum(avg_consensus_dev),num_awards_voted=n()) %>% slice(1) %>%
  select(year,voter_name,affiliation,yr_contrary:num_awards_voted)

write_csv(yearly_contrary,"Contrary Outputs/Yearly Contrary Scores.csv")

write_csv(yearly_contrary %>% group_by(year) %>% slice_max(yr_contrary),"Contrary Outputs/Most Contrary Voter by Year.csv")

players_by_contrary_score=left_join(contrary_scores,yearly_contrary %>% select(-affiliation)) %>% 
  group_by(player,award,year) %>% filter(points_given>0) %>% 
  summarize(tot_points=sum(points_given),num_voters=n(),avg_yr_contrary_of_voter=mean(yr_contrary)) %>%
  left_join(.,yearly_contrary %>% select(-affiliation) %>% group_by(year) %>% summarize(avg_yr_contrary=mean(yr_contrary))) %>%
  mutate(contrary_over_avg=avg_yr_contrary_of_voter-avg_yr_contrary)

write_csv(players_by_contrary_score,"Contrary Outputs/Players by Contrary Score of Voter.csv")

career_contrary=contrary_scores %>% group_by(voter_name) %>% 
  mutate(num_picks_made=n(),first_vote=min(year),last_vote=max(year),num_yrs_voted=n_distinct(year)) %>%
  group_by(year,award,voter_name) %>% slice_max(avg_consensus_dev,with_ties=FALSE) %>%
  group_by(voter_name) %>% mutate(career_contrary_sc=sum(avg_consensus_dev),num_awards_voted=n(),
                                  contrary_per_ballot=career_contrary_sc/num_awards_voted,
                                  contrary_per_pick=career_contrary_sc/num_picks_made) %>%
  slice(1) %>% 
  select(voter_name,first_vote:num_yrs_voted,career_contrary_sc:contrary_per_ballot,num_picks_made,contrary_per_pick)

write_csv(career_contrary,"Contrary Outputs/Career Contrary Scores.csv")
