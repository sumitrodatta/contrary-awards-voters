library(janitor)
library(tidyverse)

all_voting=read_csv("All NBA Voting Ballots.csv")

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

write_csv(all_voting,"Contrary Scores for All Picks.csv")

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