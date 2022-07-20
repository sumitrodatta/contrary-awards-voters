library(janitor)
library(tidyverse)
library(pdftools)

source("scraping funcs.R")

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

finalized_all_voting = all_voting %>%
  mutate(player=case_when(
    #3 players in all-def 2015 have one dash rather than two
    str_detect(player," - IND")~"George Hill",
    str_detect(player," - SA")~"Danny Green",
    str_detect(player," - Mil")~"Giannis Antetokounmpo",
    str_detect(player,"Barea")~"J.J. Barea",
    str_detect(player,"Michael Jr. Porter")~"Michael Porter Jr.",
    str_detect(player,"PJ Tucker")~"P.J. Tucker",
    str_detect(player,"R.J. Barrett")~"RJ Barrett",
    str_detect(player,"TJ McConnell")~"T.J. McConnell",
    str_detect(player,"TJ Warren")~"T.J. Warren",
    TRUE~player)) %>%
  #replace non-ascii dashes
  mutate(player=str_replace(player,"\u2010","-"))

write_csv(finalized_all_voting,"All NBA Voting Ballots.csv")
