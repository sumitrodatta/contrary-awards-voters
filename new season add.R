library(janitor)
library(tidyverse)
library(pdftools)

source("scraping funcs.R")
curr_year=2024
mvp=voting_tibble(year=curr_year,award="MVP")
roy=voting_tibble(year=curr_year,award="ROY")
dpoy=voting_tibble(year=curr_year,award="DPOY")
mip=voting_tibble(year=curr_year,award="MIP")
smoy=voting_tibble(year=curr_year,award="SMOY")
coy=voting_tibble(year=curr_year,award="COY")
cpoy=voting_tibble(year=curr_year,award="CPOY")
all_nba=voting_tibble(year=curr_year,award="All-NBA")
all_def=voting_tibble(year=curr_year,award="All-Defense")
all_rook=voting_tibble(year=curr_year,award="All-Rook")

#2021 edits
# all_def=all_def %>% filter(!is.na(first_fwd))
# all_nba=all_nba %>% slice(-1) %>% 
#   rename(forward=q30,forward_2=q31,center=q32,guard=q33,guard_2=q34,
#          forward_3=q35,forward_4=q36,center_2=q37,guard_3=q38,guard_4=q39,
#          forward_5=q40,forward_6=q41,center_3=q42,guard_5=na,guard_6=na_2)

#2022 edits
# mvp=mvp %>% rename(x1st_place_10_points=na_2,
#                x2nd_place_7_points=na_3,
#                x3rd_place_5_points=na_4,
#                x4th_place_3_points=na_5,
#                x5th_place_1_point=na_6)
# roy=roy %>% select(-na_5)
# all_nba=all_nba %>% filter(!is.na(forward_2))
# all_def=all_def %>% filter(!is.na(first_fwd)) %>% select(-na)

#2023 edits
#all_def=all_def %>% filter(!is.na(first_fwd))

#2024 edits - all-nba and all-defense now positionless

append_new<-function(award,season){
  csv_name=paste0(str_replace(deparse(substitute(award)),"_","-"),".csv")
  old_csv=read_csv(csv_name) %>% filter(year != season)
  write_csv(old_csv %>% bind_rows(award) %>% arrange(desc(year)),csv_name)
}
append_new(mvp,curr_year)
append_new(roy,curr_year)
append_new(dpoy,curr_year)
append_new(mip,curr_year)
append_new(smoy,curr_year)
append_new(coy,curr_year)
append_new(cpoy,curr_year)

append_new(all_nba,curr_year)
append_new(all_def,curr_year)
append_new(all_rook,curr_year)

file_list=c("mvp.csv","roy.csv","dpoy.csv","mip.csv","smoy.csv","coy.csv","cpoy.csv",
           "all-nba.csv","all-def.csv","all-rook.csv")

sapply(file_list, function(x){
  corrected=read_csv(x) %>% separate(voter_name,c("last_name","first_name"),sep=", ") %>%
    mutate(voter_name=ifelse(!is.na(first_name),
                             paste(first_name, last_name),
                             last_name),.before="last_name") %>%
    select(-c(last_name,first_name))
  write_csv(corrected,x)
})

#go into csv's to edit merged ballots

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
cpoy<-read_csv("cpoy.csv") %>%
  pivot_longer(cols=first_place_five_pts:third_place_one_pt,names_to="points",values_to="player")
all_nba<-read_csv("all-nba.csv") %>%
  pivot_longer(cols=c(forward:guard_6,starts_with("all_nba")),names_to="points",values_to="player",values_drop_na = TRUE)
all_def<-read_csv("all-def.csv") %>%
  pivot_longer(cols=c(first_fwd:second_g_2,starts_with("all_defensive")),names_to="points",values_to="player",values_drop_na = TRUE)
all_rook<-read_csv("all-rook.csv") %>%
  pivot_longer(cols=first:second_5,names_to="points",values_to="player")

all_voting=mvp %>% add_row(roy) %>% add_row(dpoy) %>% add_row(mip) %>% add_row(cpoy) %>%
  add_row(smoy) %>% add_row(coy) %>% add_row(all_nba) %>% add_row(all_def) %>% add_row(all_rook)

rm(mvp,roy,dpoy,mip,smoy,coy,cpoy,all_nba,all_def,all_rook)

all_voting_player_fix=all_voting %>% 
  #these 4 2023 awards have no comma between last name & first name
  mutate(player=case_when(year==2023 & award %in% c("All-NBA","All-Defense","MVP","DPOY")~str_replace(player," ",", "),
                           year==2024 ~ word(player,sep=", "),
                           TRUE~player)) %>%
  mutate(player=gsub("\\(.*","",player)) %>% 
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
    (award == "All-NBA" & 
       (str_detect(points,"first")|points %in% c("forward","forward_2","center","guard","guard_2")))~5,
    (award == "All-NBA" & 
       (str_detect(points,"second")|points %in% c("forward_3","forward_4","center_2","guard_3","guard_4")))~3,
    (award == "All-NBA" & 
       (str_detect(points,"third")|points %in% c("forward_5","forward_6","center_3","guard_5","guard_6")))~1)) %>%
  select(-points)

#voter names standardization
all_voting_voter_fix=all_voting_player_fix %>% mutate(voter_name=case_when(
  voter_name=="Sherrod Blakely"~"A. Sherrod Blakely",
  voter_name=="Diego Martinez"~"Diego Martínez Cabrera",
  voter_name=="Israel Gutierez"~"Israel Gutierrez",
  voter_name=="Israel Guttierez"~"Israel Gutierrez",
  voter_name=="John Krawczynski"~"Jon Krawczynski",
  voter_name=="KC Johnson"~"K.C. Johnson",
  voter_name=="Manny Navaro"~"Manny Navarro",
  voter_name=="MIchael Wilbon"~"Michael Wilbon",
  voter_name=="Mike Wilbon"~"Michael Wilbon",
  voter_name=="Michael C Wright"~"Michael C. Wright",
  str_detect(voter_name,"NBA")~"NBA Fan Vote",
  voter_name=="Vote Fan"~"NBA Fan Vote",
  str_detect(voter_name,"Rosalyn")~"Rosalyn Gold-Onwude",
  voter_name=="Vince Goodwill"~"Vincent Goodwill",
  voter_name=="Omari Sankofa"~"Omari Sankofa II",
  !(is.na(voter_name))~voter_name))

finalized_all_voting = all_voting_voter_fix %>%
  mutate(player=case_when(
    #3 players in all-def 2015 have one dash rather than two
    str_detect(player," - IND")~"George Hill",
    str_detect(player," - SA")~"Danny Green",
    str_detect(player," - Mil")~"Giannis Antetokounmpo",
    str_detect(player,"Barea")~"J.J. Barea",
    str_detect(player,"Michael Jr. Porter")~"Michael Porter Jr.",
    str_detect(player,"Jr. Jaren Jackson")~"Jaren Jackson Jr.",
    str_detect(player,"DeMar Derozan")~"DeMar DeRozan",
    str_detect(player,"O.G. Anunoby")~"OG Anunoby",
    str_detect(player,"PJ Tucker")~"P.J. Tucker",
    str_detect(player,"R.J. Barrett")~"RJ Barrett",
    str_detect(player,"TJ McConnell")~"T.J. McConnell",
    str_detect(player,"TJ Warren")~"T.J. Warren",
    TRUE~player)) %>%
  #replace non-ascii dashes
  mutate(player=str_replace(player,"\u2010","-"))

write_csv(finalized_all_voting %>% arrange(desc(year),voter_name,award,desc(points_given)),
          "All NBA Voting Ballots.csv")
