library(janitor)
library(tidyverse)
library(pdftools)

voting_tibble<-function(year=2020,award){
  pdf_name<-paste0("voting pdfs/",year," ",award,".pdf")
  a<-pdf_text(pdf_name) %>% read_lines() %>% str_trim(.) %>% 
    str_split(.,'\\s{2,100}')
  a=tibble(a) %>% unnest_wider(col=a,names_sep="_")
  if (!(award %in% c("All-NBA","All-Defense")) & year > 2017){
    colnames(a)=a[2,]
  }
  else if (award  %in% c("All-NBA","All-Defense") & year %in% 2016:2017){
    colnames(a)=a[4,]
  }
  else if ((award == "All-Defense" & year==2020)|
           (award=="All-Rook" & year %in% 2018:2020)|
           (award %in% c("All-NBA","All-Defense") & year == 2024) #positionless now
           ){
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
  else if (award == "All-Defense" & year < 2024){
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