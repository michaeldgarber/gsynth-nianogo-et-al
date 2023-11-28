
#Read presidential election vote share data from MIT----
## Read and wrangle data--------
#https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/42MVDX
library(here)
setwd(here("data","data-election"))
library(readr)
library(sf)
library(mapview)
pres_votes_state_year=readr::read_csv("1976-2020-president.csv")

party_vote_share=pres_votes_state_year %>% 
  #For some reason, Minnesota is missing in 2000 and 2004?
  #Ah -it's because they're calling the "DEMOCRAT" party
  #the "DEMOCRAT-FARMER-LABOR" party
  #Use party_simplified instead of party_detailed
  mutate(
    party_d_or_r=case_when(
      party_simplified=="DEMOCRAT"~1,
      party_simplified=="REPUBLICAN"~1,
      TRUE ~0),
    
  ) %>%
  arrange(year,state) %>% 
  group_by(year,state,party_d_or_r) %>% 
  mutate(total_votes_d_or_r=sum(candidatevotes,na.rm=T)) %>% 
  ungroup() %>% 
  filter(party_simplified=="DEMOCRAT") %>% 
  #now this has to be dem vote share among total votes for either
  #dem or rep - by definition
  mutate(
    vote_share_dem=candidatevotes/total_votes_d_or_r,
    
    #categorize for easier vis
    vote_share_dem_cat_6=
      case_when(
        vote_share_dem>0.6~6,
        vote_share_dem>0.55~5,
        vote_share_dem>0.5~4,
        vote_share_dem<0.4~1,
        vote_share_dem<0.45~2,
        vote_share_dem<0.5~3),
    
    #4 categories
    vote_share_dem_cat_4=
      case_when(
        vote_share_dem>0.55~4,
        vote_share_dem>0.5~3,
        vote_share_dem<0.45~1,
        vote_share_dem<0.5~2),
    
    
    
    #2 categories
    vote_share_dem_cat_2=case_when(
      vote_share_dem>0.5~1,
      TRUE ~0),
    
    #make each a factor for modeling options
    vote_share_dem_cat_6_fac=as.factor(vote_share_dem_cat_6),
    vote_share_dem_cat_2_fac=as.factor(vote_share_dem_cat_2),
    
    #and a character?
    vote_share_dem_cat_6_char=as.character(vote_share_dem_cat_6),
    vote_share_dem_cat_2_char=as.character(vote_share_dem_cat_2),
    
    #characters and factors don't work with the gsynth model
    #I need to manually create dummys
    #6 vs 1 - meaning most democratic vote share vs least
    vote_share_dem_cat_6_6=case_when(
      vote_share_dem_cat_6==6~1,
      TRUE ~0,
    ),
    vote_share_dem_cat_6_5=case_when(
      vote_share_dem_cat_6==5~1,
      TRUE ~0,
    ),
    vote_share_dem_cat_6_4=case_when(
      vote_share_dem_cat_6==4~1,
      TRUE ~0,
    ),
    vote_share_dem_cat_6_3=case_when(
      vote_share_dem_cat_6==3~1,
      TRUE ~0,
    ),
    vote_share_dem_cat_6_2=case_when(
      vote_share_dem_cat_6==2~1,
      TRUE ~0,
    )  ,
    
    #dummys for 4-cat
    vote_share_dem_cat_4_4=case_when(
      vote_share_dem_cat_4==4~1,
      TRUE ~0,
    ),
    vote_share_dem_cat_4_3=case_when(
      vote_share_dem_cat_4==3~1,
      TRUE ~0,
    ),
    vote_share_dem_cat_4_2=case_when(
      vote_share_dem_cat_4==2~1,
      TRUE ~0,
    )  
      
  ) %>% 
  #exclude write-in ballots as they're causing
  #duplicate entries in Maryland
  filter(writein==FALSE) %>% 
  dplyr::select(year, state, state_po, state_fips, candidatevotes, 
                total_votes_d_or_r,
                party_simplified,
                starts_with("vote_share"))

# Checks------
table(party_vote_share$vote_share_dem_cat_6)
table(party_vote_share$vote_share_dem_cat_6,
      party_vote_share$vote_share_dem_cat_6_6
      )
table(party_vote_share$vote_share_dem_cat_6,
      party_vote_share$vote_share_dem_cat_6_5
)
table(party_vote_share$vote_share_dem_cat_6,
      party_vote_share$vote_share_dem_cat_6_4
)
table(party_vote_share$vote_share_dem_cat_6,
      party_vote_share$vote_share_dem_cat_6_3
)
table(party_vote_share$vote_share_dem_cat_6,
      party_vote_share$vote_share_dem_cat_6_2
)

options(scipen = 999)
# table(party_vote_share$party_simplified)
# table(party_vote_share$party_detailed)
# party_vote_share %>% 
#   filter(state_po=="MN") %>% 
#   View()

#We're getting dupes in Maryland
# party_vote_share %>% 
#   filter(state_fips==24) %>% 
#   View()

library(RColorBrewer)
pal_rd_blue_7=RColorBrewer::brewer.pal(n=7,"RdBu")
pal_rd_blue_6_no_white=pal_rd_blue_7[c(1,2,3,5,6,7)]
library(shades)
pal_rd_blue_6_no_white %>% swatch()

pal_rd_blue_3=RColorBrewer::brewer.pal(n=3,"RdBu")
pal_rd_blue_2_no_white=pal_rd_blue_3[c(1,3)]
pal_rd_blue_2_no_white %>% swatch()

## Map 2020 vote share------
setwd(here("data","data-processed"))
load("lookup_acs_state_geo.RData")
names(party_vote_share)
names(lookup_acs_state_geo)
dem_vote_share_2020=party_vote_share %>% 
  filter(year==2020) %>% 
  left_join(lookup_acs_state_geo, by="state_fips") %>% 
  st_as_sf()
dem_vote_share_2020 %>% 
  dplyr::select(state_fips,starts_with("vote_share_dem")) %>% 
  mapview(zcol="vote_share_dem_cat_6",
          col.regions=pal_rd_blue_6_no_white)

dem_vote_share_2020 %>% 
  filter(vote_share_dem_cat_6_6==1) %>% 
  dplyr::select(state_fips,starts_with("vote_share_dem")) %>% 
  mapview(zcol="vote_share_dem_cat_6",
          col.regions=pal_rd_blue_6_no_white)

dem_vote_share_2020 %>% 
  dplyr::select(state_fips,starts_with("vote_share_dem")) %>% 
  mapview(zcol="vote_share_dem_cat_6_fac",
          col.regions=pal_rd_blue_6_no_white)

dem_vote_share_2020 %>% 
  dplyr::select(state_fips,starts_with("vote_share_dem")) %>% 
  mapview(zcol="vote_share_dem_cat_2",
          layer.name="Vote share",
          col.regions=pal_rd_blue_2_no_white)

dem_vote_share_2020 %>% 
  dplyr::select(state_fips,starts_with("vote_share_dem")) %>% 
  mapview(zcol="vote_share_dem_cat_4",
          layer.name="Vote share")



# Impute non-election years-------
#Repeat rows so that years are continuous from 2000 to 2020 and the information
#in year 2001 is the same as year 2000
party_vote_share_years_filled_in=party_vote_share %>% 
  mutate(n_rows_per_election=4) %>% 
  uncount(n_rows_per_election) %>% 
  #This makes it four rows per year per state
  group_by(year,state) %>% 
  arrange(year) %>% 
  #fill in years between election years
  #This will make it so that the year itself is 0, and we can add
  #1, 2, 3, etc. for the other rows
  rename(year_election=year) %>% #specify that the year is the election year
  mutate(
    add_to_year =row_number()-1,
    year=year_election+add_to_year
  ) %>% 
  ungroup() %>% 
  arrange(state, year)

#This looks good.
#party_vote_share_years_filled_in %>% View()