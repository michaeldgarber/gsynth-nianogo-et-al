#In this script, I load the data provided by Nianogo et al.,
#explore the data, and link in some additional data sources from ACS and elsewhere

#By Michael Garber
#Revised Nov 27, 2023

#Load packages
library(tidyverse)
library(gsynth)
library(here)
library(sf)
library(mapview)

# Read in data prepared by Nianogo et al.--------
#The authors provide their data and code here:
#https://anonymous.4open.science/r/Medicaid-CVD-Disparities-4CB7/README.md

## Read data following their code
overall <- read_rds(here("data","data-nianogo-et-al",
                         "overall.rds"))

black_miss <- read_rds(here("data", "data-nianogo-et-al","black.rds")) 

black_complete <- read_rds(here("data","data-nianogo-et-al", "black.rds")) %>% 
  #removed states with partially missing data (i.e., between 4 and 19 yearly outcome data
  #missing)  #for four states (Alaska, New Mexico, Rhode Island, Utah) and completely
  #missing   #for nine states (Hawaii, Idaho, Maine, Montana, New Hampshire, North Dakota,
  #South Dakota, Vermont, Wyoming. Total removed (n=4 + 9 = 13)
  filter(!(state %in% c('Alaska', 'New Mexico','Rhode Island', 'Utah',
                        'Hawaii', 'South Dakota',
                        'Idaho', 'Maine', 'Montana', 'New Hampshire',
                        'North Dakota', 'Vermont', 'Wyoming')))

hispanic_miss <- read_rds(here("data", "data-nianogo-et-al","hispanic.rds"))

hispanic_complete <- read_rds(here("data",  "data-nianogo-et-al","hispanic.rds")) %>% 
  #removed states with partially missing data 
  #(i.e., between 4 and 19 yearly outcome  data missing) 
  # for twelve (12) states (Alabama, Arkansas, Idaho, Iowa, Kentucky, Louisiana, 
  # Minnesota, Nebraska, Rhode Island, South Carolina, Tennessee, Wisconsin) and 
  #completely missing for 
  # eleven (11) states (Alaska, Delaware, Maine, Mississippi, Montana, 
  #New Hampshire, 
  # North Dakota, South Dakota, Vermont, West Virginia, Wyoming). 
  #Total removed (n=11+12 = 23)
  filter(!state %in% c("Alabama", "Arkansas", "Idaho", "Iowa", "Kentucky", 
                       "Louisiana", "Minnesota", "Nebraska", "Rhode Island",
                       "South Carolina", "Tennessee", "Wisconsin",
                       
                       "Alaska","Delaware","Maine","Mississippi","Montana",
                       "New Hampshire","North Dakota",  "South Dakota",  
                       "Vermont","West Virginia", "Wyoming")) %>% 
  #We imputed below outcome data for the six states with minimal missing 
  #(Kansas, 
  # Maryland, Missouri, North Carolina, Oregon, Utah). 
  #We imputed the outcome data with the closest outcome data 
  #(previous/next year) given 
  # that excluding these six states will have prevented the generalized 
  # synthetic control method from working
  arrange(state, year) %>% 
  dplyr::group_by(state) %>%
  fill(c("cvd_death_rate", "population"), .direction = "downup") %>%
  dplyr::ungroup() 

white <- read_rds(here("data","data-nianogo-et-al","white.rds")) 

men <- read_rds(here("data","data-nianogo-et-al","men.rds")) 

women <- read_rds(here("data","data-nianogo-et-al","women.rds"))

#explore data and variable names
overall
names(overall)

#save some for use in RMarkdown
setwd(here("data","data-processed"))
save(overall,file="overall.RData")
save(black_complete,file="black_complete.RData")
save(hispanic_complete,file="hispanic_complete.RData")
save(white,file="white.RData")

#treatedpost: the treatment variable
#cvd_death_rate: outcome variable
n_distinct(overall$state_id)#50 states
n_distinct(overall$year) #20 years
table(overall$year)
#50 states, each with 20 years= 1,000 rows in the dataset
nrow(overall) #1,000 rows in the data

#are covariates time-varying? that is, within-state
#do the values vary over time?
table(overall$party)
overall %>% 
  ggplot(aes(x=year,y=party))+
  geom_line()+
  facet_grid(rows="state_id")
#indeed time-varying.
#

# Explore data---------
### Create a geometry version to make some maps----

#map them
#geo file created here
#scripts/read-gather-alt-data.R
#also might need this
us_state_fips_url = "https://raw.githubusercontent.com/michaeldgarber/lookups-county-state/main/us-state-fips.csv"
us_state_fips_lookup = us_state_fips_url %>% 
  url() %>% 
  read_csv() %>% 
  mutate(state_fips=as.numeric(state_fips))
table(overall$state_abb)
setwd(here("data","data-processed"))
load("lookup_acs_state_geo.RData")
us_state_fips_lookup
overall_geo=lookup_acs_state_geo %>% 
  left_join(us_state_fips_lookup,by="state_fips") %>% 
  #state_abbrev is the same as state_abb in the Nianogo data
  rename(state_abb=state_abbrev) %>% 
  dplyr::select(-starts_with("state_name")) %>% #avoid merge conflict
  left_join(overall,by="state_abb")

#create a lookup between the geometry and state_abb
lookup_state_abb_geo=overall_geo %>% 
  filter(year==2019) %>% 
  dplyr::select(state_abb, geometry)
lookup_state_abb_geo
save(lookup_state_abb_geo,file="lookup_state_abb_geo.RData")

#a simplified version for mapping
lookup_state_abb_geo_simplify_shift_al_hi=lookup_state_abb_geo %>% 
  tigris::shift_geometry() %>% 
  st_simplify(#simplify the geometry so the map loads faster
    preserveTopology = FALSE,
    dTolerance = 10000)

save(lookup_state_abb_geo_simplify_shift_al_hi,
     file="lookup_state_abb_geo_simplify_shift_al_hi.RData")

names(overall_geo)
hispanic_complete_geo=lookup_acs_state_geo %>% 
  left_join(us_state_fips_lookup,by="state_fips") %>% 
  #state_abbrev is the same as state_abb in the Nianogo data
  rename(state_abb=state_abbrev) %>% 
  dplyr::select(-starts_with("state_name")) %>% #avoid merge conflict
  left_join(hispanic_complete,by="state_abb")

nrow(hispanic_complete_geo)
#some years with missing covariate data..
hispanic_complete_geo %>% 
  filter(year==2018) %>% 
  mapview(zcol="state_fips")
table(us_state_fips_lookup$state_abbrev)
table(black_complete$state_abb)
black_complete_geo=lookup_acs_state_geo %>% 
  left_join(us_state_fips_lookup,by="state_fips") %>% 
  dplyr::select(-starts_with("state_name")) %>% #avoid merge conflict
  #state_abbrev is the same as state_abb in the Nianogo data
  rename(state_abb=state_abbrev) %>% 
  dplyr::select(-starts_with("state_name")) %>% #avoid merge conflict
  left_join(black_complete,by="state_abb")
names(black_complete_geo)
white_geo=lookup_acs_state_geo %>% 
  left_join(us_state_fips_lookup,by="state_fips") %>% 
  dplyr::select(-starts_with("state_name")) %>% #avoid merge conflict
  #state_abbrev is the same as state_abb in the Nianogo data
  rename(state_abb=state_abbrev) %>% 
  dplyr::select(-starts_with("state_name")) %>% #avoid merge conflict
  left_join(white,by="state_abb")

## save these for use in RMarkdown
setwd(here("data","data-processed"))
save(black_complete_geo,file="black_complete_geo.RData")
save(hispanic_complete_geo,file="hispanic_complete_geo.RData")
save(overall_geo,file="overall_geo.RData")


#black_complete_geo %>% View()
#again, missing cov data
black_complete_geo %>% 
  filter(year==2018) %>% 
  mapview(zcol="state_fips")

overall_geo %>% 
  filter(year==2018) %>% 
  mapview(zcol="state_fips")

## Treatment status-------
#and how many ever-treated vs not?-
#Note treated is time-invariant in the dataset, so just pick one year
#34 treated states and 16 untreated
overall %>% 
  filter(year==2019) %>% 
  group_by(treated) %>% 
  summarise(n=n())

#Map treatment status in 2019
overall_geo %>% 
  filter(year==2019) %>% 
  mapview(zcol="treated")

overall_geo %>% 
  filter(year==2014) %>% 
  mutate(treated_char=as.character(treated)) %>% 
  mapview(zcol="treated_char")

hispanic_complete_geo %>% 
  filter(year==2019) %>% 
  mutate(treated_char=as.character(treated)) %>% 
  mapview(zcol="treated_char",
          layer.name="Medicaid expansion state, 2019")

black_complete_geo %>% 
  filter(year==2019) %>% 
  mutate(treated_char=as.character(treated)) %>% 
  mapview(zcol="treated_char",
          layer.name="Medicaid expansion state, 2019")


library(tmap)
hispanic_complete_geo %>% 
  filter(year==2014) %>% 
  tmap::qtm(fill = "treated")

hispanic_complete %>% 
  filter(year==2019) %>% 
  group_by(treated) %>% #just 7 untreated states
  summarise(n=n())

#25 treated states and 12 untreated
black_complete %>% 
  filter(year==2019) %>% 
  group_by(treated) %>% #12 untreated states
  summarise(n=n())



#what was the first year each state was treated?
#useful for an example of effect modification (e.g., immediate vs late adopters)
state_adoption_year=overall %>% 
  dplyr::select(year,state_abb,treatedpost) %>% 
  filter(treatedpost==1) %>% 
  group_by(state_abb) %>% 
  arrange(year) %>% 
  slice(1) %>%  #take just the first year by state
  ungroup() %>% 
  arrange(year) %>% 
  rename(adoption_year=year) %>% 
  dplyr::select(-treatedpost) %>% #so I can link with data
  #make it binary
  mutate(
    adoption_late=case_when(
      adoption_year==2014~0,
      TRUE~1,#all others are late in this subset
    )
  )

table(state_adoption_year$adoption_late) #7 vs 27


#ME and WV were treated in 2019. What if we exclude data from 2019? 
#That would move those two into the control data, which may
#provide better models
state_adoption_year %>% print(n=50)

state_adoption_year %>% 
  group_by(adoption_year) %>% 
  summarise(n=n())

state_adoption_year %>% 
  group_by(adoption_year) %>% 
  summarise(n=n())
overall$treatedpost

## Examine covariate data in subgroups-----
## Number of distinct states in black, hispanic, overall datasets
n_distinct(overall$state_abb)
n_distinct(overall$state_id)
n_distinct(hispanic_complete$state_abb)#27 obs - wow, quite a few missings
n_distinct(black_complete$state_abb)#37 obs
table(overall$year)








### population------
summary(overall$population)
summary(hispanic_complete$population)
summary(black_complete$population)
#Note some strange covariate data in hispanic subgroup, e.g.,
#0% male in Arizona, 75% male in Colorado
#30% employed for wages.
#0% low income in several states
names(overall)
overall %>% 
  arrange(year,state_id)

hispanic_complete %>% 
  arrange(year,state_id)


### low income----
overall %>% 
  ggplot(aes(x=low_income))+
  geom_histogram()
summary(overall$low_income)
hispanic_complete %>% 
  ggplot(aes(x=low_income))+
  geom_histogram()#note the 1s and 0s; not plausible at state level
summary(hispanic_complete$low_income)
black_complete %>% 
  ggplot(aes(x=low_income))+
  geom_histogram()
summary(black_complete$low_income)

black_complete_geo %>% 
  filter(year==2018) %>% 
  mapview(zcol="low_income")
overall_geo %>% 
  filter(year==2018) %>% 
  mapview(zcol="low_income")

#these are better - more plausible
men %>% 
  ggplot(aes(x=low_income))+
  geom_histogram()
women %>% 
  ggplot(aes(x=low_income))+
  geom_histogram()



### male----
#proportion male is a little low...maybe just survey response?
overall %>% 
  ggplot(aes(x=male))+
  geom_histogram()
overall %>% 
  filter(state_abb=="CA") %>% 
  ggplot(aes(y=male,x=year))+
  geom_line()# a little strange but perhaps trend is correct
overall %>% 
  filter(state_abb=="NY") %>% 
  ggplot(aes(y=male,x=year))+
  geom_line()
hispanic_complete %>% 
  ggplot(aes(x=male))+
  geom_histogram()
summary(hispanic_complete$male)
hispanic_complete %>% 
  filter(state_abb=="CA") %>% 
  ggplot(aes(y=male,x=year))+
  geom_line()#less than 20% male in CA, hispanic, in 2000...

summary(hispanic_complete$male)
black_complete %>% 
  ggplot(aes(x=male))+
  geom_histogram()


table(black_complete$state_abb)
black_complete %>% 
  filter(state_abb=="CA") %>% 
  ggplot(aes(y=male,x=year))+
  geom_line()

### party-------
#according to Table 1 in paper, this is a 3-level variable
#for Republican, Democrat, split.

#What did it look like in 2016?
# NY and CA are different, suggesting maybe not the 
#best indicator of politics in state
overall_geo %>% 
  filter(year==2016) %>% 
  mapview(zcol="party")

overall_geo %>% 
  filter(year==2004) %>% 
  mapview(zcol="party")


table(overall$party)
overall %>% 
  ggplot(aes(x=party))+
  geom_histogram()

#look at "party" in Texas
overall %>% 
  filter(state_abb=="TX") %>% 
  ggplot(aes(x=year,y=party))+
  geom_line()

#Georgia?
overall %>% 
  filter(state_abb=="GA") %>% 
  ggplot(aes(x=year,y=party))+
  geom_line()

#California?
overall %>% 
  filter(state_abb=="CA") %>% 
  ggplot(aes(x=year,y=party))+
  geom_line()

overall %>% 
  group_by(year) %>% 
  arrange(state_abb) %>% 
  slice(1:10) %>% 
  ungroup() %>% 
  ggplot(aes(y=party,x=year))+
  geom_line()+
  facet_grid(rows="state_abb")



hispanic_complete %>% 
  ggplot(aes(x=party))+
  geom_histogram()

black_complete %>% 
  ggplot(aes(x=party))+
  geom_histogram()

### employed_for_wages-------
#These look better, broadly, although there are some 1s which is not plausible
#at the state level
overall %>% 
  ggplot(aes(x=employed_for_wages))+
  geom_histogram()

white %>% 
  ggplot(aes(x=employed_for_wages))+
  geom_histogram()# note 0s and 1s; not plausible.

hispanic_complete %>% 
  ggplot(aes(x=employed_for_wages))+
  geom_histogram()# note 0s and 1s; not plausible.

black_complete %>% 
  ggplot(aes(x=employed_for_wages))+
  geom_histogram()#note 1s; not plausible
black_complete_geo %>% 
  filter(employed_for_wages>0.8) %>% 
  mapview(zcol="employed_for_wages")

### married-------
summary(overall$married)
summary(hispanic_complete$married)#again, not plausible to have 1s or 0s
hispanic_complete %>% 
  filter(married<.2) %>% 
  View()
dplyr::select(married) %>% 
  View()
summary(black_complete$married)#again, not plausible to have 1s or 0s

hispanic_complete %>% 
  ggplot(aes(x=married))+
  geom_histogram() 
hispanic_complete_geo %>% 
  mapview(zcol="married")
black_complete %>% 
  ggplot(aes(x=married))+
  geom_histogram() 
black_complete_geo %>% 
  filter(year==2004) %>% 
  mapview(zcol="married")

white %>% 
  ggplot(aes(x=married))+
  geom_histogram() 



#where are the extremes?
black_complete_geo %>% 
  filter(married<0.1) %>% 
  mapview(zcol="married")

white_geo %>% 
  filter(married<0.1) %>% 
  mapview(zcol="married")

men %>% 
  ggplot(aes(x=married))+
  geom_histogram() 

women %>% 
  ggplot(aes(x=married))+
  geom_histogram() 



### education--------
summary(overall$low_educ)
summary(hispanic_complete$low_educ)
hispanic_complete %>% 
  ggplot(aes(x=low_educ))+
  geom_histogram()#several zeros
summary(black_complete$low_educ)
black_complete %>% 
  ggplot(aes(x=low_educ))+
  geom_histogram()

#looks more reasonable
overall_geo %>% 
  filter(year==2019) %>% 
  mapview(zcol="married")

#black_complete %>%  View()

## Examine outcome data in treated vs controls-----
#In the original synthetic control method, outcome data must be within
#the "convex hull" of that of the controls. This is not required
#for the generalized synthetic control method, but it still may be informative
#to check on the trends of the outcome in the treated vs control states

#Find the mean, min, and max of treated and controls over time
outcome_by_treated_over_time=overall %>% 
  #note this treatment variable is constant within state
  group_by(treated,year) %>% 
  summarise(
    cvd_death_rate_min=min(cvd_death_rate,na.rm=TRUE),
    cvd_death_rate_max=max(cvd_death_rate,na.rm=TRUE),
    cvd_death_rate_mean=mean(cvd_death_rate,na.rm=TRUE)
  ) %>% 
  ungroup() %>% 
  mutate(
    treated_char=as.character(treated),
    treated_fac=fct_relevel(#reorder so dashed is untreated
      as.factor(treated_char),"1","0")
  ) %>% 
  #make it longer so I can plot mean, min, max for each on same plot
  pivot_longer(
    cols=starts_with("cvd"),
  ) %>% 
  mutate(stat=substr(name,16,20)) %>% 
  rename(cvd_death_rate=value)

#Plot outcome over time in treated vs control states
outcome_by_treated_over_time %>% 
  ggplot(aes(x=year,y=cvd_death_rate))+
  geom_line(aes(linetype=treated_fac,color=stat),linewidth=1.5)+
  theme_bw()+
  scale_linetype_discrete(
    name="Ever treated",
    labels=c("Yes","No")
  )+
  labs(
    y="CVD deaths per 100k, ages 45-64",
    x="Year"
  )+
  scale_y_continuous(
    limits=c(0,max(outcome_by_treated_over_time$cvd_death_rate))
  )

### Outcome in men vs women-------
summary(men$cvd_death_rate)
summary(women$cvd_death_rate)

#The mean outcome in treated is indeed within bounds of
#untreated, but it's worth noting that on avereage

#Plot the mean treated value and bounds of untreated
# Create dataset of just the covariates for the overall group----
#, i.e., everything but the outcome
#keep the treated and treatedpost vars in this one
#Add covariates gathered from elsewhere in this script:
#~gsynth-medicaid-nianogo-et-al/scripts/read-gather-alt-data.R
names(overall)

table(overall$state_abb)
names(overall)
#Load election vote share data and acs data
setwd(here("data","data-processed"))
load("acs_1_year_by_state_2000_2019.RData")
source(here("scripts", "read-wrangle-data-pres-election.R"))
#stroke belt data
source(here("scripts","stroke-belt.R"))
covars_alt=overall %>% 
  dplyr::select(-cvd_death_rate, 
                -state #duplicate with state_name
  ) %>% 
  mutate(
    state_po=state_abb
  ) %>% 
  rename(
    #emphasize that these covariates are in the "overall" group
    low_educ_overall=low_educ,
    employed_for_wages_overall=employed_for_wages,
    low_income_overall=low_income,
    married_overall=married,
    male_overall=male,
    race_nonwhite_overall=race_nonwhite,
    population_overall=population,
    party_overall=party
  ) %>% 
  #Link in the presidential-election voting data
  #Note that data corresponds to most recent election.
  #For example, 2018 data corresponds to 2016 election.
  left_join(party_vote_share_years_filled_in,
            by=c("year","state_po")) %>% 
  #Link in 1-year ACS data
  left_join(acs_1_year_by_state_2000_2019, by=c("year","state_fips")) %>% 
  dplyr::select(year,starts_with("state"),everything())

#covars_alt %>% View()
names(covars_alt)
nrow(covars_alt)

#I need a lookup for state_id and state_abb
lookup_state_id_state_abb=covars_alt %>% 
  distinct(state_id,state_abb)

#Save this to make maps in RMarkdown
setwd(here("data","data-processed"))
save(lookup_state_id_state_abb,file="lookup_state_id_state_abb.RData")
lookup_state_id_state_abb
#lookup_state_id that includes state_fips

#how do covars compare?
covars_alt %>% 
  filter(year==2019) %>% 
  ggplot(aes(x=prop_race_non_w,y=race_nonwhite_overall))+
  geom_point()

## Look-up tables for non-missing states in demographic groups----
names(hispanic_complete)
hispanic_miss
lookup_hispanic_miss_state_year=hispanic_complete %>% 
  dplyr::select(state_id, year) %>% 
  mutate(hispanic_not_miss=1)

lookup_hispanic_miss_state_year

lookup_black_miss_state_year=black_complete %>% 
  dplyr::select(state_id, year) %>% 
  mutate(black_not_miss=1)

lookup_black_miss_state_year

lookup_white_miss_state_year=white %>% 
  dplyr::select(state_id, year) %>% 
  mutate(white_not_miss=1)

lookup_white_miss_state_year

## Calculate state-year population proportions----
#For possible weighting of effect estimates
names(covars_alt)

## Pop. weights for each state by year--------
### Weights for each state by treated year-----
#### Overall-----
#Proportion of population in that state in that year among
#treated states
#Since the effects are only among the treated, can restrict to
#those states

#Quick check of distribution of pop in 2019
#among treated states
covars_alt %>% 
  filter(year==2019) %>% 
  filter(treated==1) %>% 
  ggplot(aes(x=pop_bot_45_64))+
  geom_histogram()

covars_alt %>% 
  filter(year==2019) %>% 
  filter(treated==1) %>% 
  dplyr::select(state_abb,pop_bot_45_64) %>% 
  arrange(pop_bot_45_64) %>% 
  print(n=100)




names(covars_alt)
state_wts_by_treated_year_overall =covars_alt %>% 
  filter(treatedpost==1) %>% 
  group_by(year,treatedpost) %>% 
  #note this group-by syntax without a mutate. The sum is still over
  #the states, but it doesn't collapse the data
  mutate(
    #pop in 45-64 age group in each demographic group.
    #Note these weights to be long-form (i.e., same var name across
    #demographic groups) so that they
    #work in the functions later, so don't put "overall" in var. name
    pop_tot_over_states=sum(pop_bot_45_64,na.rm=T)
  ) %>% 
  ungroup() %>% 
  mutate(
    prop_pop_by_state=pop_bot_45_64/pop_tot_over_states
  ) %>% 
  #only keep state_id b/c that's what's kept in the gsynth models
  dplyr::select(state_id,year,
                #treatedpost,#lose this; it's in the definition above
                starts_with("prop_pop_by_state"),
                starts_with("pop_tot_over_states"))

state_wts_by_treated_year_overall %>% 
     left_join(lookup_state_id_state_abb,by="state_id") %>% 
    dplyr::select(state_abb,everything()) #%>%      View()



#Grab the men and women weights as well
#### men-------
state_wts_by_treated_year_men =covars_alt %>% 
  filter(treatedpost==1) %>% 
  group_by(year,treatedpost) %>% 
  mutate(
    pop_tot_over_states=sum(pop_men_45_64,na.rm=T)
  ) %>% 
  ungroup() %>% 
  mutate(
    prop_pop_by_state=pop_men_45_64/pop_tot_over_states
  ) %>% 
  #only keep state_id b/c that's what's kept in the gsynth models
  dplyr::select(state_id,year,
                starts_with("prop_pop_by_state"),
                starts_with("pop_tot_over_states"))

state_wts_by_treated_year_men %>% 
  left_join(lookup_state_id_state_abb,by="state_id") %>% 
  dplyr::select(state_abb,everything()) %>% 
  View()

#### women--------
state_wts_by_treated_year_wom =covars_alt %>% 
  filter(treatedpost==1) %>% 
  group_by(year,treatedpost) %>% 
  mutate(
    pop_tot_over_states=sum(pop_wom_45_64,na.rm=T)
  ) %>% 
  ungroup() %>% 
  mutate(
    prop_pop_by_state=pop_wom_45_64/pop_tot_over_states
  ) %>% 
  #only keep state_id b/c that's what's kept in the gsynth models
  dplyr::select(state_id,year,
                starts_with("prop_pop_by_state"),
                starts_with("pop_tot_over_states"))

state_wts_by_treated_year_wom %>% 
  left_join(lookup_state_id_state_abb,by="state_id") %>% 
  dplyr::select(state_abb,everything()) %>% 
  View()




#### Black-----
#among the non-missing states

state_wts_by_treated_year_race_b =covars_alt %>% 
  left_join(lookup_black_miss_state_year,by=c("state_id","year")) %>% 
  filter(treatedpost==1) %>% 
  filter(black_not_miss==1) %>% 
  group_by(year,treatedpost) %>% 
  mutate(
    pop_tot_over_states=sum(pop_bot_45_64_race_b,na.rm=T)  ) %>% 
  ungroup() %>% 
  mutate(
    prop_pop_by_state=pop_bot_45_64_race_b/pop_tot_over_states
  ) %>% 
  dplyr::select(state_id,year,
                starts_with("prop_pop_by_state"),
                starts_with("pop_tot_over_states"))
# state_wts_by_treated_year_race_b %>% 
#   left_join(lookup_state_id_state_abb,by="state_id") %>% 
#   dplyr::select(state_abb,everything()) %>% 
#   View()

#### Hispanic-----
#among the non-missing states

state_wts_by_treated_year_race_h =covars_alt %>% 
  left_join(lookup_hispanic_miss_state_year,by=c("state_id","year")) %>% 
  filter(treatedpost==1) %>% 
  filter(hispanic_not_miss==1) %>% 
  group_by(year,treatedpost) %>% 
  mutate(
    pop_tot_over_states=sum(pop_bot_45_64_race_h,na.rm=T)  ) %>% 
  ungroup() %>% 
  mutate(
    prop_pop_by_state=pop_bot_45_64_race_h/pop_tot_over_states
  ) %>% 
  dplyr::select(state_id,year,
                starts_with("prop_pop_by_state"),
                starts_with("pop_tot_over_states"))
state_wts_by_treated_year_race_h %>% 
  left_join(lookup_state_id_state_abb,by="state_id") %>% 
  dplyr::select(state_abb,everything()) %>% 
  View()

#### white-----
#among the non-missing states
state_wts_by_treated_year_race_w =covars_alt %>% 
  left_join(lookup_white_miss_state_year,by=c("state_id","year")) %>% 
  filter(treatedpost==1) %>% 
  filter(white_not_miss==1) %>% 
  group_by(year,treatedpost) %>% 
  mutate(
    pop_tot_over_states=sum(pop_bot_45_64_race_w,na.rm=T)  ) %>% 
  ungroup() %>% 
  mutate(
    prop_pop_by_state=pop_bot_45_64_race_w/pop_tot_over_states
  ) %>% 
  dplyr::select(state_id,year,
                starts_with("prop_pop_by_state"),
                starts_with("pop_tot_over_states"))
state_wts_by_treated_year_race_w %>% 
  left_join(lookup_state_id_state_abb,by="state_id") %>% 
  dplyr::select(state_abb,everything()) %>% 
  View()

### Weights for each state-year----
#This is distinct from above because each state-year over the whole
#treated dataset receives a weight with respect to the total population-year
#total
#This would be more appropriate for the overall summary

#### overall-------
state_year_wts_overall =covars_alt %>% 
  filter(treatedpost==1) %>% 
  group_by(treatedpost) %>% 
  mutate(
    #sum over all treated years; the sum is akin to a person-time measure
    pop_tot_all_treated_years=sum(pop_bot_45_64,na.rm=T)  
  ) %>% 
  ungroup() %>% 
  mutate(
    prop_of_tot_pop_year=pop_bot_45_64/pop_tot_all_treated_years
  ) %>% 
  #only keep state_id b/c that's what's kept in the gsynth models
  dplyr::select(state_id,year,
                starts_with("pop_tot_all"),
                starts_with("prop_of_tot_pop_year"))

#Jan 3, 2024
#Saving this to use in RMarkdown doc
setwd(here("data","data-processed"))
save(state_year_wts_overall,file="state_year_wts_overall.RData")

state_year_wts_overall %>% 
  left_join(lookup_state_id_state_abb,by="state_id") %>% 
  dplyr::select(state_abb,everything()) %>% 
  View()

pop_bot_45_64_treated_states_for_join=covars_alt %>% 
#  filter(state_id==6) %>%
  filter(treatedpost==1) %>% 
  dplyr::select(year, state_id,pop_bot_45_64)

pop_bot_45_64_treated_states_for_join

state_year_wts_overall %>% 
  left_join(lookup_state_id_state_abb,by="state_id") %>% 
  filter(state_abb=="CA") %>% 
  left_join(pop_bot_45_64_treated_states_for_join,by=c("year","state_id"))

#person years per year
n_treated_years=state_year_wts_overall %>% 
  filter(state_id==6) %>% 
  group_by(state_id) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  dplyr::select(n) %>% pull()

n_treated_years
pop_tot_all_treated_person_years=state_year_wts_overall %>% 
  filter(year==2014) %>% 
  filter(state_id==6) %>% 
  dplyr::select(pop_tot_all_treated_years) %>% 
  pull()

pop_tot_all_treated_person_years/n_treated_years

#California's total share contributed to the person-time.
#Can simply sum up the proportion by treated-year
state_year_wts_overall %>% 
  left_join(lookup_state_id_state_abb,by="state_id") %>% 
#  filter(state_abb=="CA") %>% 
  left_join(pop_bot_45_64_treated_states_for_join,by=c("year","state_id")) %>% 
  group_by(state_abb) %>% 
  summarise(
    prop_of_tot_pop_year=sum(prop_of_tot_pop_year,na.rm=T),
    
    ) %>% 
  View()

#### men------
state_year_wts_men =covars_alt %>% 
  filter(treatedpost==1) %>% 
  group_by(treatedpost) %>% 
  mutate(
    #sum over all treated years; the sum is akin to a person-time measure
    pop_tot_all_treated_years=sum(pop_men_45_64,na.rm=T)  
  ) %>% 
  ungroup() %>% 
  mutate(
    prop_of_tot_pop_year=pop_men_45_64/pop_tot_all_treated_years
  ) %>% 
  #only keep state_id b/c that's what's kept in the gsynth models
  dplyr::select(state_id,year,
                starts_with("pop_tot_all"),
                starts_with("prop_of_tot_pop_year"))

state_year_wts_men %>% 
  left_join(lookup_state_id_state_abb,by="state_id") %>% 
  dplyr::select(state_abb,everything()) %>% 
  View()

#### women------
state_year_wts_wom =covars_alt %>% 
  filter(treatedpost==1) %>% 
  group_by(treatedpost) %>% 
  mutate(
    #sum over all treated years; the sum is akin to a person-time measure
    pop_tot_all_treated_years=sum(pop_wom_45_64,na.rm=T)  
  ) %>% 
  ungroup() %>% 
  mutate(
    prop_of_tot_pop_year=pop_wom_45_64/pop_tot_all_treated_years
  ) %>% 
  #only keep state_id b/c that's what's kept in the gsynth models
  dplyr::select(state_id,year,
                starts_with("pop_tot_all"),
                starts_with("prop_of_tot_pop_year"))

# state_year_wts_wom %>% 
#   left_join(lookup_state_id_state_abb,by="state_id") %>% 
#   dplyr::select(state_abb,everything()) %>% 
#   View()


#### black------
state_year_wts_race_b =covars_alt %>% 
  filter(treatedpost==1) %>% 
  group_by(treatedpost) %>% 
  mutate(
    #sum over all treated years; the sum is akin to a person-time measure
    pop_tot_all_treated_years=sum(pop_bot_45_64_race_b,na.rm=T)  
  ) %>% 
  ungroup() %>% 
  mutate(
    prop_of_tot_pop_year=pop_bot_45_64_race_b/pop_tot_all_treated_years
  ) %>% 
  #only keep state_id b/c that's what's kept in the gsynth models
  dplyr::select(state_id,year,
                starts_with("pop_tot_all"),
                starts_with("prop_of_tot_pop_year"))

#save for use in Rmarkdown
setwd(here("data","data-processed"))
save(state_year_wts_race_b,file="state_year_wts_race_b.RData")

state_year_wts_race_b %>%
  left_join(lookup_state_id_state_abb,by="state_id") %>%
  dplyr::select(state_abb,everything()) %>%
  View()

#total over all years
state_year_wts_race_b %>%
  left_join(lookup_state_id_state_abb,by="state_id") %>%
  dplyr::select(state_abb,everything()) %>%
  group_by(state_abb) %>% 
  summarise(prop_of_tot_pop_year=sum(prop_of_tot_pop_year,na.rm=T)) %>% 
  View()



#### hispanic------
state_year_wts_race_h =covars_alt %>% 
  filter(treatedpost==1) %>% 
  group_by(treatedpost) %>% 
  mutate(
    #sum over all treated years; the sum is akin to a person-time measure
    pop_tot_all_treated_years=sum(pop_bot_45_64_race_h,na.rm=T)  
  ) %>% 
  ungroup() %>% 
  mutate(
    prop_of_tot_pop_year=pop_bot_45_64_race_h/pop_tot_all_treated_years
  ) %>% 
  #only keep state_id b/c that's what's kept in the gsynth models
  dplyr::select(state_id,year,
                starts_with("pop_tot_all"),
                starts_with("prop_of_tot_pop_year"))
# state_year_wts_race_h %>% 
#   left_join(lookup_state_id_state_abb,by="state_id") %>% 
#   dplyr::select(state_abb,everything()) %>% 
#   View()

#### white------
state_year_wts_race_w =covars_alt %>% 
  filter(treatedpost==1) %>% 
  group_by(treatedpost) %>% 
  mutate(
    #sum over all treated years; the sum is akin to a person-time measure
    pop_tot_all_treated_years=sum(pop_bot_45_64_race_w,na.rm=T)  
  ) %>% 
  ungroup() %>% 
  mutate(
    prop_of_tot_pop_year=pop_bot_45_64_race_w/pop_tot_all_treated_years
  ) %>% 
  #only keep state_id b/c that's what's kept in the gsynth models
  dplyr::select(state_id,year,
                starts_with("pop_tot_all"),
                starts_with("prop_of_tot_pop_year"))
# state_year_wts_race_w %>% 
#   left_join(lookup_state_id_state_abb,by="state_id") %>% 
#   dplyr::select(state_abb,everything()) %>% 
#   View()

# Add alternative covariates to data--------

### Overall-----
#Add alternative covariates to overall dta
state_adoption_year

overall_covars_alt=overall %>% 
  dplyr::select(year,state_id,cvd_death_rate) %>% 
  left_join(covars_alt,by=c("state_id","year")) %>% 
  left_join(state_adoption_year,by="state_abb") %>% 
  left_join(lookup_stroke_belt_states_nogeo,by="state_fips") %>% 
  dplyr::select(year,starts_with("state"),everything()) %>% 
  #add an indicator for whether state-year data are missing
  #for hispanic, black, and white populations for mapping
  #and assessment of potential selection bias
  left_join(lookup_hispanic_miss_state_year,by=c("state_id","year")) %>% 
  left_join(lookup_black_miss_state_year,by=c("state_id","year")) %>%
  left_join(lookup_white_miss_state_year,by=c("state_id","year")) %>% 
  mutate(
    hispanic_miss=case_when(
      hispanic_not_miss==1~0,
      TRUE~1),
    
    #a variable for mapping tx status by missingness
    treated_post_hispanic_miss=case_when(
      treatedpost==1 & hispanic_miss==1 ~ "Yes, outcome data missing in Hispanic pop.",
      treatedpost==1 & hispanic_miss==0 ~ "Yes, outcome data available in Hispanic pop.",
      treatedpost==0 & hispanic_miss==1 ~ "No, outcome data missing in Hispanic pop.",
      treatedpost==0 & hispanic_miss==0 ~ "No, outcome data available in Hispanic pop."),

    black_miss=case_when(
      black_not_miss==1~0,
      TRUE~1),
      
    #a variable for mapping tx status by missingness
    treated_post_black_miss=case_when(
      treatedpost==1 & black_miss==1 ~ "Yes, outcome data missing in Black pop.",
      treatedpost==1 & black_miss==0 ~ "Yes, outcome data available in Black pop.",
      treatedpost==0 & black_miss==1 ~ "No, outcome data missing in Black pop.",
      treatedpost==0 & black_miss==0 ~ "No, outcome data available in Black pop."),
          
    white_miss=case_when(
      white_not_miss==1~0,
      TRUE~1),
    
    #a variable for mapping tx status by missingness
    treated_post_white_miss=case_when(
      treatedpost==1 & white_miss==1 ~ "Yes, white data missing",
      treatedpost==1 & white_miss==0 ~ "Yes, white data available",
      treatedpost==0 & white_miss==1 ~ "No, white data missing",
      treatedpost==0 & white_miss==0 ~ "No, white data available")
        
    )

#Save this for RMarkdown
save(overall_covars_alt,file="overall_covars_alt.RData")
#assess missingness variables
overall_covars_alt %>% 
  filter(year==2019) %>% 
  group_by(hispanic_miss) %>% 
  summarise(n=n())

  
overall_covars_alt %>% 
  filter(year==2019) %>% 
  group_by(treated_post_hispanic_miss) %>% 
  summarise(n=n())

overall_covars_alt %>% 
  filter(year==2019) %>% 
  group_by(treated_post_black_miss) %>% 
  summarise(n=n())

overall_covars_alt %>% 
  filter(year==2019) %>% 
  group_by(treatedpost, hispanic_miss) %>% 
  summarise(n=n())


overall_covars_alt
names(overall_covars_alt)
#are there dupes?
overall_covars_alt %>% 
  group_by(state_id,year) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  filter(n>1)

### Hispanic--------
#Because of the instability in the covariates in the hispanic group,
#try a version of the data where the covariates correspond to the overall state level
#rather than the hispanic subgroup

mutate_trim_covariates=function(df){
  df %>% 
    mutate(
      #trim covariates at 3rd and 97th percentile to move implausible 1s and 0s to a
      #more plausible range
      low_educ_trim=case_when(
        low_educ>quantile(low_educ, probs = .97)~quantile(low_educ, probs = .97),
        low_educ<quantile(low_educ, probs = .03)~quantile(low_educ, probs = .03),
        TRUE ~ low_educ
      ),
      employed_for_wages_trim=case_when(
        employed_for_wages>quantile(employed_for_wages, probs = .97)~quantile(employed_for_wages, probs = .97),
        employed_for_wages<quantile(employed_for_wages, probs = .03)~quantile(employed_for_wages, probs = .03),
        TRUE ~ employed_for_wages
      ),
      low_income_trim=case_when(
        low_income>quantile(low_income, probs = .97)~quantile(low_income, probs = .97),
        low_income<quantile(low_income, probs = .03)~quantile(low_income, probs = .03),
        TRUE ~ low_income
      ),
      married_trim=case_when(
        married>quantile(married, probs = .97)~quantile(married, probs = .97),
        married<quantile(married, probs = .03)~quantile(married, probs = .03),
        TRUE ~ married
      )
    )
  
}
hispanic_complete_covars_alt=hispanic_complete %>% 
  mutate_trim_covariates() %>% 
  rename(
    #trimmed
    low_educ_trim_hispanic=low_educ_trim,
    employed_for_wages_trim_hispanic=employed_for_wages_trim,
    low_income_trim_hispanic=low_income_trim,
    married_trim_hispanic=married_trim,
    male_hispanic=male,
    
    #original - untrimmed
    low_educ_hispanic=low_educ,
    employed_for_wages_hispanic=employed_for_wages,
    low_income_hispanic=low_income,
    married_hispanic=married,
    male_hispanic=male,
    population_hispanic=population,
    party_hispanic=party
  ) %>% 
  dplyr::select(
    year,state_id,cvd_death_rate,
    ends_with("hispanic")
  ) %>% 
  left_join(covars_alt,by=c("state_id","year")) %>% 
  left_join(state_adoption_year,by="state_abb") %>% 
  left_join(lookup_stroke_belt_states_nogeo,by="state_fips") %>% 
  dplyr::select(year,starts_with("state"),everything())

#check on trimming
summary(hispanic_complete_covars_alt$low_educ_trim_hispanic)
summary(hispanic_complete_covars_alt$low_educ_hispanic)
hispanic_complete_covars_alt %>% 
  ggplot(aes(x=low_educ_hispanic,y=low_educ_trim_hispanic))+
  geom_point()

hispanic_complete_covars_alt %>% 
  ggplot(aes(x=low_income_hispanic,y=low_income_trim_hispanic))+
  geom_point()

hispanic_complete_covars_alt %>% 
  ggplot(aes(x=married_hispanic,y=married_trim_hispanic))+
  geom_point()
hispanic_complete_covars_alt %>% 
  ggplot(aes(x=employed_for_wages_hispanic,y=employed_for_wages_trim_hispanic))+
  geom_point()



## Black-----
black_complete_covars_alt=black_complete %>% 
  mutate_trim_covariates() %>% 
  rename(
    #trimmed
    low_educ_trim_black=low_educ_trim,
    employed_for_wages_trim_black=employed_for_wages_trim,
    low_income_trim_black=low_income_trim,
    married_trim_black=married_trim,
    male_black=male,
    
    #original - untrimmed
    low_educ_black=low_educ,
    employed_for_wages_black=employed_for_wages,
    low_income_black=low_income,
    married_black=married,
    male_black=male,
    population_black=population,
    party_black=party
  ) %>% 
  dplyr::select(
    year,state_id,cvd_death_rate,
    ends_with("black")
  ) %>% 
  left_join(covars_alt,by=c("state_id","year")) %>% 
  left_join(state_adoption_year,by="state_abb") %>% 
  left_join(lookup_stroke_belt_states_nogeo,by="state_fips") %>% 
  dplyr::select(year,starts_with("state"),everything())

black_complete_covars_alt
names(black_complete_covars_alt)

#check on trimming
summary(black_complete_covars_alt$low_educ_trim_black)
summary(black_complete_covars_alt$low_educ_black)
black_complete_covars_alt %>% 
  ggplot(aes(x=low_educ_black,y=low_educ_trim_black))+
  geom_point()#a sensitivity/specifity tradeoff here. worth it to exclude the outlier.

black_complete_covars_alt %>% 
  ggplot(aes(x=low_income_black,y=low_income_trim_black))+
  geom_point()

black_complete_covars_alt %>% 
  ggplot(aes(x=married_black,y=married_trim_black))+
  geom_point()
black_complete_covars_alt %>% 
  ggplot(aes(x=employed_for_wages_black,y=employed_for_wages_trim_black))+
  geom_point()

## White----
white_covars_alt=white %>% 
  mutate_trim_covariates() %>% 
  rename(
    #trimmed
    low_educ_trim_white=low_educ_trim,
    employed_for_wages_trim_white=employed_for_wages_trim,
    low_income_trim_white=low_income_trim,
    married_trim_white=married_trim,
    male_white=male,
    
    #original - untrimmed
    low_educ_white=low_educ,
    employed_for_wages_white=employed_for_wages,
    low_income_white=low_income,
    married_white=married,
    male_white=male,
    population_white=population,
    party_white=party
  ) %>% 
  dplyr::select(
    year,state_id,cvd_death_rate,
    ends_with("white")
  ) %>% 
  left_join(covars_alt,by=c("state_id","year")) %>% 
  left_join(state_adoption_year,by="state_abb") %>% 
  left_join(lookup_stroke_belt_states_nogeo,by="state_fips") %>% 
  dplyr::select(year,starts_with("state"),everything())

## Men----
men_covars_alt=men %>% 
  rename(
    low_educ_men=low_educ,
    employed_for_wages_men=employed_for_wages,
    low_income_men=low_income,
    married_men=married,
    male_men=male,
    race_nonwhite_men=race_nonwhite,
    population_men=population,
    party_men=party
  ) %>% 
  dplyr::select(
    year,state_id,cvd_death_rate,
    ends_with("men")
  ) %>% 
  left_join(covars_alt,by=c("state_id","year")) %>% 
  left_join(state_adoption_year,by="state_abb") %>% 
  left_join(lookup_stroke_belt_states_nogeo,by="state_fips") %>% 
  dplyr::select(year,starts_with("state"),everything())

## Women----
women_covars_alt=women %>% 
  rename(
    low_educ_women=low_educ,
    employed_for_wages_women=employed_for_wages,
    low_income_women=low_income,
    married_women=married,
    male_women=male,
    race_nonwhite_women=race_nonwhite,
    population_women=population,
    party_women=party
  ) %>% 
  dplyr::select(
    year,state_id,cvd_death_rate,
    ends_with("women")
  ) %>% 
  left_join(covars_alt,by=c("state_id","year")) %>% 
  left_join(state_adoption_year,by="state_abb") %>% 
  left_join(lookup_stroke_belt_states_nogeo,by="state_fips") %>% 
  dplyr::select(year,starts_with("state"),everything())


# Datasets without 2019------
overall_covars_alt_no_2019=overall_covars_alt %>% 
  filter(year<2019)
nrow(overall_covars_alt_no_2019)
nrow(overall_covars_alt)

black_complete_covars_alt_no_2019=black_complete_covars_alt %>% 
  filter(year<2019)
nrow(black_complete_covars_alt_no_2019)
nrow(black_complete_covars_alt)

white_covars_alt_no_2019=white_covars_alt %>% 
  filter(year<2019)
nrow(white_covars_alt_no_2019)
nrow(white_covars_alt)

hispanic_complete_covars_alt_no_2019=hispanic_complete_covars_alt %>% 
  filter(year<2019)
nrow(hispanic_complete_covars_alt_no_2019)
nrow(hispanic_complete_covars_alt)

men_covars_alt_no_2019=men_covars_alt %>% 
  filter(year<2019)
nrow(men_covars_alt_no_2019)
nrow(men_covars_alt)

women_covars_alt_no_2019=women_covars_alt %>% 
  filter(year<2019)
nrow(women_covars_alt_no_2019)
nrow(women_covars_alt)

# Compare covariates------
#Is it worth using these different ones? 
#Perhaps not - they look to be just about the exact same. BRFSS might use
#ACS data to get its population estimates.
## Population in racial groups------
overall_covars_alt %>% 
  filter(year==2019) %>% 
  ggplot(aes(x=population_overall,y=pop_bot_45_64))+
  geom_point()#identical. nice

black_complete_covars_alt %>% 
  filter(year==2019) %>% 
  ggplot(aes(x=population_black,y=pop_bot_45_64_race_b))+
  geom_point()


white_covars_alt %>% 
  filter(year==2019) %>% 
  ggplot(aes(x=population_white,y=pop_bot_45_64_race_w))+
  geom_point()

#proportion non-white
summary(overall_covars_alt$race_nonwhite_overall)
overall_covars_alt %>% 
  filter(year==2019) %>% 
  ggplot(aes(x=race_nonwhite_overall,y=prop_race_non_w_bot_45_64))+
  geom_point()

men_covars_alt %>% 
  filter(year==2019) %>% 
  ggplot(aes(x=race_nonwhite_men,y=prop_race_non_w_men_45_64))+
  geom_point()

women_covars_alt %>% 
  filter(year==2019) %>% 
  ggplot(aes(x=race_nonwhite_women,y=prop_race_non_w_men_45_64))+
  geom_point()#close but not exact same

#non-white proportion - CA
overall_covars_alt %>% 
  filter(state_abb=="CA") %>% 
  ggplot(aes(y=prop_race_non_w_bot_45_64,x=year))+
  geom_line()
overall_covars_alt %>% 
  filter(state_abb=="CA") %>% 
  ggplot(aes(y=race_nonwhite_overall,x=year))+
  geom_line()

#non-white proportion - GA
overall_covars_alt %>% 
  filter(state_abb=="GA") %>% 
  ggplot(aes(y=prop_race_non_w_bot_45_64,x=year))+
  geom_line()
overall_covars_alt %>% 
  filter(state_abb=="GA") %>% 
  ggplot(aes(y=race_nonwhite_overall,x=year))+
  geom_line()


#All the exact same. Conclusion: use Nianogo version
## Proportion male, men v women pop------
summary(black_complete_covars_alt$prop_men_45_64_race_b)
summary(black_complete_covars_alt$male_black)
black_complete_covars_alt %>% 
  filter(year==2019) %>% 
  ggplot(aes(x=male_black,y=prop_men_45_64_race_b))+
  geom_point()
#Different. Conclusion: use ACS version

men_covars_alt %>% 
  filter(year==2019) %>% 
  ggplot(aes(x=population_men,y=pop_men_45_64))+
  geom_point()
#exact same. good.

women_covars_alt %>% 
  filter(year==2019) %>% 
  ggplot(aes(x=population_women,y=pop_wom_45_64))+
  geom_point()

#proportion male over time, BRFSS vs ACS, Californi
black_complete_covars_alt %>% 
  filter(state_abb=="CA") %>% 
  ggplot(aes(y=pop_men_45_64,x=year))+
  geom_line()
black_complete_covars_alt %>% 
  filter(state_abb=="CA") %>% 
  ggplot(aes(y=prop_men_45_64_race_b,x=year))+
  geom_line()
black_complete_covars_alt %>% 
  filter(state_abb=="CA") %>% 
  ggplot(aes(y=male_black,x=year))+
  geom_line()


men_covars_alt %>% 
  filter(state_abb=="CA") %>% 
  ggplot(aes(y=population_men,x=year))+
  geom_line()

#exact same. good.
## Low income / poverty-------
summary(black_complete_covars_alt$low_income_black)
summary(black_complete_covars_alt$prop_bot_45_64_race_b_pov)
black_complete_covars_alt %>% 
  filter(year==2019) %>% 
  ggplot(aes(x=low_income_black,y=prop_bot_45_64_race_b_pov))+
  geom_point()

hispanic_complete_covars_alt %>% 
  filter(year==2019) %>% 
  ggplot(aes(x=low_income_hispanic,y=prop_bot_45_64_race_b_pov))+
  geom_point()


overall_covars_alt %>% 
  filter(state_abb=="GA") %>% 
  ggplot(aes(y=prop_bot_45_64_pov,x=year))+
  geom_line()
overall_covars_alt %>% 
  filter(state_abb=="GA") %>% 
  ggplot(aes(y=low_income_overall,x=year))+
  geom_line()



## Education------
overall_covars_alt %>% 
  filter(year==2019) %>% 
  ggplot(aes(x=low_educ_overall,y=prop_bot_45_64_educ_lt_hs))+
  geom_point()
#very close but not identical. ACS prob better for overall
#but since no info in that age group in race categories, stick with
#BRFSS for consistency

#Is state_id the same thing as state_fips?
overall_covars_alt %>% 
  dplyr::select(state_fips,state_id) %>% 
  mutate(the_same=case_when(
    state_fips==state_id~1,
    TRUE ~0
  )) %>% 
  group_by(the_same) %>% 
  summarise(n=n())
#yes, always the same


## Compare covars with outcome----
black_complete_covars_alt %>% 
  filter(year==2019) %>% 
  ggplot(aes(x=male_black,y=cvd_death_rate))+
  geom_point()

names(black_complete_covars_alt)
black_complete_covars_alt %>% 
  filter(year==2019) %>% 
  ggplot(aes(x=vote_share_dem,y=cvd_death_rate))+
  geom_point()

black_complete_covars_alt %>% 
  filter(year==2019) %>% 
  ggplot(aes(x=married_black,y=cvd_death_rate))+
  geom_point()
black_complete_covars_alt %>% 
  filter(year==2019) %>% 
  ggplot(aes(x=low_educ_black,y=cvd_death_rate))+
  geom_point()
black_complete_covars_alt %>% 
  filter(year==2019) %>% 
  ggplot(aes(x=low_income_black,y=cvd_death_rate))+
  geom_point()
overall_covars_alt %>% 
  filter(year==2019) %>% 
  ggplot(aes(x=race_nonwhite_overall,y=cvd_death_rate))+
  geom_point()

# Assess distribution of race in included/excluded states----
#To inform potential selection bias
#In 2019, according to ACS, how many Black individuals in 
#states with missing vs non-missing states?
names(overall_covars_alt)
overall_covars_alt %>% 
  filter(year==2019) %>% 
  group_by(hispanic_miss) %>% 
  summarise(
    #pop in 45-64 age group
    pop_bot_45_64_race_h=sum(pop_bot_45_64_race_h,na.rm=T),
    pop_bot_45_64=sum(pop_bot_45_64,na.rm=T),
    
    #pop overall
    pop_tot=sum(pop_tot,na.rm=T),
    pop_race_h=sum(pop_race_h,na.rm=T)
  ) %>% 
  mutate(
    prop_race_h_bot_45_64=pop_bot_45_64_race_h/pop_bot_45_64,
    prop_race_h=pop_race_h/pop_tot
  )

overall_covars_alt %>% 
  filter(year==2019) %>% 
  group_by(black_miss) %>% 
  summarise(
    #pop in 45-64 age group
    pop_bot_45_64_race_b=sum(pop_bot_45_64_race_b,na.rm=T),
    pop_bot_45_64=sum(pop_bot_45_64,na.rm=T),
    
    #pop overall
    pop_tot=sum(pop_tot,na.rm=T),
    pop_race_b=sum(pop_race_b,na.rm=T)
  ) %>% 
  mutate(
    prop_race_b_bot_45_64=pop_bot_45_64_race_b/pop_bot_45_64,
    prop_race_b=pop_race_b/pop_tot
  )


#dist of black by treated or not
overall_covars_alt %>% 
  filter(year==2019) %>% 
  group_by(treated) %>% 
  summarise(
    #pop in 45-64 age group
    pop_bot_45_64_race_b=sum(pop_bot_45_64_race_b,na.rm=T),
    pop_bot_45_64=sum(pop_bot_45_64,na.rm=T),
    
    #pop overall
    pop_tot=sum(pop_tot,na.rm=T),
    pop_race_b=sum(pop_race_b,na.rm=T)
  ) %>% 
  mutate(
    prop_race_b_bot_45_64=pop_bot_45_64_race_b/pop_bot_45_64,
    prop_race_b=pop_race_b/pop_tot
  )
19131299/(19131299+22537668)




